module internal Felicity.ResourceBuilder

open System
open System.Text.Json.Serialization


type ResourceBuilder<'ctx>(resourceModuleMap: Map<ResourceTypeName, Type>, baseUrl: Uri, currentIncludePath: RelationshipName list, ctx: 'ctx, req: Request, resourceDef: ResourceDefinition<'ctx>, entity: obj) =

  let identifier = { ``type`` = resourceDef.TypeName; id = resourceDef.GetIdBoxed entity }
  let selfUrlOpt = resourceDef.CollectionName |> Option.map (fun collName -> baseUrl |> Uri.addSegments [collName; identifier.id])

  let shouldUseField fieldName =
    match req.Fieldsets.TryFind resourceDef.TypeName with
    | None -> true
    | Some fields -> fields.Contains fieldName

  let shouldIncludeRelationship relName =
    req.Includes |> List.exists (fun path ->
      path.Length >= currentIncludePath.Length + 1
      && path |> List.take (currentIncludePath.Length + 1) = currentIncludePath @ [relName]
    )

  let resourceModule: Type =
    resourceModuleMap
    |> Map.tryFind resourceDef.TypeName
    |> Option.defaultWith (fun () -> failwithf "Framework bug: Attempted to build resource '%s', but no resource module was found" resourceDef.TypeName)

  let constrainedFields = ResourceModule.constrainedFields<'ctx> resourceModule

  let constraintsAttr =
    if constrainedFields |> Array.forall (fun f -> not f.HasConstraints) then None
    else
      Some <|
        { new Attribute<'ctx> with
            member _.Name = "constraints"
            member _.BoxedGetSerialized =
              Some <| fun ctx boxedEntity ->
                async {
                  let! constraints =
                    constrainedFields
                    |> Array.filter (fun f -> shouldUseField f.Name)
                    |> Array.map (fun f ->
                        async {
                          let! constraints = f.BoxedGetConstraints ctx boxedEntity
                          return f.Name, constraints |> Map.ofList
                        }
                    )
                    |> Async.Parallel

                  return
                    constraints
                    |> Array.filter (fun (_, cs) -> not cs.IsEmpty)
                    |> Include
                    |> Skippable.filter (not << Array.isEmpty)
                    |> Skippable.map (Map.ofArray >> box)
                }
        }

  member _.Identifier = identifier

  member _.Attributes () : Async<Map<AttributeName, obj>> =
    ResourceModule.attributes<'ctx> resourceModule
    |> Array.append (constraintsAttr |> Option.toArray)
    |> Array.filter (fun a -> shouldUseField a.Name)
    |> Array.choose (fun a -> a.BoxedGetSerialized |> Option.map (fun get -> get ctx entity |> Async.map (fun v -> a.Name, v)))
    |> Async.Parallel
    |> Async.map (Array.choose (fun (n, v) -> v |> Skippable.toOption |> Option.map (fun v -> n, v)))
    |> Async.map Map.ofArray

  member _.Relationships () : Async<Map<RelationshipName, IRelationship> * ResourceBuilder<'ctx> list> =
    let toOneRels =
      ResourceModule.toOneRels<'ctx> resourceModule
      |> Array.filter (fun r -> shouldUseField r.Name || shouldIncludeRelationship r.Name)

    let toOneNullableRels =
      ResourceModule.toOneNullableRels<'ctx> resourceModule
      |> Array.filter (fun r -> shouldUseField r.Name || shouldIncludeRelationship r.Name)

    let toManyRels =
      ResourceModule.toManyRels<'ctx> resourceModule
      |> Array.filter (fun r -> shouldUseField r.Name || shouldIncludeRelationship r.Name)

    async {

      let! toOneRelsComp =
        toOneRels
        |> Array.map (fun r ->
            async {
              let links =
                Map.empty
                |> match selfUrlOpt with Some u when r.SelfLink -> Links.add "self" (u.AddSegments ["relationships"; r.Name]) | _ -> id
                |> match selfUrlOpt with Some u when r.RelatedLink -> Links.add "related" (u.AddSegment r.Name) | _ -> id
                |> Include
                // The links object must contain at least one link
                |> Skippable.filter (not << Map.isEmpty)

              let meta = Skip  // support later when valid use-cases arrive

              match shouldIncludeRelationship r.Name, r.BoxedGetRelated with
              | true, Some get ->
                  match! get ctx entity with
                  | Skip ->
                      return
                        r.Name,
                        { ToOne.links = links; data = Skip; meta = meta } :> IRelationship,
                        []
                  | Include (rDef, e) ->
                      let id = { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e }
                      return
                        r.Name,
                        { ToOne.links = links; data = Include id; meta = meta } :> IRelationship,
                        [ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], ctx, req, rDef, e)]

              | true, None | false, Some _ | false, None ->
                  return
                    r.Name,
                    { ToOne.links = links; data = Skip; meta = meta } :> IRelationship,
                    []
            }
        )
        |> Async.Parallel
        |> Async.StartChild

      let! toOneNullableRelsComp =
        toOneNullableRels
        |> Array.map (fun r ->
            async {
              let links =
                Map.empty
                |> match selfUrlOpt with Some u when r.SelfLink -> Links.add "self" (u.AddSegments ["relationships"; r.Name]) | _ -> id
                |> match selfUrlOpt with Some u when r.RelatedLink -> Links.add "related" (u.AddSegment r.Name) | _ -> id
                |> Include
                // The links object must contain at least one link
                |> Skippable.filter (not << Map.isEmpty)

              let meta = Skip  // support later when valid use-cases arrive

              match shouldIncludeRelationship r.Name, r.BoxedGetRelated with
              | true, Some get ->
                  match! get ctx entity with
                  | Skip ->
                      return
                        r.Name,
                        { ToOneNullable.links = links; data = Skip; meta = meta } :> IRelationship,
                        []
                  | Include None ->
                      return
                        r.Name,
                        { ToOneNullable.links = links; data = Include None; meta = meta } :> IRelationship,
                        []
                  | Include (Some (rDef, e)) ->
                      let id = { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e }
                      return
                        r.Name,
                        { ToOneNullable.links = links; data = Include (Some id); meta = meta } :> IRelationship,
                        [ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], ctx, req, rDef, e)]

              | true, None | false, Some _ | false, None ->
                  return
                    r.Name,
                    { ToOneNullable.links = links; data = Skip; meta = meta } :> IRelationship,
                    []
            }
        )
        |> Async.Parallel
        |> Async.StartChild

      let! toManyRelsComp =
        toManyRels
        |> Array.map (fun r ->
            async {
              let links =
                Map.empty
                |> match selfUrlOpt with Some u when r.SelfLink -> Links.add "self" (u.AddSegments ["relationships"; r.Name]) | _ -> id
                |> match selfUrlOpt with Some u when r.RelatedLink -> Links.add "related" (u.AddSegment r.Name) | _ -> id
                |> Include
                // The links object must contain at least one link
                |> Skippable.filter (not << Map.isEmpty)

              let meta = Skip  // support later when valid use-cases arrive

              match shouldIncludeRelationship r.Name, r.BoxedGetRelated with
              | true, Some get ->
                  match! get ctx entity with
                  | Skip ->
                      return
                        r.Name,
                        { ToMany.links = links; data = Skip; meta = meta } :> IRelationship,
                        []
                  | Include xs ->
                      let data = xs |> List.map (fun (rDef, e) -> { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e })
                      let builders = xs |> List.map (fun (rDef, e) -> ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], ctx, req, rDef, e))
                      return
                        r.Name,
                        { ToMany.links = links; data = Include data; meta = meta } :> IRelationship,
                        builders

              | true, None | false, Some _ | false, None ->
                  return
                    r.Name,
                    { ToMany.links = links; data = Skip; meta = meta } :> IRelationship,
                    []
            }
          )
          |> Async.Parallel
          |> Async.StartChild

      let! toOneRels = toOneRelsComp
      let! toOneNullableRels = toOneNullableRelsComp
      let! toManyRels = toManyRelsComp

      let all = Array.concat [toOneRels; toOneNullableRels; toManyRels]

      let relationships =
        all
        |> Array.choose (fun (name, rel, _) -> if shouldUseField name then Some (name, rel) else None)
        |> Map.ofArray

      let builders = all |> Array.toList |> List.collect (fun (_, _, builder) -> builder)

      return relationships, builders
    }

  member _.Links () : Async<Map<string, Link>> =
    async {
      let! opNamesHrefsAndMeta =
        ResourceModule.customOps<'ctx> resourceModule
        |> Array.map (fun op ->
            async {
              let selfUrl = selfUrlOpt |> Option.defaultWith (fun () -> failwithf "Framework bug: Attempted to use self URL of resource type '%s' which has no collection name. This error should be caught at startup." resourceDef.TypeName)
              let! href, meta = op.HrefAndMeta ctx selfUrl entity
              return op.Name, href, meta
            }
        )
        |> Async.Parallel

      return
        (Map.empty, opNamesHrefsAndMeta)
        ||> Array.fold (fun links (name, href, meta) ->
              match href, meta with
              | None, None -> links
              | Some href, None -> links |> Links.addOpt name (Some href)
              | hrefOpt, Some meta -> links |> Links.addOptWithMeta name hrefOpt meta
        )
        |> match selfUrlOpt with
           | Some selfUrl when ResourceModule.hasGetResourceOperation<'ctx> resourceModule ->
              Links.addOpt "self" (Some selfUrl)
           | _ -> id
    }

  member _.Meta () : Async<Map<string, obj>> =
    async.Return Map.empty  // support later when valid use-cases arrive


open System.Collections.Concurrent

/// Gets the resource's built relationship collections, merges them, and
/// returns a copy of the resource containing its relationship collection.
let setRelationships 
    (getRelationships: ResourceIdentifier -> Map<string, IRelationship>) (res: Resource)
    : Resource =
  match res.id with
  | Include id ->
      let rels = getRelationships { ``type`` = res.``type``; id = id }
      { res with relationships = if rels = Map.empty then Skip else Include rels }
  | _ -> res


/// Builds a single resource, calls addRelationship with the identifier and
/// relationship collection, and returns the resource along with builders for
/// the included resources.
let internal buildAndGetRelated
    addRelationships (builder: ResourceBuilder<'ctx>)
    : Async<Resource * ResourceBuilder<'ctx> list> =
  async {
    let! attrsComp = builder.Attributes () |> Async.StartChild
    let! relsAndIncludedComp = builder.Relationships () |> Async.StartChild
    let! linksComp = builder.Links () |> Async.StartChild
    let! metaComp = builder.Meta () |> Async.StartChild

    let! attrs = attrsComp
    let! rels, included = relsAndIncludedComp
    let! links = linksComp
    let! meta = metaComp

    addRelationships builder.Identifier rels

    let resource = {
      ``type`` = builder.Identifier.``type``
      id = Include builder.Identifier.id
      attributes = if attrs.IsEmpty then Skip else Include attrs
      links = if links.IsEmpty then Skip else Include links
      relationships = Skip  // We add all relationships at the end of the process
      meta = if meta.IsEmpty then Skip else Include meta
    }

    return resource, included
  }

/// Calls addRelationship with the identifier and relationship collection, and
/// returns the builders for the included resources.
let internal getRelated 
    addRelationship (builder: ResourceBuilder<'ctx>)
    : Async<ResourceBuilder<'ctx> list> =
  async {
    let! rels, included = builder.Relationships ()
    addRelationship builder.Identifier rels
    return included
  }

/// Builds a single resource and all included resources. If isMain is true,
/// calls addMainResource with the built resource, otherwise calls
/// addIncludedResource with the built resource (unless it's already built).
let rec internal buildRecursive 
    initRelationships addRelationships addMainResource addIncludedResource
    isMain (builder: ResourceBuilder<'ctx>)
    : Async<unit> =
  async {
    let recurse = buildRecursive initRelationships addRelationships addMainResource addIncludedResource false
    let wasInitiated = initRelationships builder.Identifier
    if isMain then
      // We are building a main resource
      let! mainResource, relatedBuilders = buildAndGetRelated addRelationships builder
      addMainResource mainResource
      do! relatedBuilders |> List.map recurse |> Async.Parallel |> Async.map ignoreUnitArray
    elif wasInitiated then
      // We are building an included resource that has not been built yet
      let! includedResource, relatedBuilders = buildAndGetRelated addRelationships builder
      addIncludedResource includedResource
      do! relatedBuilders |> List.map recurse |> Async.Parallel |> Async.map ignoreUnitArray
    else
      // We are building a resource that has already been built
      let! relatedBuilders = getRelated addRelationships builder
      do! relatedBuilders |> List.map recurse |> Async.Parallel |> Async.map ignoreUnitArray
  }

/// Builds the specified main resources and returns the built resources along
/// with any included resources. Included resources are deterministically
/// sorted (but the actual sorting is an implementation detail).
let internal build (mainBuilders: ResourceBuilder<'ctx> list)
    : Async<Resource list * Resource list> =

  /// We only build each resource once, but different builders for the same
  /// resource may have different relationships included, so we add all
  /// relationships to this dictionary when building, and merge them and add
  /// them to the resources later.
  ///
  /// A ResourceIdentifier being present in this dict indicates that it is
  /// already being built and should not be built again (except the
  /// relationships and included resources).
  let relationships = ConcurrentDictionary<ResourceIdentifier, Map<string, IRelationship>>()

  /// A container for all the main resources
  let mainResources : Resource [] = Array.zeroCreate(mainBuilders.Length)

  /// A container for all the included resources
  let includedResources = ConcurrentBag()


  // Helper functions

  let initRelationships identifier = 
    relationships.TryAdd (identifier, Map.empty)

  let addRelationships identifier newRels =
    relationships.AddOrUpdate(
      identifier,
      (fun _ -> failwith "Framework bug: Resource identifier not found in relationships dict during update."),
      fun _ existingRels -> (existingRels, newRels) ||> Map.fold (fun (allRels: Map<string, IRelationship>) name newRel ->
        match allRels.TryGetValue name, newRel with
        | (false, _), _ -> allRels |> Map.add name newRel
        | (true, (:? ToOne as rOld)), (:? ToOne as rNew) ->
            allRels |> Map.add name (upcast { rOld with data = rOld.data |> Skippable.orElse rNew.data })
        | (true, (:? ToOneNullable as rOld)), (:? ToOneNullable as rNew) ->
            allRels |> Map.add name (upcast { rOld with data = rOld.data |> Skippable.orElse rNew.data })
        | (true, (:? ToMany as rOld)), (:? ToMany as rNew) ->
            allRels |> Map.add name (upcast { rOld with data = rOld.data |> Skippable.orElse rNew.data })
        | (true, rOld), rNew -> failwithf "Framework bug: Attempted to merge different relationship types %s and %s" (rOld.GetType().Name) (rNew.GetType().Name)
      ))
    |> ignore

  let addMainResource idx res =
    mainResources.[idx] <- res

  let addIncludedResource res =
    includedResources.Add res

  let getRelationships identifier =
    match relationships.TryGetValue identifier with
    | false, _ -> Map.empty
    | true, rels -> rels

  // Do the actual work

  async {
    do!
      mainBuilders
      |> List.mapi (fun i b -> b |> buildRecursive initRelationships addRelationships (addMainResource i) addIncludedResource true)
      |> Async.Parallel
      |> Async.map ignoreUnitArray

    return
      mainResources
      |> Array.map (setRelationships getRelationships)
      |> Array.toList,

      includedResources.ToArray()
      |> Array.map (setRelationships getRelationships)
      // Included resource order should be deterministic in order to support
      // hashing the response body for ETag
      |> Array.sortBy (fun r -> r.``type``, r.id)
      |> Array.toList
  }

/// Builds the specified main resource and returns the built resource along
/// with any included resources. Included resources are deterministically
/// sorted (but the actual sorting is an implementation detail).
let internal buildOne (mainBuilder: ResourceBuilder<'ctx>)
    : Async<Resource * Resource list> =
  async {
    let! main, included = build [mainBuilder]
    return main.Head, included
  }
