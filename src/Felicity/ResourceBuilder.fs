module internal Felicity.ResourceBuilder

open System
open System.Text.Json.Serialization
open Hopac


type ResourceBuilder<'ctx>(resourceModuleMap: Map<ResourceTypeName, Type>, baseUrl: string, currentIncludePath: RelationshipName list, ctx: 'ctx, req: Request, resourceDef: ResourceDefinition<'ctx>, entity: obj) =

  let identifier = { ``type`` = resourceDef.TypeName; id = resourceDef.GetIdBoxed entity }

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

  let selfUrlOpt =
    resourceDef.CollectionName
    |> Option.map (fun collName -> baseUrl + "/" + collName + "/" + identifier.id)
    |> Option.filter (fun _ -> ResourceModule.hasGetResourceOperation<'ctx> resourceModule)

  let constrainedFields = ResourceModule.constrainedFields<'ctx> resourceModule

  let constraintsAttr =
    if constrainedFields |> Array.forall (fun f -> not f.HasConstraints) then None
    else
      Some <|
        { new Attribute<'ctx> with
            member _.Name = "constraints"
            member _.BoxedGetSerialized =
              Some <| fun ctx boxedEntity ->
                job {
                  let! constraints =
                    constrainedFields
                    |> Array.filter (fun f -> shouldUseField f.Name)
                    |> Array.map (fun f ->
                        job {
                          let! constraints = f.BoxedGetConstraints ctx boxedEntity
                          return f.Name, constraints |> Map.ofList
                        }
                    )
                    |> Job.conCollect

                  return
                    constraints
                    |> Seq.filter (fun (_, cs) -> not cs.IsEmpty)
                    |> Seq.toArray
                    |> Include
                    |> Skippable.filter (not << Array.isEmpty)
                    |> Skippable.map (Map.ofArray >> box)
                }
        }

  member _.Identifier = identifier

  member _.Attributes () : Job<Map<AttributeName, obj>> =
    ResourceModule.attributes<'ctx> resourceModule
    |> Array.append (constraintsAttr |> Option.toArray)
    |> Array.filter (fun a -> shouldUseField a.Name)
    |> Array.choose (fun a -> a.BoxedGetSerialized |> Option.map (fun get -> get ctx entity |> Job.map (fun v -> a.Name, v)))
    |> Job.conCollect
    |> Job.map (Seq.choose (fun (n, v) -> v |> Skippable.toOption |> Option.map (fun v -> n, v)))
    |> Job.map Map.ofSeq

  member _.Relationships () : Job<Map<RelationshipName, IRelationship> * ResourceBuilder<'ctx> list> =
    let toOneRels =
      ResourceModule.toOneRels<'ctx> resourceModule
      |> Array.filter (fun r -> shouldUseField r.Name || shouldIncludeRelationship r.Name)

    let toOneNullableRels =
      ResourceModule.toOneNullableRels<'ctx> resourceModule
      |> Array.filter (fun r -> shouldUseField r.Name || shouldIncludeRelationship r.Name)

    let toManyRels =
      ResourceModule.toManyRels<'ctx> resourceModule
      |> Array.filter (fun r -> shouldUseField r.Name || shouldIncludeRelationship r.Name)

    job {

      let! toOneRelsPromise =
        toOneRels
        |> Array.map (fun r ->
            job {
              let links =
                Map.empty
                |> match selfUrlOpt with Some u when r.SelfLink -> Links.add "self" (u + "/relationships/" + r.Name) | _ -> id
                |> match selfUrlOpt with Some u when r.RelatedLink -> Links.add "related" (u + "/" + r.Name) | _ -> id
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
                  let! data = r.GetLinkageIfNotIncluded ctx entity
                  return
                    r.Name,
                    { ToOne.links = links; data = data; meta = meta } :> IRelationship,
                    []
            }
        )
        |> Job.conCollect
        |> Promise.start

      let! toOneNullableRelsPromise =
        toOneNullableRels
        |> Array.map (fun r ->
            job {
              let links =
                Map.empty
                |> match selfUrlOpt with Some u when r.SelfLink -> Links.add "self" (u + "/relationships/" + r.Name) | _ -> id
                |> match selfUrlOpt with Some u when r.RelatedLink -> Links.add "related" (u + "/" + r.Name) | _ -> id
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
                  let! data = r.GetLinkageIfNotIncluded ctx entity
                  return
                    r.Name,
                    { ToOneNullable.links = links; data = data; meta = meta } :> IRelationship,
                    []
            }
        )
        |> Job.conCollect
        |> Promise.start

      let! toManyRelsPromise =
        toManyRels
        |> Array.map (fun r ->
            job {
              let links =
                Map.empty
                |> match selfUrlOpt with Some u when r.SelfLink -> Links.add "self" (u + "/relationships/" + r.Name) | _ -> id
                |> match selfUrlOpt with Some u when r.RelatedLink -> Links.add "related" (u + "/" + r.Name) | _ -> id
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
                  let! data = r.GetLinkageIfNotIncluded ctx entity
                  return
                    r.Name,
                    { ToMany.links = links; data = data; meta = meta } :> IRelationship,
                    []
            }
          )
          |> Job.conCollect
          |> Promise.start

      let! toOneRels = toOneRelsPromise
      let! toOneNullableRels = toOneNullableRelsPromise
      let! toManyRels = toManyRelsPromise

      let all = Seq.concat [toOneRels; toOneNullableRels; toManyRels] |> Seq.cache

      let relationships =
        all
        |> Seq.choose (fun (name, rel, _) -> if shouldUseField name then Some (name, rel) else None)
        |> Map.ofSeq

      let builders = all |> Seq.toList |> List.collect (fun (_, _, builder) -> builder)

      return relationships, builders
    }

  member _.Links () : Job<Map<string, Link>> =
    job {
      let! opNamesHrefsAndMeta =
        ResourceModule.customOps<'ctx> resourceModule
        |> Array.map (fun op ->
            job {
              let selfUrl = selfUrlOpt |> Option.defaultWith (fun () -> failwithf "Framework bug: Attempted to use self URL of resource type '%s' which has no collection name. This error should be caught at startup." resourceDef.TypeName)
              let! href, meta = op.HrefAndMeta ctx selfUrl entity
              return op.Name, href, meta
            }
        )
        |> Job.conCollect

      return
        (Map.empty, opNamesHrefsAndMeta)
        ||> Seq.fold (fun links (name, href, meta) ->
              match href, meta with
              | None, None -> links
              | Some href, None -> links |> Links.addOpt name (Some href)
              | hrefOpt, Some meta -> links |> Links.addOptWithMeta name hrefOpt meta
        )
        |> match selfUrlOpt with
           | Some selfUrl -> Links.addOpt "self" (Some selfUrl)
           | _ -> id
    }

  member _.Meta () : Job<Map<string, obj>> =
    Job.result Map.empty  // support later when valid use-cases arrive


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
    : Job<Resource * ResourceBuilder<'ctx> list> =
  job {
    let! attrsPromise = builder.Attributes () |> Promise.start
    let! relsAndIncludedPromise = builder.Relationships () |> Promise.start
    let! linksPromise = builder.Links () |> Promise.start
    let! metaPromise = builder.Meta () |> Promise.start

    let! attrs = attrsPromise
    let! rels, included = relsAndIncludedPromise
    let! links = linksPromise
    let! meta = metaPromise

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
    : Job<ResourceBuilder<'ctx> list> =
  job {
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
    : Job<unit> =
  job {
    let recurse = buildRecursive initRelationships addRelationships addMainResource addIncludedResource false
    let wasInitiated = initRelationships builder.Identifier
    if isMain then
      // We are building a main resource
      let! mainResource, relatedBuilders = buildAndGetRelated addRelationships builder
      addMainResource mainResource
      do! relatedBuilders |> List.map recurse |> Job.conCollect |> Job.map ignore<ResizeArray<unit>>
    elif wasInitiated then
      // We are building an included resource that has not been built yet
      let! includedResource, relatedBuilders = buildAndGetRelated addRelationships builder
      addIncludedResource includedResource
      do! relatedBuilders |> List.map recurse |> Job.conCollect |> Job.map ignore<ResizeArray<unit>>
    else
      // We are building a resource that has already been built
      let! relatedBuilders = getRelated addRelationships builder
      do! relatedBuilders |> List.map recurse |> Job.conCollect |> Job.map ignore<ResizeArray<unit>>
  }

/// Builds the specified main resources and returns the built resources along
/// with any included resources. Included resources are deterministically
/// sorted (but the actual sorting is an implementation detail).
let internal build (mainBuilders: ResourceBuilder<'ctx> list)
    : Job<Resource list * Resource list> =

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

  job {
    do!
      mainBuilders
      |> List.mapi (fun i b -> b |> buildRecursive initRelationships addRelationships (addMainResource i) addIncludedResource true)
      |> Job.conCollect
      |> Job.map ignore<ResizeArray<unit>>

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
    : Job<Resource * Resource list> =
  job {
    let! main, included = build [mainBuilder]
    return main.Head, included
  }
