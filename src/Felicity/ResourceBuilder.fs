module internal Felicity.ResourceBuilder

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open System.Threading.Tasks

let private emptyMetaDictNeverModify = Dictionary(0)
let emptyLinkArrayNeverModify = [||]


type ResourceBuilder<'ctx>(resourceModuleMap: Map<ResourceTypeName, Type>, baseUrl: string, currentIncludePath: RelationshipName list, linkCfg: LinkConfig<'ctx>, httpCtx, ctx: 'ctx, req: Request, resourceDef: ResourceDefinition<'ctx>, entity: obj) =

  let identifier = { ``type`` = resourceDef.TypeName; id = resourceDef.GetIdBoxed entity }

  // Note: Similar code exists in FieldUsageTracker
  let shouldUseField fieldName requiresExplicitInclude =
    match req.Fieldsets.TryGetValue resourceDef.TypeName with
    | false, _ -> not requiresExplicitInclude
    | true, fields -> fields.Contains fieldName

  // Note: Similar code exists in FieldUsageTracker
  let shouldIncludeRelationship relName =
    req.Includes |> List.exists (fun path ->
      path.Length > currentIncludePath.Length && path[currentIncludePath.Length] = relName
    )

  let resourceModule: Type =
    match resourceModuleMap.TryGetValue resourceDef.TypeName with
    | true, t -> t
    | false, _ -> failwith $"Framework bug: Attempted to build resource '%s{resourceDef.TypeName}', but no resource module was found"

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
                task {
                  let! constraints =
                    constrainedFields
                    |> Array.filter (fun f -> shouldUseField f.Name f.RequiresExplicitInclude)
                    |> Task.mapWhenAll (fun f ->
                        task {
                          let! constraints = f.BoxedGetConstraints ctx boxedEntity
                          return f.Name, constraints |> dict
                        }
                    )

                  return
                    constraints
                    |> Array.filter (fun (_, x) -> x.Count > 0)
                    |> Include
                    |> Skippable.filter (not << Array.isEmpty)
                    |> Skippable.map (dict >> box)
                }
            member _.RequiresExplicitInclude = false
        }

  member _.Identifier = identifier

  member _.Attributes () : Task<IDictionary<AttributeName, obj>> =
    ResourceModule.attributes<'ctx> resourceModule
    |> Array.append (constraintsAttr |> Option.toArray)
    |> Array.choose (fun a ->
        if shouldUseField a.Name a.RequiresExplicitInclude then
          a.BoxedGetSerialized |> Option.map (fun get -> get ctx entity |> Task.map (fun v -> a.Name, v))
        else None
    )
    |> Task.WhenAll
    |> Task.map (Array.choose (fun (n, v) -> v |> Skippable.toOption |> Option.map (fun v -> n, v)))
    |> Task.map dict

  member _.Relationships(onlyData) =
    task {
      let relationships = Dictionary<RelationshipName, IRelationship>()
      let builders = ResizeArray<ResourceBuilder<'ctx>>()

      let addRelationship relName rel =
        lock relationships (fun () -> relationships[relName] <- rel)

      let addBuilder builder =
        lock builders (fun () -> builders.Add(builder))

      let toOneRelsTask =
        ResourceModule.toOneRels<'ctx> resourceModule
        |> Task.mapWhenAllIgnore (fun r ->
            if not (r.SkipRelationship ctx entity) && (shouldUseField r.Name false || shouldIncludeRelationship r.Name) then
              task {
                let links : Skippable<IDictionary<_,_>> =
                  match selfUrlOpt with
                  | Some u when not onlyData && linkCfg.ShouldUseStandardLinks(httpCtx) && (r.SelfLink || r.RelatedLink) ->
                      let links = Dictionary()
                      if r.SelfLink then links["self"] <- { href = Some (u + "/relationships/" + r.Name); meta = Skip }
                      if r.RelatedLink then links["related"] <- { href = Some (u + "/" + r.Name); meta = Skip }
                      Include links
                  | _ -> Skip

                let meta = Skip  // support later when valid use-cases arrive

                match shouldIncludeRelationship r.Name, r.BoxedGetRelated with
                | true, Some get ->
                    match! get ctx entity with
                    | Skip ->
                        if shouldUseField r.Name false then
                          addRelationship r.Name { ToOne.links = links; data = Skip; meta = meta }
                    | Include (rDef, e) ->
                        let id = { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e }
                        if shouldUseField r.Name false then
                          addRelationship r.Name { ToOne.links = links; data = Include id; meta = meta }
                        addBuilder (ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], linkCfg, httpCtx, ctx, req, rDef, e))

                | true, None | false, Some _ | false, None ->
                    let! data = r.GetLinkageIfNotIncluded ctx entity
                    if shouldUseField r.Name false then
                      addRelationship r.Name { ToOne.links = links; data = data; meta = meta }
              }
              :> Task
            else Task.CompletedTask
        )

      let toOneNullableRelsTask =
        ResourceModule.toOneNullableRels<'ctx> resourceModule
        |> Task.mapWhenAllIgnore (fun r ->
            if  not (r.SkipRelationship ctx entity) && (shouldUseField r.Name false || shouldIncludeRelationship r.Name) then
              task {
                let links : Skippable<IDictionary<_,_>> =
                  match selfUrlOpt with
                  | Some u when not onlyData && linkCfg.ShouldUseStandardLinks(httpCtx) && (r.SelfLink || r.RelatedLink) ->
                      let links = Dictionary()
                      if r.SelfLink then links["self"] <- { href = Some (u + "/relationships/" + r.Name); meta = Skip }
                      if r.RelatedLink then links["related"] <- { href = Some (u + "/" + r.Name); meta = Skip }
                      Include links
                  | _ -> Skip

                let meta = Skip  // support later when valid use-cases arrive

                match shouldIncludeRelationship r.Name, r.BoxedGetRelated with
                | true, Some get ->
                    match! get ctx entity with
                    | Skip ->
                        if shouldUseField r.Name false then
                          addRelationship r.Name { ToOneNullable.links = links; data = Skip; meta = meta }
                    | Include None ->
                        if shouldUseField r.Name false then
                          addRelationship r.Name { ToOneNullable.links = links; data = Include None; meta = meta }
                    | Include (Some (rDef, e)) ->
                        let id = { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e }
                        if shouldUseField r.Name false then
                          addRelationship r.Name { ToOneNullable.links = links; data = Include (Some id); meta = meta }
                        addBuilder (ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], linkCfg, httpCtx, ctx, req, rDef, e))

                | true, None | false, Some _ | false, None ->
                    let! data = r.GetLinkageIfNotIncluded ctx entity
                    if shouldUseField r.Name false then
                      addRelationship r.Name { ToOneNullable.links = links; data = data; meta = meta }
              }
              :> Task
            else Task.CompletedTask
        )

      let toManyRelsTask =
        ResourceModule.toManyRels<'ctx> resourceModule
        |> Task.mapWhenAllIgnore (fun r ->
            if not (r.SkipRelationship ctx entity) && (shouldUseField r.Name false || shouldIncludeRelationship r.Name) then
              task {
                let links : Skippable<IDictionary<_,_>> =
                  match selfUrlOpt with
                  | Some u when not onlyData && linkCfg.ShouldUseStandardLinks(httpCtx) && (r.SelfLink || r.RelatedLink) ->
                      let links = Dictionary()
                      if r.SelfLink then links["self"] <- { href = Some (u + "/relationships/" + r.Name); meta = Skip }
                      if r.RelatedLink then links["related"] <- { href = Some (u + "/" + r.Name); meta = Skip }
                      Include links
                  | _ -> Skip

                let meta = Skip  // support later when valid use-cases arrive

                match shouldIncludeRelationship r.Name, r.BoxedGetRelated with
                | true, Some get ->
                    match! get ctx entity with
                    | Skip ->
                        if shouldUseField r.Name false then
                          addRelationship r.Name { ToMany.links = links; data = Skip; meta = meta }
                    | Include xs ->
                        let data = xs |> List.map (fun (rDef, e) -> { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e })
                        if shouldUseField r.Name false then
                          addRelationship r.Name { ToMany.links = links; data = Include data; meta = meta }
                        xs
                        |> List.iter (fun (rDef, e) ->
                            addBuilder (ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], linkCfg, httpCtx, ctx, req, rDef, e))
                        )

                | true, None | false, Some _ | false, None ->
                    let! data = r.GetLinkageIfNotIncluded ctx entity
                    if shouldUseField r.Name false then
                      addRelationship r.Name { ToMany.links = links; data = data; meta = meta }
              }
              :> Task
            else Task.CompletedTask
          )

      do! toOneRelsTask
      do! toOneNullableRelsTask
      do! toManyRelsTask

      return relationships, builders
    }

  member _.Links () : Task<Map<string, Link>> =
    task {
      let! opNamesHrefsAndMeta =
        if linkCfg.ShouldUseCustomLinks(httpCtx) then
          ResourceModule.customOps<'ctx> resourceModule
          |> Task.mapWhenAll (fun op ->
              task {
                let selfUrl = selfUrlOpt |> Option.defaultWith (fun () -> failwith $"Framework bug: Attempted to use self URL of resource type '%s{resourceDef.TypeName}' which has no collection name. This error should be caught at startup.")
                let! href, meta = op.HrefAndMeta ctx selfUrl entity
                return op.Name, href, meta
              }
          )
        else Task.result emptyLinkArrayNeverModify

      return
        (Map.empty, opNamesHrefsAndMeta)
        ||> Array.fold (fun links (name, href, meta) ->
              match href, meta with
              | None, None -> links
              | Some href, None -> links |> Links.addOpt name (Some href)
              | hrefOpt, Some meta -> links |> Links.addOptWithMeta name hrefOpt meta
        )
        |> match selfUrlOpt with
           | Some selfUrl when linkCfg.ShouldUseStandardLinks(httpCtx) -> Links.addOpt "self" (Some selfUrl)
           | _ -> id
    }

  member _.Meta () : Task<IDictionary<string, obj>> =
    Task.result emptyMetaDictNeverModify  // support later when valid use-cases arrive


/// Gets the resource's built relationship collections, merges them, and
/// returns a copy of the resource containing its relationship collection.
let setRelationships
    (getRelationships: ResourceIdentifier -> Dictionary<RelationshipName, IRelationship> voption) (res: Resource) =
  match res.id with
  | Include id ->
      match getRelationships { ``type`` = res.``type``; id = id } with
      | ValueNone -> ()
      | ValueSome rels -> res.relationships <- Include rels
  | _ -> ()


/// Builds a single resource, calls addRelationship with the identifier and
/// relationship collection, and returns the resource along with builders for
/// the included resources.
let internal buildAndGetRelatedBuilders (builder: ResourceBuilder<'ctx>) =
  task {
    let attrsTask = builder.Attributes ()
    let relsAndIncludedTask = builder.Relationships(false)
    let linksTask = builder.Links ()
    let metaTask = builder.Meta ()

    let! attrs = attrsTask
    let! rels, included = relsAndIncludedTask
    let! links = linksTask
    let! meta = metaTask

    let resource = {
      ``type`` = builder.Identifier.``type``
      id = Include builder.Identifier.id
      attributes = if attrs.Count = 0 then Skip else Include attrs
      links = if links.IsEmpty then Skip else Include links
      relationships = if rels.Count = 0 then Skip else Include rels
      meta = if meta.Count = 0 then Skip else Include meta
    }

    return resource, included
  }


let rec internal buildRecursive shouldBuildEntireResource addResource addRelationships (builder: ResourceBuilder<_>) =
  task {
    let recurse = buildRecursive shouldBuildEntireResource addResource addRelationships
    if shouldBuildEntireResource builder.Identifier then
      // We are building a new resource
      let! resource, relatedBuilders = buildAndGetRelatedBuilders builder
      addResource builder.Identifier resource
      do! Task.mapWhenAllIgnore recurse relatedBuilders
    else
      // We are building the relationships for a resource that has already been built
      let! rels, relatedBuilders = builder.Relationships(true)
      addRelationships builder.Identifier rels
      do! Task.mapWhenAllIgnore recurse relatedBuilders
  }

let internal includedResourceComparer =
  {
    new IComparer<Resource> with
      member _.Compare(x, y) =
        LanguagePrimitives.GenericComparer.Compare(struct (x.``type``, x.id), struct (y.``type``, y.id))
  }

let internal build (mainBuilders: ResourceBuilder<'ctx> list) =
  task {
    let allResources = Dictionary<ResourceIdentifier, Resource voption>()
    let additionalRelationships = Dictionary<ResourceIdentifier, IDictionary<RelationshipName, IRelationship>>()

    let shouldBuildEntireResource resId =
      lock allResources (fun () -> allResources.TryAdd(resId, ValueNone))

    let addResource resId res =
      lock allResources (fun () -> allResources.[resId] <- ValueSome res)

    let mergeRelationships (existingRels: IDictionary<RelationshipName, IRelationship>) (relsToAdd: IDictionary<RelationshipName, IRelationship>) =
      for kvp in relsToAdd do
        let relName = kvp.Key
        let rel = kvp.Value
        match existingRels.TryGetValue relName, rel with
        | (true, (:? ToOne as rOld)), (:? ToOne as rNew) -> rOld.data <- rOld.data |> Skippable.orElse rNew.data
        | (true, (:? ToOneNullable as rOld)), (:? ToOneNullable as rNew) -> rOld.data <- rOld.data |> Skippable.orElse rNew.data
        | (true, (:? ToMany as rOld)), (:? ToMany as rNew) -> rOld.data <- rOld.data |> Skippable.orElse rNew.data
        | (true, rOld), rNew -> failwith $"Framework bug: Attempted to merge different relationship types %s{rOld.GetType().Name} and %s{rNew.GetType().Name}"
        | (false, _), _ -> failwith "Framework bug: Relationships should never be included and empty"

    let addRelationships resId (relsToAdd: IDictionary<RelationshipName, IRelationship>) =
      if relsToAdd.Count > 0 then
        lock additionalRelationships (fun () ->
          match additionalRelationships.TryGetValue resId with
          | false, _ -> additionalRelationships[resId] <- relsToAdd
          | true, existingRels -> lock existingRels (fun () -> mergeRelationships existingRels relsToAdd)
        )

    let numMainBuilders = mainBuilders.Length

    do!
      mainBuilders
      |> Task.mapWhenAllIgnoreWithCount numMainBuilders (buildRecursive shouldBuildEntireResource addResource addRelationships)

    for kvp in additionalRelationships do
      let res = allResources[kvp.Key].Value
      match res.relationships with
      | Skip -> res.relationships <- Include kvp.Value
      | Include existingRels -> mergeRelationships existingRels kvp.Value

    let mainResources = Array.zeroCreate(numMainBuilders)

    mainBuilders |> List.iteri (fun i b ->
      let res = ref ValueNone
      if allResources.Remove(b.Identifier, res) then
        mainResources[i] <- res.Value.Value
      else
        failwith "Framework bug: Built resource not found in dict"
    )

    let includedResources = Array.zeroCreate(allResources.Count)

    allResources |> Seq.iteri (fun i kvp -> includedResources[i] <- kvp.Value.Value)

    Array.Sort(includedResources, includedResourceComparer)

    return mainResources, includedResources
  }

/// Builds the specified main resource and returns the built resource along
/// with any included resources. Included resources are deterministically
/// sorted (but the actual sorting is an implementation detail).
let internal buildOne (mainBuilder: ResourceBuilder<'ctx>) =
  task {
    let! main, included = build [mainBuilder]
    return main[0], included
  }
