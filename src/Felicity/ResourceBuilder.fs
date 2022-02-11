module internal Felicity.ResourceBuilder

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open Hopac
open Hopac.Extensions

let private emptyMetaDictNeverModify = Dictionary(0)
let emptyLinkArrayNeverModify = ResizeArray(0)


type ResourceBuilder<'ctx>(resourceModuleMap: Map<ResourceTypeName, Type>, baseUrl: string, currentIncludePath: RelationshipName list, linkCfg: LinkConfig<'ctx>, httpCtx, ctx: 'ctx, req: Request, resourceDef: ResourceDefinition<'ctx>, entity: obj) =

  let identifier = { ``type`` = resourceDef.TypeName; id = resourceDef.GetIdBoxed entity }

  let shouldUseField fieldName =
    match req.Fieldsets.TryGetValue resourceDef.TypeName with
    | false, _ -> true
    | true, fields -> fields.Contains fieldName

  let shouldIncludeRelationship relName =
    req.Includes |> List.exists (fun path ->
      path.Length >= currentIncludePath.Length + 1
      && path |> List.take (currentIncludePath.Length + 1) = currentIncludePath @ [relName]
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
                job {
                  let! constraints =
                    constrainedFields
                    |> Seq.filter (fun f -> shouldUseField f.Name)
                    |> Seq.Con.mapJob (fun f ->
                        job {
                          let! constraints = f.BoxedGetConstraints ctx boxedEntity
                          return f.Name, constraints |> dict
                        }
                    )

                  return
                    constraints
                    |> Seq.filter (fun (_, x) -> x.Count > 0)
                    |> Seq.toArray
                    |> Include
                    |> Skippable.filter (not << Array.isEmpty)
                    |> Skippable.map (dict >> box)
                }
        }

  member _.Identifier = identifier

  member _.Attributes () : Job<IDictionary<AttributeName, obj>> =
    ResourceModule.attributes<'ctx> resourceModule
    |> Array.append (constraintsAttr |> Option.toArray)
    |> Array.filter (fun a -> shouldUseField a.Name)
    |> Array.choose (fun a -> a.BoxedGetSerialized |> Option.map (fun get -> get ctx entity |> Job.map (fun v -> a.Name, v)))
    |> Job.conCollect
    |> Job.map (Seq.choose (fun (n, v) -> v |> Skippable.toOption |> Option.map (fun v -> n, v)))
    |> Job.map dict

  member _.Relationships(onlyData) =
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

      let relationships = Dictionary<RelationshipName, IRelationship>()
      let builders = ResizeArray<ResourceBuilder<'ctx>>()

      let addRelationship relName rel =
        lock relationships (fun () -> relationships[relName] <- rel)

      let addBuilder builder =
        lock builders (fun () -> builders.Add(builder))

      let! toOneRelsPromise =
        toOneRels
        |> Seq.Con.iterJob (fun r ->
            job {
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
                      if shouldUseField r.Name then
                        addRelationship r.Name { ToOne.links = links; data = Skip; meta = meta }
                  | Include (rDef, e) ->
                      let id = { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e }
                      if shouldUseField r.Name then
                        addRelationship r.Name { ToOne.links = links; data = Include id; meta = meta }
                      addBuilder (ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], linkCfg, httpCtx, ctx, req, rDef, e))

              | true, None | false, Some _ | false, None ->
                  let! data = r.GetLinkageIfNotIncluded ctx entity
                  if shouldUseField r.Name then
                    addRelationship r.Name { ToOne.links = links; data = data; meta = meta }
            }
        )
        |> Promise.start

      let! toOneNullableRelsPromise =
        toOneNullableRels
        |> Seq.Con.iterJob (fun r ->
            job {
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
                      if shouldUseField r.Name then
                        addRelationship r.Name { ToOneNullable.links = links; data = Skip; meta = meta }
                  | Include None ->
                      if shouldUseField r.Name then
                        addRelationship r.Name { ToOneNullable.links = links; data = Include None; meta = meta }
                  | Include (Some (rDef, e)) ->
                      let id = { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e }
                      if shouldUseField r.Name then
                        addRelationship r.Name { ToOneNullable.links = links; data = Include (Some id); meta = meta }
                      addBuilder (ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], linkCfg, httpCtx, ctx, req, rDef, e))

              | true, None | false, Some _ | false, None ->
                  let! data = r.GetLinkageIfNotIncluded ctx entity
                  if shouldUseField r.Name then
                    addRelationship r.Name { ToOneNullable.links = links; data = data; meta = meta }
            }
        )
        |> Promise.start

      let! toManyRelsPromise =
        toManyRels
        |> Seq.Con.iterJob (fun r ->
            job {
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
                      if shouldUseField r.Name then
                        addRelationship r.Name { ToMany.links = links; data = Skip; meta = meta }
                  | Include xs ->
                      let data = xs |> List.map (fun (rDef, e) -> { ``type`` = rDef.TypeName; id = rDef.GetIdBoxed e })
                      if shouldUseField r.Name then
                        addRelationship r.Name { ToMany.links = links; data = Include data; meta = meta }
                      for rDef, e in xs do
                        addBuilder (ResourceBuilder<'ctx>(resourceModuleMap, baseUrl, currentIncludePath @ [r.Name], linkCfg, httpCtx, ctx, req, rDef, e))

              | true, None | false, Some _ | false, None ->
                  let! data = r.GetLinkageIfNotIncluded ctx entity
                  if shouldUseField r.Name then
                    addRelationship r.Name { ToMany.links = links; data = data; meta = meta }
            }
          )
          |> Promise.start

      do! toOneRelsPromise
      do! toOneNullableRelsPromise
      do! toManyRelsPromise

      return relationships, builders
    }

  member _.Links () : Job<Map<string, Link>> =
    job {
      let! opNamesHrefsAndMeta =
        if linkCfg.ShouldUseCustomLinks(httpCtx) then
          ResourceModule.customOps<'ctx> resourceModule
          |> Seq.Con.mapJob (fun op ->
              job {
                let selfUrl = selfUrlOpt |> Option.defaultWith (fun () -> failwith $"Framework bug: Attempted to use self URL of resource type '%s{resourceDef.TypeName}' which has no collection name. This error should be caught at startup.")
                let! href, meta = op.HrefAndMeta ctx selfUrl entity
                return op.Name, href, meta
              }
          )
        else Job.result emptyLinkArrayNeverModify

      return
        (Map.empty, opNamesHrefsAndMeta)
        ||> Seq.fold (fun links (name, href, meta) ->
              match href, meta with
              | None, None -> links
              | Some href, None -> links |> Links.addOpt name (Some href)
              | hrefOpt, Some meta -> links |> Links.addOptWithMeta name hrefOpt meta
        )
        |> match selfUrlOpt with
           | Some selfUrl when linkCfg.ShouldUseStandardLinks(httpCtx) -> Links.addOpt "self" (Some selfUrl)
           | _ -> id
    }

  member _.Meta () : Job<IDictionary<string, obj>> =
    Job.result emptyMetaDictNeverModify  // support later when valid use-cases arrive


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
  job {
    let! attrsPromise = builder.Attributes () |> Promise.start
    let! relsAndIncludedPromise = builder.Relationships(false) |> Promise.start
    let! linksPromise = builder.Links () |> Promise.start
    let! metaPromise = builder.Meta () |> Promise.start

    let! attrs = attrsPromise
    let! rels, included = relsAndIncludedPromise
    let! links = linksPromise
    let! meta = metaPromise

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
  job {
    let recurse = buildRecursive shouldBuildEntireResource addResource addRelationships
    if shouldBuildEntireResource builder.Identifier then
      // We are building a new resource
      let! resource, relatedBuilders = buildAndGetRelatedBuilders builder
      addResource builder.Identifier resource
      do! relatedBuilders |> Seq.Con.iterJob recurse
    else
      // We are building the relationships for a resource that has already been built
      let! rels, relatedBuilders = builder.Relationships(true)
      addRelationships builder.Identifier rels
      do! relatedBuilders |> Seq.Con.iterJob recurse
  }

let internal includedResourceComparer =
  {
    new IComparer<Resource> with
      member _.Compare(x, y) =
        LanguagePrimitives.GenericComparer.Compare(struct (x.``type``, x.id), struct (y.``type``, y.id))
  }

let internal build (mainBuilders: ResourceBuilder<'ctx> list) =
  job {
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

    do!
      mainBuilders |> Seq.Con.iterJob (buildRecursive shouldBuildEntireResource addResource addRelationships)

    for kvp in additionalRelationships do
      let res = allResources[kvp.Key].Value
      match res.relationships with
      | Skip -> res.relationships <- Include kvp.Value
      | Include existingRels -> mergeRelationships existingRels kvp.Value

    let mainResources = Array.zeroCreate(mainBuilders.Length)

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
  job {
    let! main, included = build [mainBuilder]
    return main[0], included
  }
