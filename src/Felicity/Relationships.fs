namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Hopac
open Giraffe
open Errors



type RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId> =
  abstract Name: string
  abstract IdToDomain: 'ctx -> string -> Job<Result<'relatedId, Error list>>


type internal RelationshipHandlers<'ctx> =
  abstract Name: string
  abstract IsToMany: bool
  abstract IsSettableButNotGettable: bool
  abstract IsSettableWithoutPersist: bool
  abstract GetRelated: ('ctx -> Request -> BoxedEntity -> ResponseBuilder<'ctx> -> HttpHandler) option
  abstract GetSelf: ('ctx -> Request -> BoxedEntity -> HttpHandler) option
  abstract PostSelf: ('ctx -> Request -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract PatchSelf: ('ctx -> Request -> ParentResourceTypeName -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract DeleteSelf: ('ctx -> Request -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option


type internal ToOneRelationship<'ctx> =
  abstract Name: RelationshipName
  abstract SelfLink: bool
  abstract RelatedLink: bool
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Job<Skippable<ResourceDefinition<'ctx> * BoxedEntity>>) option


type ToOneRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  idGetter: RequestGetter<'ctx, 'relatedId option>
  getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>
} with

  static member internal Create(name, idGetter: RequestGetter<'ctx, 'relatedId option>, getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) : ToOneRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      idGetter = idGetter
      getRelated = getRelated
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          this.idGetter.Get(ctx, req, includedTypeAndId)
          |> JobResult.bind (
              Option.traverseJobResult (
                this.getRelated.GetById ctx
                >> JobResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                >> JobResult.requireSome [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")]
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]


type ToOneRelationshipIncludedGetter<'ctx, 'relatedEntity> = internal {
  name: string
  getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>
  allowedTypes: ResourceTypeName list
} with

  static member internal Create(name, getParser, allowedTypes) : ToOneRelationshipIncludedGetter<'ctx, 'relatedEntity> =
    {
      name = name
      getParser = getParser
      allowedTypes = allowedTypes
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOne as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Job.result
                  | { data = Include identifier } when not (this.allowedTypes |> List.contains identifier.``type``) ->
                      let pointer = relsPointer + "/" + this.name + "/data/type"
                      Error [relInvalidType this.name identifier.``type`` this.allowedTypes pointer] |> Job.result
                  | { data = Include identifier } ->
                      RequestParserHelper<'ctx>(ctx, req, (identifier.``type``, identifier.id))
                      |> this.getParser
                      |> fun p -> p.ParseJob()
                      |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOne>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result

    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]


type ToOneRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Job<Result<'relatedId, Error list>>> option
  get: ('ctx -> 'entity -> Job<'relatedEntity Skippable>) option
  set: ('ctx -> Pointer -> 'relatedId -> 'entity -> Job<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>
  beforeModifySelf: 'ctx -> 'entity -> Job<Result<'entity, Error list>>
  afterModifySelf: ('ctx -> 'entity -> 'entity -> Job<Result<'entity, Error list>>) option
  modifyGetRelatedResponse: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler
  modifyGetSelfResponse: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler
  modifyPatchSelfOkResponse: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler
  modifyPatchSelfAcceptedResponse: 'ctx -> 'entity -> HttpHandler
  patchSelfReturn202Accepted: bool
} with

  static member internal Create (name: string, resolveEntity, idParsers) : ToOneRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      resolveEntity = resolveEntity
      idParsers = idParsers
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> Job.result []
      beforeModifySelf = fun _ e -> Ok e |> Job.result
      afterModifySelf = None
      modifyGetRelatedResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyGetSelfResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      patchSelfReturn202Accepted = false
    }

  member private _.toIdSetter (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) entitySetter =
    fun ctx (dataPointer: Pointer) relatedId entity ->
      getRelated.GetById ctx relatedId
      |> JobResult.mapError (List.map (Error.setSourcePointer dataPointer))
      |> JobResult.requireSome [relatedResourceNotFound dataPointer]
      |> JobResult.bind (fun r ->
          entitySetter ctx r entity
          |> JobResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwithf "Attempted to parse resource ID for polymorphic relationship '%s', but no ID parsers have been specified." this.name)
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOne as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Job.result
                  | { data = Include identifier } ->
                      match idParsers.TryGetValue identifier.``type`` with
                      | false, _ ->
                          let allowedTypes = idParsers |> Map.toList |> List.map fst
                          let pointer = relsPointer + "/" + this.name + "/data/type"
                          Error [relInvalidType this.name identifier.``type`` allowedTypes pointer] |> Job.result
                      | true, parseId ->
                          parseId ctx identifier.id
                          // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                          // this just means that the resource does not exist, which is a more helpful result.
                          |> JobResult.mapError (fun _ -> [relatedResourceNotFound (relsPointer + "/" + this.name + "/data")])
                          |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOne>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result
    }


  interface OptionalRequestGetter<'ctx, 'relatedId> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedId> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          let pointer = Request.pointerForMissingRel includedTypeAndId req + "/" + this.name
          [reqParserProhibitedRel this.name pointer]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      job {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { relationships = Include rels } }) ->
            match this.set, rels.TryFind this.name with
            | _, None -> return Ok entity // not provided in request
            | None, Some _ -> return Error [setRelReadOnly this.name ("/data/relationships/" + this.name)]
            | Some set, Some (:? ToOne as rel) ->
                let idParsers =
                  this.idParsers
                  |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
                match rel.data with
                | Skip -> return Error [relMissingData this.name ("/data/relationships/" + this.name)]
                | Include identifier ->
                    match idParsers.TryGetValue identifier.``type`` with
                    | false, _ ->
                        let allowedTypes = idParsers |> Map.toList |> List.map fst
                        let pointer = "/data/relationships/" + this.name + "/data/type"
                        return Error [relInvalidType this.name identifier.``type`` allowedTypes pointer]
                    | true, parseId ->
                        return!
                          parseId ctx identifier.id
                          // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                          // this just means that the resource does not exist, which is a more helpful result.
                          |> JobResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                          |> JobResult.bind (fun domain ->
                              set ctx ("/data/relationships/" + this.name + "/data") domain (unbox<'entity> entity))
                          |> JobResult.map box<'entity>
            | Some _, Some rel -> return failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOne>.FullName (rel.GetType().FullName)
        | _ -> return Ok entity  // no relationships provided
      }


  interface ToOneRelationship<'ctx> with
    member this.Name = this.name
    member this.SelfLink = this.get.IsSome
    member this.RelatedLink = this.get.IsSome
    member this.BoxedGetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx entity ->
          getRelated ctx (unbox<'entity> entity) |> Job.map (Skippable.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          ))
      )

  
  member this.Related (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) =
    ToOneRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(this.name, this.Optional, getRelated)


  member this.Included (getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>) =
    ToOneRelationshipIncludedGetter.Create(this.name, getParser, this.idParsers |> Option.map (Map.toList >> List.map fst) |> Option.defaultValue [])


  interface RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId> with
    member this.Name = this.name
    member this.IdToDomain ctx str =
      match this.idParsers |> Option.defaultValue Map.empty |> Map.toList with
      | [] -> failwithf "Relationship '%s' does not contain any ID parsers and may not be used to parse query IDs" this.name
      | [_, parseId] -> parseId ctx str
      | _::_::_ -> failwithf "Relationship '%s' contains ID parsers for several types and may therefore not be used to parse query IDs" this.name


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)


  interface RelationshipHandlers<'ctx> with

    member this.Name = this.name

    member _.IsToMany = false

    member this.IsSettableButNotGettable = this.set.IsSome && this.get.IsNone

    member this.IsSettableWithoutPersist = this.set.IsSome && this.afterModifySelf.IsNone

    member this.GetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resp ->
          fun next httpCtx ->
            job {
              let entity = unbox<'entity> entity
              match! getRelated ctx entity with
              | Skip -> return! handleErrors [getRelWhileSkip] next httpCtx
              | Include relatedEntity ->
                  let b = resolveEntity relatedEntity
                  let! doc = resp.Write ctx req (b.resourceDef, b.entity)
                  let handler =
                    setStatusCode 200
                    >=> this.modifyGetRelatedResponse ctx entity relatedEntity
                    >=> jsonApiWithETag<'ctx> doc
                  return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity ->
          fun next httpCtx ->
            job {
              (*
              TODO: Support 'include' query parameter for relationship self (for all relationships)

              https://jsonapi.org/format/#fetching-includes
              
                  Furthermore, related resources can be requested from a relationship endpoint:
              
                  GET /articles/1/relationships/comments?include=comments.author HTTP/1.1
              
                  In this case, the primary data would be a collection of resource identifier objects that
                  represent linkage to comments for an article, while the full comments and comment authors
                  would be returned as included data.
              *)
              if req.Query.ContainsKey "include" then
                return! handleErrors [relSelfIncludeNotSupported] next httpCtx
              else
                let entity = unbox<'entity> entity
                match! getRelated ctx entity with
                | Skip -> return! handleErrors [getRelWhileSkip] next httpCtx
                | Include relatedEntity ->
                    let b = resolveEntity relatedEntity
                    let doc : ResourceIdentifierDocument = {
                      jsonapi = Skip
                      links = Skip
                      meta = Skip
                      data = Some { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                    }
                    let handler =
                      setStatusCode 200
                      >=> this.modifyGetSelfResponse ctx entity relatedEntity
                      >=> jsonApiWithETag<'ctx> doc
                    return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member _.PostSelf = None

    member this.PatchSelf =
      (this.get, this.set) ||> Option.map2 (fun getRelated set ->
        let idParsers =
          this.idParsers
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        let afterModifySelf =
          this.afterModifySelf
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without AfterModifySelf. This should be caught at startup.")
        fun ctx req parentTypeName preconditions entity0 ->
          fun next httpCtx ->
            job {
              if req.Query.ContainsKey "include" then
                return! handleErrors [relSelfIncludeNotSupported] next httpCtx
              else
                match req.IdentifierDocument.Value with
                | Error errs -> return! handleErrors errs next httpCtx
                | Ok None -> return! handleErrors [modifyRelSelfMissingData ""] next httpCtx
                | Ok (Some { data = None }) -> return! handleErrors [relInvalidNull parentTypeName this.name "/data"] next httpCtx
                | Ok (Some { data = Some id }) ->
                    match preconditions.Validate httpCtx ctx entity0 with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok () ->
                        match! this.beforeModifySelf ctx (unbox<'entity> entity0) with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok entity1 ->
                            match idParsers.TryGetValue id.``type`` with
                            | false, _ ->
                                let allowedTypes = idParsers |> Map.toList |> List.map fst
                                return! handleErrors [relInvalidTypeSelf id.``type`` allowedTypes "/data/type"] next httpCtx
                            | true, parseId ->
                                let! entity2Res =
                                  parseId ctx id.id
                                  // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                  // this just means that the resource does not exist, which is a more helpful result.
                                  |> JobResult.mapError (fun _ -> [relatedResourceNotFound ("/data")])
                                  |> JobResult.bind (fun domain ->
                                      set ctx "/data" domain (unbox<'entity> entity1)
                                  )
                                match entity2Res with
                                | Error errs -> return! handleErrors errs next httpCtx
                                | Ok entity2 ->
                                    match! afterModifySelf ctx (unbox<'entity> entity0) (unbox<'entity> entity2) with
                                    | Error errors -> return! handleErrors errors next httpCtx
                                    | Ok entity3 ->
                                        if this.patchSelfReturn202Accepted then
                                          let handler =
                                            setStatusCode 202
                                            >=> this.modifyPatchSelfAcceptedResponse ctx (unbox<'entity> entity2)
                                          return! handler next httpCtx
                                        else
                                          match! getRelated ctx (unbox<'entity> entity3) with
                                          | Skip ->
                                              let logger = httpCtx.GetLogger("Felicity.Relationships")
                                              logger.LogError("Relationship {RelationshipName} was updated using a self URL, but no success response could be returned becuase the relationship getter returned Skip. This violates the JSON:API specification. Make sure that the relationship getter never returns Skip after an update.", this.name)
                                              return! handleErrors [relModifySelfWhileSkip] next httpCtx
                                          | Include relatedEntity ->
                                              let b = resolveEntity relatedEntity
                                              let doc : ResourceIdentifierDocument = {
                                                jsonapi = Skip
                                                links = Skip
                                                meta = Skip
                                                data = Some { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                                              }
                                              let handler =
                                                setStatusCode 200
                                                >=> this.modifyPatchSelfOkResponse ctx (unbox<'entity> entity3) relatedEntity
                                                >=> jsonApiWithETag<'ctx> doc
                                              return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member _.DeleteSelf = None


  member this.Name = this.name

  member this.GetJobSkip(get: Func<'ctx, 'entity, Job<'relatedEntity Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity Skippable>>) =
    this.GetJobSkip(Job.liftAsyncFunc2 get)

  member this.GetJob (get: Func<'ctx, 'entity, Job<'relatedEntity>>) =
    this.GetJobSkip(fun ctx r -> get.Invoke(ctx, r) |> Job.map Include)

  member this.GetJob (get: Func<'entity, Job<'relatedEntity>>) =
    this.GetJobSkip(fun _ r -> get.Invoke r |> Job.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity>>) =
    this.GetJob(Job.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity>>) =
    this.GetJob(Job.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity>>) =
    this.GetJobSkip(fun ctx r -> get.Invoke(ctx, r) |> Job.result)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity>) =
    this.GetJobSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> Job.result)

  member this.Get (get: Func<'entity, 'relatedEntity>) =
    this.GetJobSkip(fun _ r -> get.Invoke r |> Include |> Job.result)

  member private this.SetJobRes (set: Func<'ctx, Pointer, 'relatedId, 'entity, Job<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with set = Some (fun ctx ptr relId e -> set.Invoke(ctx, ptr, relId, e)) }

  member this.SetJobRes (set: Func<'ctx, 'relatedId, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(fun ctx pointer relId e -> set.Invoke(ctx, relId, e) |> JobResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetJobRes (set: Func<'relatedId, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(fun _ id e -> set.Invoke(id, e))

  member this.SetJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(this.toIdSetter getRelated (fun ctx relId e -> set.Invoke(ctx, relId, e)))

  member this.SetJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(this.toIdSetter getRelated (fun _ id e -> set.Invoke(id, e)))

  member this.SetAsyncRes (set: Func<'ctx, 'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(Job.liftAsyncFunc3 set)

  member this.SetAsyncRes (set: Func<'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(Job.liftAsyncFunc2 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(getRelated, Job.liftAsyncFunc3 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(getRelated, Job.liftAsyncFunc2 set)

  member this.SetJob (set: Func<'ctx, 'relatedId, 'entity, Job<'entity>>) =
    this.SetJobRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.SetJob (set: Func<'relatedId, 'entity, Job<'entity>>) =
    this.SetJobRes(fun _ related entity -> set.Invoke(related, entity) |> Job.map Ok)

  member this.SetJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Job<'entity>>) =
    this.SetJobRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Job.map Ok))

  member this.SetJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Job<'entity>>) =
    this.SetJobRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Job.map Ok))

  member this.SetAsync (set: Func<'ctx, 'relatedId, 'entity, Async<'entity>>) =
    this.SetJob(Job.liftAsyncFunc3 set)

  member this.SetAsync (set: Func<'relatedId, 'entity, Async<'entity>>) =
    this.SetJob(Job.liftAsyncFunc2 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<'entity>>) =
    this.SetJob(getRelated, Job.liftAsyncFunc3 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<'entity>>) =
    this.SetJob(getRelated, Job.liftAsyncFunc2 set)

  member this.SetRes (set: Func<'ctx, 'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(Job.liftFunc3 set)

  member this.SetRes (set: Func<'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(Job.liftFunc2 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(getRelated, Job.liftFunc3 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(getRelated, Job.liftFunc2 set)

  member this.Set (set: Func<'ctx, 'relatedId, 'entity, 'entity>) =
    this.SetJobRes(JobResult.liftFunc3 set)

  member this.Set (set: Func<'relatedId, 'entity, 'entity>) =
    this.SetJobRes(JobResult.liftFunc2 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, 'entity>) =
    this.SetJobRes(getRelated, JobResult.liftFunc3 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, 'entity>) =
    this.SetJobRes(getRelated, JobResult.liftFunc2 set)

  member this.AddConstraintsJob(getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            job {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsJob(Job.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsJob(Job.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsJob(fun ctx e -> [name, box (getValue ctx e)] |> Job.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.AddConstraint (name: string, value: 'a) =
    this.AddConstraint(name, fun _ -> value)

  member this.BeforeModifySelfJobRes(f: Func<'ctx, 'entity, Job<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfJobRes(f: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> JobResult.map (fun () -> e))

  member this.BeforeModifySelfJobRes(f: Func<'entity, Job<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfJobRes(f: Func<'entity, Job<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc f)

  member this.BeforeModifySelfJob(f: Func<'ctx, 'entity, Job<'entity>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'ctx, 'entity, Job<unit>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'entity, Job<'entity>>) =
    this.BeforeModifySelfJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'entity, Job<unit>>) =
    this.BeforeModifySelfJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc f)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc f)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, unit>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc f)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc f)

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> 'entity -> Job<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew |> JobResult.map (fun () -> eNew)) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx _ e -> f ctx e) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> Job<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> JobResult.map (fun () -> e))

  member this.AfterModifySelfJobRes(f: 'entity -> Job<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(fun _ e -> f e)

  member this.AfterModifySelfJobRes(f: 'entity -> Job<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(fun _ e -> f e |> JobResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync f)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun ctx eOld eNew -> f ctx eOld eNew |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun ctx eOld eNew -> f ctx eOld eNew |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun e -> f e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun e -> f e |> Job.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift3 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift3 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift2 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift f)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift3 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift3 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift2 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift f)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift f)

  member this.PatchSelfReturn202Accepted () =
    { this with patchSelfReturn202Accepted = true }

  member this.ModifyGetRelatedResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler) =
    { this with modifyGetRelatedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyGetRelatedResponse(f: 'ctx -> 'entity -> 'relatedEntity -> HttpContext -> unit) =
    this.ModifyGetRelatedResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetRelatedResponse(handler: HttpHandler) =
    this.ModifyGetRelatedResponse(fun _ _ _ -> handler)

  member this.ModifyGetSelfResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler) =
    { this with modifyGetSelfResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyGetSelfResponse(f: 'ctx -> 'entity -> 'relatedEntity -> HttpContext -> unit) =
    this.ModifyGetSelfResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetSelfResponse(handler: HttpHandler) =
    this.ModifyGetSelfResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfOkResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler) =
    { this with modifyPatchSelfOkResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPatchSelfOkResponse(f: 'ctx -> 'entity -> 'relatedEntity -> HttpContext -> unit) =
    this.ModifyPatchSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPatchSelfOkResponse(handler: HttpHandler) =
    this.ModifyPatchSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfAcceptedResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyPatchSelfAcceptedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPatchSelfAcceptedResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyPatchSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPatchSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPatchSelfAcceptedResponse(fun _ _ -> handler)



type internal ToOneNullableRelationship<'ctx> =
  abstract Name: RelationshipName
  abstract SelfLink: bool
  abstract RelatedLink: bool
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Job<Skippable<(ResourceDefinition<'ctx> * BoxedEntity) option>>) option



type ToOneNullableRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  idGetter: RequestGetter<'ctx, 'relatedId option option>
  getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>
} with

  static member internal Create(name, idGetter: RequestGetter<'ctx, 'relatedId option option>, getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) : ToOneNullableRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      idGetter = idGetter
      getRelated = getRelated
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          this.idGetter.Get(ctx, req, includedTypeAndId)
          |> JobResult.bind (  // ID did not fail parsing, but may be missing or null
              Option.traverseJobResult (  // ID was present, but may be null
                Option.traverseJobResult (  // ID was not null
                  this.getRelated.GetById ctx
                  >> JobResult.mapError (List.map (Error.setSourcePointer ("/data/relationships/" + this.name + "/data")))
                  >> JobResult.requireSome [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")]
                )
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToOneNullableRelationshipIncludedGetter<'ctx, 'relatedEntity> = internal {
  name: string
  getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>
  allowedTypes: ResourceTypeName list
} with

  static member internal Create(name, getParser, allowedTypes) : ToOneNullableRelationshipIncludedGetter<'ctx, 'relatedEntity> =
    {
      name = name
      getParser = getParser
      allowedTypes = allowedTypes
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOneNullable as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Job.result
                  | { data = Include identifier } ->
                      identifier
                      |> Option.traverseJobResult (fun id ->
                          if not (this.allowedTypes |> List.contains id.``type``) then
                            let pointer = relsPointer + "/" + this.name + "/data/type"
                            Error [relInvalidType this.name id.``type`` this.allowedTypes pointer] |> Job.result
                          else
                            RequestParserHelper<'ctx>(ctx, req, (id.``type``, id.id))
                            |> this.getParser
                            |> fun p -> p.ParseJob()
                      )
                      |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOneNullable>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result

    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToOneNullableRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Job<Result<'relatedId, Error list>>> option
  get: ('ctx -> 'entity -> Job<'relatedEntity option Skippable>) option
  set: ('ctx -> Pointer -> 'relatedId option -> 'entity -> Job<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>
  beforeModifySelf: 'ctx -> 'entity -> Job<Result<'entity, Error list>>
  afterModifySelf: ('ctx -> 'entity -> 'entity -> Job<Result<'entity, Error list>>) option
  modifyGetRelatedResponse: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler
  modifyGetSelfResponse: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler
  modifyPatchSelfOkResponse: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler
  modifyPatchSelfAcceptedResponse: 'ctx -> 'entity -> HttpHandler
  patchSelfReturn202Accepted: bool
} with

  static member internal Create (name: string, resolveEntity, idParsers) : ToOneNullableRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      resolveEntity = resolveEntity
      idParsers = idParsers
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> Job.result []
      beforeModifySelf = fun _ e -> Ok e |> Job.result
      afterModifySelf = None
      modifyGetRelatedResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyGetSelfResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      patchSelfReturn202Accepted = false
    }

  member private _.toIdSetter (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) entitySetter =
    fun ctx (dataPointer: Pointer) relatedId entity ->
      relatedId
      |> Option.traverseJobResult (
          getRelated.GetById ctx
          >> JobResult.mapError (List.map (Error.setSourcePointer dataPointer))
          >> JobResult.requireSome [relatedResourceNotFound dataPointer]
      )
      |> JobResult.bind (fun r ->
        entitySetter ctx r entity
        |> JobResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwithf "Attempted to parse resource ID for polymorphic relationship '%s', but no ID parsers have been specified." this.name)
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOneNullable as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + this.name)] |> Job.result
                  | { data = Include identifier } ->
                      identifier
                      |> Option.traverseJobResult (fun id ->
                          match idParsers.TryGetValue id.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = relsPointer + this.name + "/data/type"
                              Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> Job.result
                          | true, parseId ->
                              parseId ctx id.id
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> JobResult.mapError (fun _ -> [relatedResourceNotFound (relsPointer + this.name + "/data")])
                              |> JobResult.map Some
                      )
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOneNullable>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result
    }


  interface OptionalRequestGetter<'ctx, 'relatedId option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedId option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          let pointer = Request.pointerForMissingRel includedTypeAndId req + "/" + this.name
          [reqParserProhibitedRel this.name pointer]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      job {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { relationships = Include rels } }) ->
            match this.set, rels.TryFind this.name with
            | _, None -> return Ok entity // not provided in request
            | None, Some _ -> return Error [setRelReadOnly this.name ("/data/relationships/" + this.name)]
            | Some set, Some (:? ToOneNullable as rel) ->
                let idParsers =
                  this.idParsers
                  |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
                match rel.data with
                | Skip -> return Error [relMissingData this.name ("/data/relationships/" + this.name)]
                | Include identifier ->
                    return!
                      identifier
                      |> Option.traverseJobResult (fun id ->
                          match idParsers.TryGetValue id.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = "/data/relationships/" + this.name + "/data/type"
                              Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> Job.result
                          | true, parseId ->
                            parseId ctx id.id
                            // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                            // this just means that the resource does not exist, which is a more helpful result.
                            |> JobResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                      )
                      |> JobResult.bind (fun domain ->
                          set ctx ("/data/relationships/" + this.name + "/data") domain (unbox<'entity> entity))
                      |> JobResult.map box<'entity>
            | Some _, Some rel -> return failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOneNullable>.FullName (rel.GetType().FullName)
        | _ -> return Ok entity  // no relationships provided
      }


  interface ToOneNullableRelationship<'ctx> with
    member this.Name = this.name
    member this.SelfLink = this.get.IsSome
    member this.RelatedLink = this.get.IsSome
    member this.BoxedGetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx entity ->
          getRelated ctx (unbox<'entity> entity) |> Job.map (Skippable.map (Option.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          )))
      )

  
  member this.Related (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) =
    ToOneNullableRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(this.name, this.Optional, getRelated)


  member this.Included (getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>) =
    ToOneNullableRelationshipIncludedGetter.Create(this.name, getParser, this.idParsers |> Option.map (Map.toList >> List.map fst) |> Option.defaultValue [])


  interface RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId> with
    member this.Name = this.name
    member this.IdToDomain ctx str =
      match this.idParsers |> Option.defaultValue Map.empty |> Map.toList with
      | [] -> failwithf "Relationship '%s' does not contain any ID parsers and may not be used to parse query IDs" this.name
      | [_, parseId] -> parseId ctx str
      | _::_::_ -> failwithf "Relationship '%s' contains ID parsers for several types and may therefore not be used to parse query IDs" this.name


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)


  interface RelationshipHandlers<'ctx> with

    member this.Name = this.name
    
    member _.IsToMany = false
    
    member this.IsSettableButNotGettable = this.set.IsSome && this.get.IsNone

    member this.IsSettableWithoutPersist = this.set.IsSome && this.afterModifySelf.IsNone

    member this.GetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resp ->
          fun next httpCtx ->
            job {
              let entity = unbox<'entity> entity
              match! getRelated ctx entity with
              | Skip -> return! handleErrors [getRelWhileSkip] next httpCtx
              | Include relatedEntity ->
                  let! doc = resp.WriteOpt ctx req (relatedEntity |> Option.map (fun e ->
                    let b = resolveEntity e
                    b.resourceDef, b.entity
                  ))
                  let handler =
                    setStatusCode 200
                    >=> this.modifyGetRelatedResponse ctx entity relatedEntity
                    >=> jsonApiWithETag<'ctx> doc
                  return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity ->
          fun next httpCtx ->
            job {
              if req.Query.ContainsKey "include" then
                return! handleErrors [relSelfIncludeNotSupported] next httpCtx
              else
                let entity = unbox<'entity> entity
                match! getRelated ctx entity with
                | Skip -> return! handleErrors [getRelWhileSkip] next httpCtx
                | Include relatedEntity ->
                    let doc : ResourceIdentifierDocument = {
                      jsonapi = Skip
                      links = Skip
                      meta = Skip
                      data =
                        relatedEntity |> Option.map (fun e ->
                          let b = resolveEntity e
                          { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                      )
                    }
                    let handler =
                      setStatusCode 200
                      >=> this.modifyGetSelfResponse ctx entity relatedEntity
                      >=> jsonApiWithETag<'ctx> doc
                    return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member _.PostSelf = None

    member this.PatchSelf =
      (this.get, this.set) ||> Option.map2 (fun getRelated set ->
        let idParsers =
          this.idParsers
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        let afterModifySelf =
          this.afterModifySelf
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without AfterModifySelf. This should be caught at startup.")
        fun ctx req parentTypeName preconditions entity0 ->
          fun next httpCtx ->
            job {
              if req.Query.ContainsKey "include" then
                return! handleErrors [relSelfIncludeNotSupported] next httpCtx
              else
                match req.IdentifierDocument.Value with
                | Error errs -> return! handleErrors errs next httpCtx
                | Ok None -> return! handleErrors [modifyRelSelfMissingData ""] next httpCtx
                | Ok (Some { data = identifier }) ->
                    match preconditions.Validate httpCtx ctx entity0 with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok () ->
                        match! this.beforeModifySelf ctx (unbox<'entity> entity0) with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok entity1 ->
                            let! entity2Res =
                              identifier
                              |> Option.traverseJobResult (fun id ->
                                  match idParsers.TryGetValue id.``type`` with
                                  | false, _ ->
                                      let allowedTypes = idParsers |> Map.toList |> List.map fst
                                      Error [relInvalidTypeSelf id.``type`` allowedTypes "/data/type"] |> Job.result
                                  | true, parseId ->
                                      parseId ctx id.id
                                      // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                      // this just means that the resource does not exist, which is a more helpful result.
                                      |> JobResult.mapError (fun _ -> [relatedResourceNotFound ("/data")])
                              )
                              |> JobResult.bind (fun domain -> set ctx "/data" domain (unbox<'entity> entity1))
                            match entity2Res with
                            | Error errs -> return! handleErrors errs next httpCtx
                            | Ok entity2 ->
                                match! afterModifySelf ctx (unbox<'entity> entity0) (unbox<'entity> entity2) with
                                | Error errors -> return! handleErrors errors next httpCtx
                                | Ok entity3 ->
                                    if this.patchSelfReturn202Accepted then
                                      let handler =
                                        setStatusCode 202
                                        >=> this.modifyPatchSelfAcceptedResponse ctx (unbox<'entity> entity3)
                                      return! handler next httpCtx
                                    else
                                      match! getRelated ctx (unbox<'entity> entity3) with
                                      | Skip ->
                                          let logger = httpCtx.GetLogger("Felicity.Relationships")
                                          logger.LogError("Relationship {RelationshipName} was updated using a self URL, but no success response could be returned becuase the relationship getter returned Skip. This violates the JSON:API specification. Make sure that the relationship getter never returns Skip after an update.", this.name)
                                          return! handleErrors [relModifySelfWhileSkip] next httpCtx
                                      | Include relatedEntity ->
                                          let doc : ResourceIdentifierDocument = {
                                            jsonapi = Skip
                                            links = Skip
                                            meta = Skip
                                            data =
                                              relatedEntity |> Option.map (fun e ->
                                                let b = resolveEntity e
                                                { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                                              )
                                          }
                                          let handler =
                                            setStatusCode 200
                                            >=> this.modifyPatchSelfOkResponse ctx (unbox<'entity> entity3) relatedEntity
                                            >=> jsonApiWithETag<'ctx> doc
                                          return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member _.DeleteSelf = None


  member this.Name = this.name

  member this.GetJobSkip(get: Func<'ctx, 'entity, Job<'relatedEntity option Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity option Skippable>>) =
    this.GetJobSkip(Job.liftAsyncFunc2 get)

  member this.GetJob (get: Func<'ctx, 'entity, Job<'relatedEntity option>>) =
    this.GetJobSkip(fun ctx r -> get.Invoke(ctx, r) |> Job.map Include)

  member this.GetJob (get: Func<'entity, Job<'relatedEntity option>>) =
    this.GetJobSkip(fun _ r -> get.Invoke r |> Job.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity option>>) =
    this.GetJob(Job.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity option>>) =
    this.GetJob(Job.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity option>>) =
    this.GetJobSkip(fun ctx r -> get.Invoke(ctx, r) |> Job.result)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity option>) =
    this.GetJobSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> Job.result)

  member this.Get (get: Func<'entity, 'relatedEntity option>) =
    this.GetJobSkip(fun _ r -> get.Invoke r |> Include |> Job.result)

  member private this.SetJobRes (set: Func<'ctx, Pointer, 'relatedId option, 'entity, Job<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with set = Some (fun ctx ptr relId e -> set.Invoke(ctx, ptr, relId, e)) }

  member this.SetJobRes (set: Func<'ctx, 'relatedId option, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(fun ctx pointer relId e -> set.Invoke(ctx, relId, e) |> JobResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetJobRes (set: Func<'relatedId option, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(fun _ id e -> set.Invoke(id, e))

  member this.SetJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(this.toIdSetter getRelated (fun ctx relId e -> set.Invoke(ctx, relId, e)))

  member this.SetJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(this.toIdSetter getRelated (fun _ id e -> set.Invoke(id, e)))

  member this.SetAsyncRes (set: Func<'ctx, 'relatedId option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(Job.liftAsyncFunc3 set)

  member this.SetAsyncRes (set: Func<'relatedId option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(Job.liftAsyncFunc2 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(getRelated, Job.liftAsyncFunc3 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetJobRes(getRelated, Job.liftAsyncFunc2 set)

  member this.SetJob (set: Func<'ctx, 'relatedId option, 'entity, Job<'entity>>) =
    this.SetJobRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.SetJob (set: Func<'relatedId option, 'entity, Job<'entity>>) =
    this.SetJobRes(fun _ related entity -> set.Invoke(related, entity) |> Job.map Ok)

  member this.SetJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Job<'entity>>) =
    this.SetJobRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Job.map Ok))

  member this.SetJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Job<'entity>>) =
    this.SetJobRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Job.map Ok))

  member this.SetAsync (set: Func<'ctx, 'relatedId option, 'entity, Async<'entity>>) =
    this.SetJob(Job.liftAsyncFunc3 set)

  member this.SetAsync (set: Func<'relatedId option, 'entity, Async<'entity>>) =
    this.SetJob(Job.liftAsyncFunc2 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Async<'entity>>) =
    this.SetJob(getRelated, Job.liftAsyncFunc3 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Async<'entity>>) =
    this.SetJob(getRelated, Job.liftAsyncFunc2 set)

  member this.SetRes (set: Func<'ctx, 'relatedId option, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(Job.liftFunc3 set)

  member this.SetRes (set: Func<'relatedId option, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(Job.liftFunc2 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(getRelated, Job.liftFunc3 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Result<'entity, Error list>>) =
    this.SetJobRes(getRelated, Job.liftFunc2 set)

  member this.Set (set: Func<'ctx, 'relatedId option, 'entity, 'entity>) =
    this.SetJobRes(JobResult.liftFunc3 set)

  member this.Set (set: Func<'relatedId option, 'entity, 'entity>) =
    this.SetJobRes(JobResult.liftFunc2 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, 'entity>) =
    this.SetJobRes(getRelated, JobResult.liftFunc3 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, 'entity>) =
    this.SetJobRes(getRelated, JobResult.liftFunc2 set)

  member this.SetNonNullJobRes (set: Func<'ctx, 'relatedId, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(fun ctx relId e ->
      relId
      |> Result.requireSome [setRelNullNotAllowed this.name]
      |> Job.result
      |> JobResult.bind (fun relId -> set.Invoke(ctx, relId, e))
    )

  member this.SetNonNullJobRes (set: Func<'relatedId, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetNonNullJobRes(fun _ id e -> set.Invoke(id, e))

  member this.SetNonNullJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetJobRes(getRelated, fun ctx relId e ->
      relId
      |> Result.requireSome [setRelNullNotAllowed this.name]
      |> Job.result
      |> JobResult.bind (fun relId -> set.Invoke(ctx, relId, e))
    )

  member this.SetNonNullJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetNonNullJobRes(getRelated, fun _ id e -> set.Invoke(id, e))

  member this.SetNonNullAsyncRes (set: Func<'ctx, 'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullJobRes(Job.liftAsyncFunc3 set)

  member this.SetNonNullAsyncRes (set: Func<'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullJobRes(Job.liftAsyncFunc2 set)

  member this.SetNonNullAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullJobRes(getRelated, Job.liftAsyncFunc3 set)

  member this.SetNonNullAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullJobRes(getRelated, Job.liftAsyncFunc2 set)

  member this.SetNonNullJob (set: Func<'ctx, 'relatedId, 'entity, Job<'entity>>) =
    this.SetNonNullJobRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.SetNonNullJob (set: Func<'relatedId, 'entity, Job<'entity>>) =
    this.SetNonNullJobRes(fun _ related entity -> set.Invoke(related, entity) |> Job.map Ok)

  member this.SetNonNullJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Job<'entity>>) =
    this.SetNonNullJobRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Job.map Ok))

  member this.SetNonNullJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Job<'entity>>) =
    this.SetNonNullJobRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Job.map Ok))

  member this.SetNonNullAsync (set: Func<'ctx, 'relatedId, 'entity, Async<'entity>>) =
    this.SetNonNullJob(Job.liftAsyncFunc3 set)

  member this.SetNonNullAsync (set: Func<'relatedId, 'entity, Async<'entity>>) =
    this.SetNonNullJob(Job.liftAsyncFunc2 set)

  member this.SetNonNullAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<'entity>>) =
    this.SetNonNullJob(getRelated, Job.liftAsyncFunc3 set)

  member this.SetNonNullAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<'entity>>) =
    this.SetNonNullJob(getRelated, Job.liftAsyncFunc2 set)

  member this.SetNonNullRes (set: Func<'ctx, 'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullJobRes(Job.liftFunc3 set)

  member this.SetNonNullRes (set: Func<'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullJobRes(Job.liftFunc2 set)

  member this.SetNonNullRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullJobRes(getRelated, Job.liftFunc3 set)

  member this.SetNonNullRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullJobRes(getRelated, Job.liftFunc2 set)

  member this.SetNonNull (set: Func<'ctx, 'relatedId, 'entity, 'entity>) =
    this.SetNonNullJobRes(JobResult.liftFunc3 set)

  member this.SetNonNull (set: Func<'relatedId, 'entity, 'entity>) =
    this.SetNonNullJobRes(JobResult.liftFunc2 set)

  member this.SetNonNull (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, 'entity>) =
    this.SetNonNullJobRes(getRelated, JobResult.liftFunc3 set)

  member this.SetNonNull (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, 'entity>) =
    this.SetNonNullJobRes(getRelated, JobResult.liftFunc2 set)

  member this.AddConstraintsJob(getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            job {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsJob(Job.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsJob(Job.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsJob(fun ctx e -> [name, box (getValue ctx e)] |> Job.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.AddConstraint (name: string, value: 'a) =
    this.AddConstraint(name, fun _ -> value)

  member this.BeforeModifySelfJobRes(f: Func<'ctx, 'entity, Job<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfJobRes(f: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> JobResult.map (fun () -> e))

  member this.BeforeModifySelfJobRes(f: Func<'entity, Job<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfJobRes(f: Func<'entity, Job<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc f)

  member this.BeforeModifySelfJob(f: Func<'ctx, 'entity, Job<'entity>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'ctx, 'entity, Job<unit>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'entity, Job<'entity>>) =
    this.BeforeModifySelfJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'entity, Job<unit>>) =
    this.BeforeModifySelfJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc f)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc f)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, unit>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc f)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc f)

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> 'entity -> Job<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew |> JobResult.map (fun () -> eNew)) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx _ e -> f ctx e) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> Job<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> JobResult.map (fun () -> e))

  member this.AfterModifySelfJobRes(f: 'entity -> Job<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(fun _ e -> f e)

  member this.AfterModifySelfJobRes(f: 'entity -> Job<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(fun _ e -> f e |> JobResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync f)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun ctx eOld eNew -> f ctx eOld eNew |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun ctx eOld eNew -> f ctx eOld eNew |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun e -> f e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun e -> f e |> Job.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift3 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift3 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift2 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift f)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift3 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift3 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift2 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift f)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift f)

  member this.PatchSelfReturn202Accepted () =
    { this with patchSelfReturn202Accepted = true }

  member this.ModifyGetRelatedResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler) =
    { this with modifyGetRelatedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyGetRelatedResponse(f: 'ctx -> 'entity -> 'relatedEntity option -> HttpContext -> unit) =
    this.ModifyGetRelatedResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetRelatedResponse(handler: HttpHandler) =
    this.ModifyGetRelatedResponse(fun _ _ _ -> handler)

  member this.ModifyGetSelfResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler) =
    { this with modifyGetSelfResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyGetSelfResponse(f: 'ctx -> 'entity -> 'relatedEntity option -> HttpContext -> unit) =
    this.ModifyGetSelfResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetSelfResponse(handler: HttpHandler) =
    this.ModifyGetSelfResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfOkResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler) =
    { this with modifyPatchSelfOkResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPatchSelfOkResponse(f: 'ctx -> 'entity -> 'relatedEntity option -> HttpContext -> unit) =
    this.ModifyPatchSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPatchSelfOkResponse(handler: HttpHandler) =
    this.ModifyPatchSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfAcceptedResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyPatchSelfAcceptedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPatchSelfAcceptedResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyPatchSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPatchSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPatchSelfAcceptedResponse(fun _ _ -> handler)


type internal ToManyRelationship<'ctx> =
  abstract Name: RelationshipName
  abstract SelfLink: bool
  abstract RelatedLink: bool
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Job<Skippable<(ResourceDefinition<'ctx> * BoxedEntity) list>>) option


type ToManyRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  idGetter: RequestGetter<'ctx, 'relatedId list option>
  getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>
} with

  static member internal Create(name, idGetter: RequestGetter<'ctx, 'relatedId list option>, getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) : ToManyRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      idGetter = idGetter
      getRelated = getRelated
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity list option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          this.idGetter.Get(ctx, req, includedTypeAndId)
          |> JobResult.bind (
              Option.traverseJobResult (
                List.indexed
                >> List.traverseJobResultA (fun (i, id) ->
                    this.getRelated.GetById ctx id
                    |> JobResult.mapError (List.map (Error.setSourcePointer ("/data/relationships/" + this.name + "/data/" + string i)))
                    |> JobResult.requireSome [relatedResourceNotFound ("/data/relationships/" + this.name + "/data/" + string i)]
                )
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToManyRelationshipIncludedGetter<'ctx, 'relatedEntity> = internal {
  name: string
  getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>
  allowedTypes: ResourceTypeName list
} with

  static member internal Create(name, getParser, allowedTypes) : ToManyRelationshipIncludedGetter<'ctx, 'relatedEntity> =
    {
      name = name
      getParser = getParser
      allowedTypes = allowedTypes
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity list option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToMany as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Job.result
                  | { data = Include list } ->
                      list
                      |> List.indexed
                      |> List.traverseJobResultA (fun (i, identifier) ->
                          if not (this.allowedTypes |> List.contains identifier.``type``) then
                            let pointer = relsPointer + "/" + this.name + "/data/" + string i + "/type"
                            Error [relInvalidType this.name identifier.``type`` this.allowedTypes pointer] |> Job.result
                          else
                            RequestParserHelper<'ctx>(ctx, req, (identifier.``type``, identifier.id))
                            |> this.getParser
                            |> fun p -> p.ParseJob()
                      )
                      |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToMany>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result

    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToManyRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Job<Result<'relatedId, Error list>>> option
  get: ('ctx -> 'entity -> Job<'relatedEntity list Skippable>) option
  setAll: ('ctx -> Pointer -> 'relatedId list -> 'entity -> Job<Result<'entity, Error list>>) option
  add: ('ctx -> Pointer -> 'relatedId list -> 'entity -> Job<Result<'entity, Error list>>) option
  remove: ('ctx -> Pointer -> 'relatedId list -> 'entity -> Job<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>
  beforeModifySelf: 'ctx -> 'entity -> Job<Result<'entity, Error list>>
  afterModifySelf: ('ctx -> 'entity -> 'entity -> Job<Result<'entity, Error list>>) option
  modifyGetRelatedResponse: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyGetSelfResponse: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyPostSelfOkResponse: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyPostSelfAcceptedResponse: 'ctx -> 'entity -> HttpHandler
  modifyPatchSelfOkResponse: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyPatchSelfAcceptedResponse: 'ctx -> 'entity -> HttpHandler
  modifyDeleteSelfOkResponse: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyDeleteSelfAcceptedResponse: 'ctx -> 'entity -> HttpHandler
  modifySelfReturn202Accepted: bool
} with

  static member internal Create(name: string, resolveEntity, idParsers) : ToManyRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      resolveEntity = resolveEntity
      idParsers = idParsers
      get = None
      setAll = None
      add = None
      remove = None
      hasConstraints = false
      getConstraints = fun _ _ -> Job.result []
      beforeModifySelf = fun _ e -> Ok e |> Job.result
      afterModifySelf = None
      modifyGetRelatedResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyGetSelfResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPostSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPostSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      modifyPatchSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      modifyDeleteSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyDeleteSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      modifySelfReturn202Accepted = false
    }

  member private _.toIdSetter (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) (entitySetter: 'ctx -> 'relatedEntity list -> 'entity -> Job<Result<'entity, Error list>>) =
    fun ctx (dataPointer: Pointer) relatedIds entity ->
      relatedIds
      |> List.map (getRelated.GetById ctx)
      |> Job.conCollect
      |> Job.map (
          Seq.indexed
          >> Seq.toList
          >> List.traverseResultA (fun (i, t) ->
              t
              |> Result.mapError (List.map (Error.setSourcePointer (dataPointer + "/" + string i)))
              |> Result.bind (Result.requireSome [relatedResourceNotFound (dataPointer + "/" + string i)])
          )
      )
      |> JobResult.bind (fun r ->
          entitySetter ctx r entity
          |> JobResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId list option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwithf "Attempted to parse resource ID for polymorphic relationship '%s', but no ID parsers have been specified." this.name)
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToMany as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + this.name)] |> Job.result
                  | { data = Include list } ->
                      list
                      |> List.indexed
                      |> List.traverseJobResultA (fun (i, identifier) ->
                          match idParsers.TryGetValue identifier.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = relsPointer + this.name + "/data/" + string i + "/type"
                              Error [relInvalidType this.name identifier.``type`` allowedTypes pointer] |> Job.result
                          | true, parseId ->
                              parseId ctx identifier.id
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> JobResult.mapError (fun _ -> [relatedResourceNotFound (relsPointer + this.name + "/data/" + string i)])
                      )
                      |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToMany>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result
    }

  interface OptionalRequestGetter<'ctx, 'relatedId list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)


  interface RequestGetter<'ctx, 'relatedId list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          let pointer = Request.pointerForMissingRel includedTypeAndId req + "/" + this.name
          [reqParserProhibitedRel this.name pointer]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      job {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { ``type`` = t; relationships = Include rels } }) ->
            match this.setAll, rels.TryFind this.name with
            | _, None -> return Ok entity // not provided in request
            | None, Some _ ->
                if this.add.IsNone && this.remove.IsNone then
                  return Error [setRelReadOnly this.name ("/data/relationships/" + this.name)]
                else
                  return Error [setToManyRelReplacementNotSupported this.name t ("/data/relationships/" + this.name) this.add.IsSome this.remove.IsSome]
            | Some set, Some (:? ToMany as rel) ->
                let idParsers =
                  this.idParsers
                  |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
                match rel.data with
                | Skip -> return Error [relMissingData this.name ("/data/relationships/" + this.name)]
                | Include identifiers ->
                    return!
                      identifiers
                      |> List.indexed
                      |> List.traverseJobResultA (fun (i, id) ->
                          match idParsers.TryGetValue id.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = "/data/relationships/" + this.name + "/data/" + string i + "/type"
                              Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> Job.result
                          | true, parseId ->
                              parseId ctx id.id
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> JobResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data/" + string i)])
                      )
                      |> JobResult.bind (fun domain ->
                          set ctx ("/data/relationships/" + this.name + "/data") domain (unbox<'entity> entity)
                      )
                      |> JobResult.map box<'entity>
            | Some _, Some rel -> return failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToMany>.FullName (rel.GetType().FullName)
        | _ -> return Ok entity  // no relationships provided
      }


  interface ToManyRelationship<'ctx> with
    member this.Name = this.name
    member this.SelfLink = this.get.IsSome
    member this.RelatedLink = this.get.IsSome
    member this.BoxedGetRelated =
      this.get |> Option.map (fun get ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx entity ->
          get ctx (unbox<'entity> entity) |> Job.map (Skippable.map (List.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          )))
      )


  member this.Related (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) =
    ToManyRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(this.name, this.Optional, getRelated)


  member this.Included (getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>) =
    ToManyRelationshipIncludedGetter.Create(this.name, getParser, this.idParsers |> Option.map (Map.toList >> List.map fst) |> Option.defaultValue [])


  interface RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId> with
    member this.Name = this.name
    member this.IdToDomain ctx str =
      match this.idParsers |> Option.defaultValue Map.empty |> Map.toList with
      | [] -> failwithf "Relationship '%s' does not contain any ID parsers and may not be used to parse query IDs" this.name
      | [_, parseId] -> parseId ctx str
      | _::_::_ -> failwithf "Relationship '%s' contains ID parsers for several types and may therefore not be used to parse query IDs" this.name


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)


  member private this.ModifySelfHandler f modifyOkResponse modifyAcceptedResponse =
    (this.get, f) ||> Option.map2 (fun getRelated f ->
      let idParsers =
        this.idParsers
        |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
      let resolveEntity =
        this.resolveEntity
        |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
      let afterModifySelf =
        this.afterModifySelf
        |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without AfterModifySelf. This should be caught at startup.")
      fun ctx req (preconditions: Preconditions<'ctx>) entity0 ->
        fun next httpCtx ->
          job {
            if req.Query.ContainsKey "include" then
              return! handleErrors [relSelfIncludeNotSupported] next httpCtx
            else
              match req.IdentifierCollectionDocument.Value with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok None -> return! handleErrors [modifyRelSelfMissingData ""] next httpCtx
              | Ok (Some { data = ids }) ->
                  match preconditions.Validate httpCtx ctx entity0 with
                  | Error errors -> return! handleErrors errors next httpCtx
                  | Ok () ->
                      match! this.beforeModifySelf ctx (unbox<'entity> entity0) with
                      | Error errors -> return! handleErrors errors next httpCtx
                      | Ok entity1 ->
                          let! entity2Res =
                            ids
                            |> List.indexed
                            |> List.traverseJobResultA (fun (i, id) ->
                                match idParsers.TryGetValue id.``type`` with
                                | false, _ ->
                                    let allowedTypes = idParsers |> Map.toList |> List.map fst
                                    let pointer = "/data/" + string i + "/type"
                                    Error [relInvalidTypeSelf id.``type`` allowedTypes pointer] |> Job.result
                                | true, parseId ->
                                    parseId ctx id.id
                                    // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                    // this just means that the resource does not exist, which is a more helpful result.
                                    |> JobResult.mapError (fun _ -> [relatedResourceNotFound ("/data/" + string i)])
                            )
                            |> JobResult.bind (fun domain -> f ctx "/data" domain (unbox<'entity> entity1))
                          match entity2Res with
                          | Error errs -> return! handleErrors errs next httpCtx
                          | Ok entity2 ->
                              match! afterModifySelf ctx (unbox<'entity> entity0) (unbox<'entity> entity2) with
                              | Error errors -> return! handleErrors errors next httpCtx
                              | Ok entity3 ->
                                  if this.modifySelfReturn202Accepted then
                                    let handler =
                                      setStatusCode 202
                                      >=> modifyAcceptedResponse ctx (unbox<'entity> entity3)
                                    return! handler next httpCtx
                                  else
                                    match! getRelated ctx (unbox<'entity> entity3) with
                                    | Skip ->
                                        let logger = httpCtx.GetLogger("Felicity.Relationships")
                                        logger.LogError("Relationship {RelationshipName} was updated using a self URL, but no success response could be returned becuase the relationship getter returned Skip. This violates the JSON:API specification. Make sure that the relationship getter never returns Skip after an update.", this.name)
                                        return! handleErrors [relModifySelfWhileSkip] next httpCtx
                                    | Include relatedEntities ->
                                        let doc : ResourceIdentifierCollectionDocument = {
                                          jsonapi = Skip
                                          links = Skip
                                          meta = Skip
                                          data = relatedEntities |> List.map (fun e ->
                                            let b = resolveEntity e
                                            { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                                          )
                                        }
                                        let handler =
                                          setStatusCode 200
                                          >=> modifyOkResponse ctx (unbox<'entity> entity3) relatedEntities
                                          >=> jsonApiWithETag<'ctx> doc
                                        return! handler next httpCtx
          }
          |> Job.startAsTask
    )


  interface RelationshipHandlers<'ctx> with

    member this.Name = this.name
    
    member _.IsToMany = true

    member this.IsSettableButNotGettable = this.setAll.IsSome && this.get.IsNone

    member this.IsSettableWithoutPersist = (this.setAll.IsSome || this.add.IsSome || this.remove.IsSome) && this.afterModifySelf.IsNone

    member this.GetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resp ->
          fun next httpCtx ->
            job {
              // TODO: Support 'sort' query parameter
              if httpCtx.TryGetQueryStringValue "sort" |> Option.isSome then
                return! handleErrors [sortNotSupported] next httpCtx
              else
                let entity = unbox<'entity> entity
                match! getRelated ctx entity with
                | Skip -> return! handleErrors [getRelWhileSkip] next httpCtx
                | Include relatedEntities ->
                    let! doc = resp.WriteList ctx req (relatedEntities |> List.map (fun e ->
                      let b = resolveEntity e
                      b.resourceDef, b.entity
                    ))
                    let handler =
                      setStatusCode 200
                      >=> this.modifyGetRelatedResponse ctx entity relatedEntities
                      >=> jsonApiWithETag<'ctx> doc
                    return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity ->
          fun next httpCtx ->
            job {
              // TODO: Support 'include' query parameter
              if req.Query.ContainsKey "include" then
                return! handleErrors [relSelfIncludeNotSupported] next httpCtx
              else
                let entity = unbox<'entity> entity
                match! getRelated ctx entity with
                | Skip -> return! handleErrors [getRelWhileSkip] next httpCtx
                | Include relatedEntities ->
                    let doc : ResourceIdentifierCollectionDocument = {
                      jsonapi = Skip
                      links = Skip
                      meta = Skip
                      data = relatedEntities |> List.map (fun e ->
                        let b = resolveEntity e
                        { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                      )
                    }
                    let handler =
                      setStatusCode 200
                      >=> this.modifyGetSelfResponse ctx entity relatedEntities
                      >=> jsonApiWithETag<'ctx> doc
                    return! handler next httpCtx
            }
            |> Job.startAsTask
      )

    member this.PostSelf =
      this.ModifySelfHandler this.add this.modifyPostSelfOkResponse this.modifyPostSelfAcceptedResponse

    member this.PatchSelf =
      this.ModifySelfHandler this.setAll this.modifyPatchSelfOkResponse this.modifyPatchSelfAcceptedResponse
      |> Option.map (fun patch -> fun ctx req _ entity -> patch ctx req entity)
      
    member this.DeleteSelf =
      this.ModifySelfHandler this.remove this.modifyDeleteSelfOkResponse this.modifyDeleteSelfAcceptedResponse


  member this.Name = this.name

  member this.GetJobSkip(get: Func<'ctx, 'entity, Job<'relatedEntity list Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity list Skippable>>) =
    this.GetJobSkip(Job.liftAsyncFunc2 get)

  member this.GetJob (get: Func<'ctx, 'entity, Job<'relatedEntity list>>) =
    this.GetJobSkip(fun ctx r -> get.Invoke(ctx, r) |> Job.map Include)

  member this.GetJob (get: Func<'entity, Job<'relatedEntity list>>) =
    this.GetJobSkip(fun _ r -> get.Invoke r |> Job.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity list>>) =
    this.GetJob(Job.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity list>>) =
    this.GetJob(Job.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity list>>) =
    this.GetJobSkip(Job.liftFunc2 get)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity list>) =
    this.GetJob(Job.liftFunc2 get)

  member this.Get (get: Func<'entity, 'relatedEntity list>) =
    this.GetJob(Job.liftFunc get)

  member private this.SetAllJobRes (setAll: Func<'ctx, Pointer, 'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with setAll = Some (fun ctx ptr relIds e -> setAll.Invoke(ctx, ptr, relIds, e)) }

  member this.SetAllJobRes (setAll: Func<'ctx, 'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetAllJobRes(fun ctx pointer relIds e -> setAll.Invoke(ctx, relIds, e) |> JobResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetAllJobRes (setAll: Func<'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetAllJobRes(fun _ ids e -> setAll.Invoke(ids, e))

  member this.SetAllJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetAllJobRes(this.toIdSetter getRelated (fun ctx rels e -> setAll.Invoke(ctx, rels, e)))

  member this.SetAllJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Job<Result<'entity, Error list>>>) =
    this.SetAllJobRes(this.toIdSetter getRelated (fun _ ids e -> setAll.Invoke(ids, e)))

  member this.SetAllAsyncRes (setAll: Func<'ctx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllJobRes(Job.liftAsyncFunc3 setAll)

  member this.SetAllAsyncRes (setAll: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllJobRes(Job.liftAsyncFunc2 setAll)

  member this.SetAllAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllJobRes(getRelated, Job.liftAsyncFunc3 setAll)

  member this.SetAllAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllJobRes(getRelated, Job.liftAsyncFunc2 setAll)

  member this.SetAllJob (setAll: Func<'ctx, 'relatedId list, 'entity, Job<'entity>>) =
    this.SetAllJobRes(fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.SetAllJob (setAll: Func<'relatedId list, 'entity, Job<'entity>>) =
    this.SetAllJobRes(fun _ related entity -> setAll.Invoke(related, entity) |> Job.map Ok)

  member this.SetAllJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Job<'entity>>) =
    this.SetAllJobRes(getRelated, fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.SetAllJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Job<'entity>>) =
    this.SetAllJobRes(getRelated, fun _ related entity -> setAll.Invoke(related, entity) |> Job.map Ok)

  member this.SetAllAsync (setAll: Func<'ctx, 'relatedId list, 'entity, Async<'entity>>) =
    this.SetAllJob(Job.liftAsyncFunc3 setAll)

  member this.SetAllAsync (setAll: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.SetAllJob(Job.liftAsyncFunc2 setAll)

  member this.SetAllAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Async<'entity>>) =
    this.SetAllJob(getRelated, Job.liftAsyncFunc3 setAll)

  member this.SetAllAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Async<'entity>>) =
    this.SetAllJob(getRelated, Job.liftAsyncFunc2 setAll)

  member this.SetAllRes (setAll: Func<'ctx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.SetAllJobRes(Job.liftFunc3 setAll)

  member this.SetAllRes (setAll: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.SetAllJobRes(Job.liftFunc2 setAll)

  member this.SetAllRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.SetAllJobRes(getRelated, Job.liftFunc3 setAll)

  member this.SetAllRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.SetAllJobRes(getRelated, Job.liftFunc2 setAll)

  member this.SetAll (setAll: Func<'ctx, 'relatedId list, 'entity, 'entity>) =
    this.SetAllJobRes(JobResult.liftFunc3 setAll)

  member this.SetAll (setAll: Func<'relatedId list, 'entity, 'entity>) =
    this.SetAllJobRes(JobResult.liftFunc2 setAll)

  member this.SetAll (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, 'entity>) =
    this.SetAllJobRes(getRelated, JobResult.liftFunc3 setAll)

  member this.SetAll (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, 'entity>) =
    this.SetAllJobRes(getRelated, JobResult.liftFunc2 setAll)

  member private this.AddJobRes (add: Func<'ctx, Pointer, 'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwith "Can only add setter if the polymorphic resource definition contains ID parsers."
    if this.get.IsNone then
      failwith "Can only add POST to relationship if it contains a getter."
    { this with add = Some (fun ctx ptr relIds e -> add.Invoke(ctx, ptr, relIds, e)) }

  member this.AddJobRes (add: Func<'ctx, 'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    this.AddJobRes(fun ctx pointer relIds e -> add.Invoke(ctx, relIds, e) |> JobResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.AddJobRes (add: Func<'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    this.AddJobRes(fun _ ids e -> add.Invoke(ids, e))

  member this.AddJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Job<Result<'entity, Error list>>>) =
    this.AddJobRes(this.toIdSetter getRelated (fun ctx rels e -> add.Invoke(ctx, rels, e)))

  member this.AddJobRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Job<Result<'entity, Error list>>>) =
    this.AddJobRes(this.toIdSetter getRelated (fun _ ids e -> add.Invoke(ids, e)))

  member this.AddAsyncRes (add: Func<'ctx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddJobRes(Job.liftAsyncFunc3 add)

  member this.AddAsyncRes (add: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddJobRes(Job.liftAsyncFunc2 add)

  member this.AddAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddJobRes(getRelated, Job.liftAsyncFunc3 add)

  member this.AddAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddJobRes(getRelated, Job.liftAsyncFunc2 add)

  member this.AddJob (add: Func<'ctx, 'relatedId list, 'entity, Job<'entity>>) =
    this.AddJobRes(fun ctx related entity -> add.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.AddJob (add: Func<'relatedId list, 'entity, Job<'entity>>) =
    this.AddJobRes(fun _ related entity -> add.Invoke(related, entity) |> Job.map Ok)

  member this.AddJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Job<'entity>>) =
    this.AddJobRes(getRelated, fun ctx related entity -> add.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.AddJob (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Job<'entity>>) =
    this.AddJobRes(getRelated, fun _ related entity -> add.Invoke(related, entity) |> Job.map Ok)

  member this.AddAsync (add: Func<'ctx, 'relatedId list, 'entity, Async<'entity>>) =
    this.AddJob(Job.liftAsyncFunc3 add)

  member this.AddAsync (add: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.AddJob(Job.liftAsyncFunc2 add)

  member this.AddAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Async<'entity>>) =
    this.AddJob(getRelated, Job.liftAsyncFunc3 add)

  member this.AddAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Async<'entity>>) =
    this.AddJob(getRelated, Job.liftAsyncFunc2 add)

  member this.AddRes (add: Func<'ctx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.AddJobRes(Job.liftFunc3 add)

  member this.AddRes (add: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.AddJobRes(Job.liftFunc2 add)

  member this.AddRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.AddJobRes(getRelated, Job.liftFunc3 add)

  member this.AddRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.AddJobRes(getRelated, Job.liftFunc2 add)

  member this.Add (add: Func<'ctx, 'relatedId list, 'entity, 'entity>) =
    this.AddJobRes(JobResult.liftFunc3 add)

  member this.Add (add: Func<'relatedId list, 'entity, 'entity>) =
    this.AddJobRes(JobResult.liftFunc2 add)

  member this.Add (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, 'entity>) =
    this.AddJobRes(getRelated, JobResult.liftFunc3 add)

  member this.Add (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, 'entity>) =
    this.AddJobRes(getRelated, JobResult.liftFunc2 add)

  member private this.RemoveJobRes (remove: Func<'ctx, Pointer, 'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    if this.get.IsNone then
      failwith "Can only add DELETE to relationship if it contains a getter."
    { this with remove = Some (fun ctx ptr relIds e -> remove.Invoke(ctx, ptr, relIds, e)) }

  member this.RemoveJobRes (remove: Func<'ctx, 'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    this.RemoveJobRes(fun ctx pointer relIds e -> remove.Invoke(ctx, relIds, e) |> JobResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.RemoveJobRes (remove: Func<'relatedId list, 'entity, Job<Result<'entity, Error list>>>) =
    this.RemoveJobRes(fun _ ids e -> remove.Invoke(ids, e))

  member this.RemoveAsyncRes (remove: Func<'ctx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveJobRes(Job.liftAsyncFunc3 remove)

  member this.RemoveAsyncRes (remove: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveJobRes(Job.liftAsyncFunc2 remove)

  member this.RemoveJob (remove: Func<'ctx, 'relatedId list, 'entity, Job<'entity>>) =
    this.RemoveJobRes(fun ctx related entity -> remove.Invoke(ctx, related, entity) |> Job.map Ok)

  member this.RemoveJob (remove: Func<'relatedId list, 'entity, Job<'entity>>) =
    this.RemoveJobRes(fun _ related entity -> remove.Invoke(related, entity) |> Job.map Ok)

  member this.RemoveAsync (remove: Func<'ctx, 'relatedId list, 'entity, Async<'entity>>) =
    this.RemoveJob(Job.liftAsyncFunc3 remove)

  member this.RemoveAsync (remove: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.RemoveJob(Job.liftAsyncFunc2 remove)

  member this.RemoveRes (remove: Func<'ctx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.RemoveJobRes(Job.liftFunc3 remove)

  member this.RemoveRes (remove: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.RemoveJobRes(Job.liftFunc2 remove)

  member this.Remove (remove: Func<'ctx, 'relatedId list, 'entity, 'entity>) =
    this.RemoveJobRes(JobResult.liftFunc3 remove)

  member this.Remove (remove: Func<'relatedId list, 'entity, 'entity>) =
    this.RemoveJobRes(JobResult.liftFunc2 remove)

  member this.AddConstraintsJob(getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            job {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsJob(Job.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsJob(Job.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsJob(fun ctx e -> [name, box (getValue ctx e)] |> Job.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.AddConstraint (name: string, value: 'a) =
    this.AddConstraint(name, fun _ -> value)

  member this.ModifyGetRelatedResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyGetRelatedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyGetRelatedResponse(f: 'ctx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyGetRelatedResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetRelatedResponse(handler: HttpHandler) =
    this.ModifyGetRelatedResponse(fun _ _ _ -> handler)

  member this.ModifyGetSelfResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyGetSelfResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyGetSelfResponse(f: 'ctx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyGetSelfResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetSelfResponse(handler: HttpHandler) =
    this.ModifyGetSelfResponse(fun _ _ _ -> handler)

  member this.ModifyPostSelfOkResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyPostSelfOkResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPostSelfOkResponse(f: 'ctx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyPostSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPostSelfOkResponse(handler: HttpHandler) =
    this.ModifyPostSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPostSelfAcceptedResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyPostSelfAcceptedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPostSelfAcceptedResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyPostSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPostSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPostSelfAcceptedResponse(fun _ _ -> handler)

  member this.ModifyPatchSelfOkResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyPatchSelfOkResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPatchSelfOkResponse(f: 'ctx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyPatchSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPatchSelfOkResponse(handler: HttpHandler) =
    this.ModifyPatchSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfAcceptedResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyPatchSelfAcceptedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyPatchSelfAcceptedResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyPatchSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPatchSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPatchSelfAcceptedResponse(fun _ _ -> handler)

  member this.ModifyDeleteSelfOkResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyDeleteSelfOkResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyDeleteSelfOkResponse(f: 'ctx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyDeleteSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyDeleteSelfOkResponse(handler: HttpHandler) =
    this.ModifyDeleteSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyDeleteSelfAcceptedResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyDeleteSelfAcceptedResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyDeleteSelfAcceptedResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyDeleteSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyDeleteSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyDeleteSelfAcceptedResponse(fun _ _ -> handler)

  member this.BeforeModifySelfJobRes(f: Func<'ctx, 'entity, Job<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfJobRes(f: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> JobResult.map (fun () -> e))

  member this.BeforeModifySelfJobRes(f: Func<'entity, Job<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfJobRes(f: Func<'entity, Job<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfJobRes(Job.liftAsyncFunc f)

  member this.BeforeModifySelfJob(f: Func<'ctx, 'entity, Job<'entity>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'ctx, 'entity, Job<unit>>) =
    this.BeforeModifySelfJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'entity, Job<'entity>>) =
    this.BeforeModifySelfJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeModifySelfJob(f: Func<'entity, Job<unit>>) =
    this.BeforeModifySelfJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfJob(Job.liftAsyncFunc f)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfJobRes(Job.liftFunc f)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, unit>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc f)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfJobRes(JobResult.liftFunc f)

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> 'entity -> Job<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew |> JobResult.map (fun () -> eNew)) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx _ e -> f ctx e) }

  member this.AfterModifySelfJobRes(f: 'ctx -> 'entity -> Job<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> JobResult.map (fun () -> e))

  member this.AfterModifySelfJobRes(f: 'entity -> Job<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(fun _ e -> f e)

  member this.AfterModifySelfJobRes(f: 'entity -> Job<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(fun _ e -> f e |> JobResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfJobRes(Job.liftAsync f)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun ctx eOld eNew -> f ctx eOld eNew |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun ctx eOld eNew -> f ctx eOld eNew |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'ctx -> 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun ctx e -> f ctx e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'entity -> Job<'entity>) =
    this.AfterModifySelfJobRes(fun e -> f e |> Job.map Ok)

  member this.AfterModifySelfJob(f: 'entity -> Job<unit>) =
    this.AfterModifySelfJobRes(fun e -> f e |> Job.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfJob(Job.liftAsync f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfJob(Job.liftAsync f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift3 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift3 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift2 f)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfJobRes(Job.lift f)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfJobRes(Job.lift f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift3 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift3 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift2 f)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfJobRes(JobResult.lift f)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfJobRes(JobResult.lift f)

  member this.ModifySelfReturn202Accepted () =
    { this with modifySelfReturn202Accepted = true }



type PolymorphicRelationshipHelper<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Job<Result<'relatedId, Error list>>> option
} with

  static member internal Create () : PolymorphicRelationshipHelper<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    { resolveEntity = None; idParsers = None }

  member this.AddIdParser(resDef: ResourceDefinition<'ctx, 'e, 'relatedId>) =
    { this with
        idParsers =
          this.idParsers
          |> Option.defaultValue Map.empty
          |> Map.add resDef.name resDef.id.toDomain
          |> Some
    }

  member this.AddIdParser(resDef: ResourceDefinition<'ctx, 'e, 'id>, mapId: 'id -> 'relatedId) =
    { this with
        idParsers =
          this.idParsers
          |> Option.defaultValue Map.empty
          |> Map.add resDef.name (fun ctx resId ->
              resDef.id.toDomain ctx resId |> JobResult.map mapId)
          |> Some
    }

  member this.ResolveEntity(getPolyBuilder: 'relatedEntity -> PolymorphicBuilder<'ctx>) =
    { this with resolveEntity = Some getPolyBuilder }

  member this.ToOne([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    ToOneRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(name, this.resolveEntity, this.idParsers)

  member this.ToOneNullable([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    ToOneNullableRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(name, this.resolveEntity, this.idParsers)

  member this.ToMany([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    ToManyRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(name, this.resolveEntity, this.idParsers)



type RelationshipHelper<'ctx, 'entity> internal () =

  member _.Polymorphic () = PolymorphicRelationshipHelper<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create()

  member _.ToOne(resourceDef: ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let idParsers = Map.empty |> Map.add resourceDef.name resourceDef.id.toDomain
    let resolveEntity = resourceDef.PolymorphicFor
    ToOneRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(name, Some resolveEntity, Some idParsers)

  member _.ToOneNullable(resourceDef: ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let idParsers = Map.empty |> Map.add resourceDef.name resourceDef.id.toDomain
    let resolveEntity = resourceDef.PolymorphicFor
    ToOneNullableRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(name, Some resolveEntity, Some idParsers)

  member _.ToMany(resourceDef: ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let idParsers = Map.empty |> Map.add resourceDef.name resourceDef.id.toDomain
    let resolveEntity = resourceDef.PolymorphicFor
    ToManyRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(name, Some resolveEntity, Some idParsers)
