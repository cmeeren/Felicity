namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Errors



type RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId> =
  abstract Name: string
  abstract IdToDomain: 'ctx -> string -> Async<Result<'relatedId, Error list>>


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
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Async<Skippable<ResourceDefinition<'ctx> * BoxedEntity>>) option


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
        member _.Get(ctx, req) =
          this.idGetter.Get(ctx, req)
          |> AsyncResult.bind (
              Option.traverseAsyncResult (
                this.getRelated.GetById ctx
                >> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                >> AsyncResult.requireSome [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")]
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      this.Optional.Get(ctx, req)

  interface RequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      let pointer =
        match req.Document.Value with
        | Error _ -> ""  // Won't be used
        | Ok None -> ""
        | Ok (Some { data = None }) -> ""
        | Ok (Some { data = Some { relationships = Skip } }) -> "/data"
        | Ok (Some { data = Some { relationships = Include _ } }) -> "/data/relationships"
      this.Optional.Get(ctx, req)
      |> AsyncResult.requireSome [reqParserMissingRequiredRel this.name pointer]


type ToOneRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Async<Result<'relatedId, Error list>>> option
  get: ('ctx -> 'entity -> Async<'relatedEntity Skippable>) option
  set: ('ctx -> Pointer -> 'relatedId -> 'entity -> Async<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>
  beforeModifySelf: 'ctx -> 'entity -> Async<Result<'entity, Error list>>
  afterModifySelf: ('ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) option
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
      getConstraints = fun _ _ -> async.Return []
      beforeModifySelf = fun _ e -> Ok e |> async.Return
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
      |> AsyncResult.mapError (List.map (Error.setSourcePointer dataPointer))
      |> AsyncResult.requireSome [relatedResourceNotFound dataPointer]
      |> AsyncResult.bind (fun r ->
          entitySetter ctx r entity
          |> AsyncResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwithf "Attempted to parse resource ID for polymorphic relationship '%s', but no ID parsers have been specified." this.name)
          match req.Document.Value with
          | Error errs -> Error errs |> async.Return
          | Ok (Some { data = Some { relationships = Include rels } }) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOne as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name ("/data/relationships/" + this.name)] |> async.Return
                  | { data = Include identifier } ->
                      match idParsers.TryGetValue identifier.``type`` with
                      | false, _ ->
                          let allowedTypes = idParsers |> Map.toList |> List.map fst
                          let pointer = "/data/relationships/" + this.name + "/data/type"
                          Error [relInvalidType this.name identifier.``type`` allowedTypes pointer] |> async.Return
                      | true, parseId ->
                          parseId ctx identifier.id
                          // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                          // this just means that the resource does not exist, which is a more helpful result.
                          |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                          |> AsyncResult.map Some
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOne>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> async.Return
          | _ -> None |> Ok |> async.Return
    }


  interface OptionalRequestGetter<'ctx, 'relatedId> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      this.Optional.Get(ctx, req)

  interface RequestGetter<'ctx, 'relatedId> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      let pointer =
        match req.Document.Value with
        | Error _ -> ""  // Won't be used
        | Ok None -> ""
        | Ok (Some { data = None }) -> ""
        | Ok (Some { data = Some { relationships = Skip } }) -> "/data"
        | Ok (Some { data = Some { relationships = Include _ } }) -> "/data/relationships"
      this.Optional.Get(ctx, req)
      |> AsyncResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors req =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          [reqParserProhibitedRel this.name ("/data/relationships/" + this.name)]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      async {
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
                          |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                          |> AsyncResult.bind (fun domain ->
                              set ctx ("/data/relationships/" + this.name + "/data") domain (unbox<'entity> entity))
                          |> AsyncResult.map box<'entity>
            | Some _, Some rel -> return failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOne>.FullName (rel.GetType().FullName)
        | _ -> return Ok entity  // no attributes provided
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
          getRelated ctx (unbox<'entity> entity) |> Async.map (Skippable.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          ))
      )

  
  member this.Related (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) =
    ToOneRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(this.name, this.Optional, getRelated)


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
            task {
              let entity = unbox<'entity> entity
              match! getRelated ctx entity with
              | Skip -> return! handleErrors [getRelWhileSkip] next httpCtx
              | Include relatedEntity ->
                  let b = resolveEntity relatedEntity
                  let! doc = resp.Write ctx req (b.resourceDef, b.entity)
                  let handler =
                    setStatusCode 200
                    >=> this.modifyGetRelatedResponse ctx entity relatedEntity
                    >=> jsonApiWithETag doc
                  return! handler next httpCtx
            }
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity ->
          fun next httpCtx ->
            task {
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
                      >=> jsonApiWithETag doc
                    return! handler next httpCtx
            }
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
            task {
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
                                  |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data")])
                                  |> AsyncResult.bind (fun domain ->
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
                                                >=> jsonApiWithETag doc
                                              return! handler next httpCtx
            }
      )

    member _.DeleteSelf = None


  member this.Name = this.name

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity>>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> Async.map Include)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity>>) =
    this.GetAsyncSkip(fun _ r -> get.Invoke r |> Async.map Include)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity>>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> async.Return)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> async.Return)

  member this.Get (get: Func<'entity, 'relatedEntity>) =
    this.GetAsyncSkip(fun _ r -> get.Invoke r |> Include |> async.Return)

  member private this.SetAsyncRes (set: Func<'ctx, Pointer, 'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with set = Some (fun ctx ptr relId e -> set.Invoke(ctx, ptr, relId, e)) }

  member this.SetAsyncRes (set: Func<'ctx, 'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(fun ctx pointer relId e -> set.Invoke(ctx, relId, e) |> AsyncResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetAsyncRes (set: Func<'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(fun _ id e -> set.Invoke(id, e))

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(this.toIdSetter getRelated (fun ctx relId e -> set.Invoke(ctx, relId, e)))

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(this.toIdSetter getRelated (fun _ id e -> set.Invoke(id, e)))

  member this.SetAsync (set: Func<'ctx, 'relatedId, 'entity, Async<'entity>>) =
    this.SetAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.SetAsync (set: Func<'relatedId, 'entity, Async<'entity>>) =
    this.SetAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> Async.map Ok)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<'entity>>) =
    this.SetAsyncRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Async.map Ok))

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<'entity>>) =
    this.SetAsyncRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Async.map Ok))

  member this.SetRes (set: Func<'ctx, 'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> async.Return)

  member this.SetRes (set: Func<'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> async.Return)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(getRelated, fun ctx related entity -> set.Invoke(ctx, related, entity) |> async.Return)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(getRelated, fun _ related entity -> set.Invoke(related, entity) |> async.Return)

  member this.Set (set: Func<'ctx, 'relatedId, 'entity, 'entity>) =
    this.SetAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Set (set: Func<'relatedId, 'entity, 'entity>) =
    this.SetAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> Ok |> async.Return)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, 'entity>) =
    this.SetAsyncRes(getRelated, fun ctx related entity -> set.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, 'entity>) =
    this.SetAsyncRes(getRelated, fun _ related entity -> set.Invoke(related, entity) |> Ok |> async.Return)

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            async {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsAsync(fun ctx e -> getConstraints ctx e |> async.Return)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsAsync(fun ctx e -> [name, box (getValue ctx e)] |> async.Return)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.AddConstraint (name: string, value: 'a) =
    this.AddConstraint(name, fun _ -> value)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> AsyncResult.map (fun () -> e))

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, unit>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew) }

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew |> AsyncResult.map (fun () -> eNew)) }

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx _ e -> f ctx e) }

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> AsyncResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfAsyncRes(fun _ e -> f e)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun _ e -> f e |> AsyncResult.map (fun () -> e))

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Async.map Ok)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> async.Return)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> async.Return)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Ok |> async.Return)

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
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Async<Skippable<(ResourceDefinition<'ctx> * BoxedEntity) option>>) option



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
        member _.Get(ctx, req) =
          this.idGetter.Get(ctx, req)
          |> AsyncResult.bind (  // ID did not fail parsing, but may be missing or null
              Option.traverseAsyncResult (  // ID was present, but may be null
                Option.traverseAsyncResult (  // ID was not null
                  this.getRelated.GetById ctx
                  >> AsyncResult.mapError (List.map (Error.setSourcePointer ("/data/relationships/" + this.name + "/data")))
                  >> AsyncResult.requireSome [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")]
                )
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      this.Optional.Get(ctx, req)

  interface RequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      let pointer =
        match req.Document.Value with
        | Error _ -> ""  // Won't be used
        | Ok None -> ""
        | Ok (Some { data = None }) -> ""
        | Ok (Some { data = Some { relationships = Skip } }) -> "/data"
        | Ok (Some { data = Some { relationships = Include _ } }) -> "/data/relationships"
      this.Optional.Get(ctx, req)
      |> AsyncResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToOneNullableRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Async<Result<'relatedId, Error list>>> option
  get: ('ctx -> 'entity -> Async<'relatedEntity option Skippable>) option
  set: ('ctx -> Pointer -> 'relatedId option -> 'entity -> Async<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>
  beforeModifySelf: 'ctx -> 'entity -> Async<Result<'entity, Error list>>
  afterModifySelf: ('ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) option
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
      getConstraints = fun _ _ -> async.Return []
      beforeModifySelf = fun _ e -> Ok e |> async.Return
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
      |> Option.traverseAsyncResult (
          getRelated.GetById ctx
          >> AsyncResult.mapError (List.map (Error.setSourcePointer dataPointer))
          >> AsyncResult.requireSome [relatedResourceNotFound dataPointer]
      )
      |> AsyncResult.bind (fun r ->
        entitySetter ctx r entity
        |> AsyncResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwithf "Attempted to parse resource ID for polymorphic relationship '%s', but no ID parsers have been specified." this.name)
          match req.Document.Value with
          | Error errs -> Error errs |> async.Return
          | Ok (Some { data = Some { relationships = Include rels } }) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOneNullable as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name ("/data/relationships/" + this.name)] |> async.Return
                  | { data = Include identifier } ->
                      identifier
                      |> Option.traverseAsyncResult (fun id ->
                          match idParsers.TryGetValue id.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = "/data/relationships/" + this.name + "/data/type"
                              Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> async.Return
                          | true, parseId ->
                              parseId ctx id.id
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                              |> AsyncResult.map Some
                      )
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOneNullable>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> async.Return
          | _ -> None |> Ok |> async.Return
    }


  interface OptionalRequestGetter<'ctx, 'relatedId option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      this.Optional.Get(ctx, req)

  interface RequestGetter<'ctx, 'relatedId option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      let pointer =
        match req.Document.Value with
        | Error _ -> ""  // Won't be used
        | Ok None -> ""
        | Ok (Some { data = None }) -> ""
        | Ok (Some { data = Some { relationships = Skip } }) -> "/data"
        | Ok (Some { data = Some { relationships = Include _ } }) -> "/data/relationships"
      this.Optional.Get(ctx, req)
      |> AsyncResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors req =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          [reqParserProhibitedRel this.name ("/data/relationships/" + this.name)]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      async {
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
                      |> Option.traverseAsyncResult (fun id ->
                          match idParsers.TryGetValue id.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = "/data/relationships/" + this.name + "/data/type"
                              Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> async.Return
                          | true, parseId ->
                            parseId ctx id.id
                            // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                            // this just means that the resource does not exist, which is a more helpful result.
                            |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data")])
                      )
                      |> AsyncResult.bind (fun domain ->
                          set ctx ("/data/relationships/" + this.name + "/data") domain (unbox<'entity> entity))
                      |> AsyncResult.map box<'entity>
            | Some _, Some rel -> return failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToOneNullable>.FullName (rel.GetType().FullName)
        | _ -> return Ok entity  // no attributes provided
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
          getRelated ctx (unbox<'entity> entity) |> Async.map (Skippable.map (Option.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          )))
      )

  
  member this.Related (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) =
    ToOneNullableRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(this.name, this.Optional, getRelated)


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
            task {
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
                    >=> jsonApiWithETag doc
                  return! handler next httpCtx
            }
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity ->
          fun next httpCtx ->
            task {
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
                      >=> jsonApiWithETag doc
                    return! handler next httpCtx
            }
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
            task {
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
                              |> Option.traverseAsyncResult (fun id ->
                                  match idParsers.TryGetValue id.``type`` with
                                  | false, _ ->
                                      let allowedTypes = idParsers |> Map.toList |> List.map fst
                                      Error [relInvalidTypeSelf id.``type`` allowedTypes "/data/type"] |> async.Return
                                  | true, parseId ->
                                      parseId ctx id.id
                                      // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                      // this just means that the resource does not exist, which is a more helpful result.
                                      |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data")])
                              )
                              |> AsyncResult.bind (fun domain -> set ctx "/data" domain (unbox<'entity> entity1))
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
                                            >=> jsonApiWithETag doc
                                          return! handler next httpCtx
            }
      )

    member _.DeleteSelf = None


  member this.Name = this.name

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity option Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity option>>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> Async.map Include)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity option>>) =
    this.GetAsyncSkip(fun _ r -> get.Invoke r |> Async.map Include)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity option>>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> async.Return)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity option>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> async.Return)

  member this.Get (get: Func<'entity, 'relatedEntity option>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke r |> Include |> async.Return)

  member private this.SetAsyncRes (set: Func<'ctx, Pointer, 'relatedId option, 'entity, Async<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with set = Some (fun ctx ptr relId e -> set.Invoke(ctx, ptr, relId, e)) }

  member this.SetAsyncRes (set: Func<'ctx, 'relatedId option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(fun ctx pointer relId e -> set.Invoke(ctx, relId, e) |> AsyncResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetAsyncRes (set: Func<'relatedId option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(fun _ id e -> set.Invoke(id, e))

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(this.toIdSetter getRelated (fun ctx rel e -> set.Invoke(ctx, rel, e)))

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(this.toIdSetter getRelated (fun _ id e -> set.Invoke(id, e)))

  member this.SetAsync (set: Func<'ctx, 'relatedId option, 'entity, Async<'entity>>) =
    this.SetAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.SetAsync (set: Func<'relatedId option, 'entity, Async<'entity>>) =
    this.SetAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> Async.map Ok)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Async<'entity>>) =
    this.SetAsyncRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Async.map Ok))

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Async<'entity>>) =
    this.SetAsyncRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Async.map Ok))

  member this.SetRes (set: Func<'ctx, 'relatedId option, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> async.Return)

  member this.SetRes (set: Func<'relatedId option, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> async.Return)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(getRelated, fun ctx related entity -> set.Invoke(ctx, related, entity) |> async.Return)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, Result<'entity, Error list>>) =
    this.SetAsyncRes(getRelated, fun _ related entity -> set.Invoke(related, entity) |> async.Return)

  member this.Set (set: Func<'ctx, 'relatedId option, 'entity, 'entity>) =
    this.SetAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Set (set: Func<'relatedId option, 'entity, 'entity>) =
    this.SetAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> Ok |> async.Return)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity option, 'entity, 'entity>) =
    this.SetAsyncRes(getRelated, fun ctx related entity -> set.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Set (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity option, 'entity, 'entity>) =
    this.SetAsyncRes(getRelated, fun _ related entity -> set.Invoke(related, entity) |> Ok |> async.Return)

  member this.SetNonNullAsyncRes (set: Func<'ctx, 'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(fun ctx relId e ->
      relId
      |> Result.requireSome [setRelNullNotAllowed this.name]
      |> async.Return
      |> AsyncResult.bind (fun relId -> set.Invoke(ctx, relId, e))
    )

  member this.SetNonNullAsyncRes (set: Func<'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullAsyncRes(fun _ id e -> set.Invoke(id, e))

  member this.SetNonNullAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAsyncRes(getRelated, (fun ctx rel e ->
      rel
      |> Result.requireSome [setRelNullNotAllowed this.name]
      |> async.Return
      |> AsyncResult.bind (fun rel -> set.Invoke(ctx, rel, e))
    ))

  member this.SetNonNullAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullAsyncRes(getRelated, (fun _ id e -> set.Invoke(id, e)))

  member this.SetNonNullAsync (set: Func<'ctx, 'relatedId, 'entity, Async<'entity>>) =
    this.SetNonNullAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.SetNonNullAsync (set: Func<'relatedId, 'entity, Async<'entity>>) =
    this.SetNonNullAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> Async.map Ok)

  member this.SetNonNullAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Async<'entity>>) =
    this.SetNonNullAsyncRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Async.map Ok))

  member this.SetNonNullAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Async<'entity>>) =
    this.SetNonNullAsyncRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Async.map Ok))

  member this.SetNonNullRes (set: Func<'ctx, 'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> async.Return)

  member this.SetNonNullRes (set: Func<'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> async.Return)

  member this.SetNonNullRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullAsyncRes(getRelated, fun ctx related entity -> set.Invoke(ctx, related, entity) |> async.Return)

  member this.SetNonNullRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullAsyncRes(getRelated, fun _ related entity -> set.Invoke(related, entity) |> async.Return)

  member this.SetNonNull (set: Func<'ctx, 'relatedId, 'entity, 'entity>) =
    this.SetNonNullAsyncRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.SetNonNull (set: Func<'relatedId, 'entity, 'entity>) =
    this.SetNonNullAsyncRes(fun _ related entity -> set.Invoke(related, entity) |> Ok |> async.Return)

  member this.SetNonNull (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'ctx, 'relatedEntity, 'entity, 'entity>) =
    this.SetNonNullAsyncRes(getRelated, fun ctx related entity -> set.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.SetNonNull (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, set: Func<'relatedEntity, 'entity, 'entity>) =
    this.SetNonNullAsyncRes(getRelated, fun _ related entity -> set.Invoke(related, entity) |> Ok |> async.Return)

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            async {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsAsync(fun ctx e -> getConstraints ctx e |> async.Return)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsAsync(fun ctx e -> [name, box (getValue ctx e)] |> async.Return)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.AddConstraint (name: string, value: 'a) =
    this.AddConstraint(name, fun _ -> value)

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> AsyncResult.map (fun () -> e))

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, unit>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew) }

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> AsyncResult.map (fun () -> eNew))

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfAsyncRes(fun ctx _ eNew -> f ctx eNew)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> AsyncResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfAsyncRes(fun _ e -> f e)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun _ e -> f e |> AsyncResult.map (fun () -> e))

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Async.map Ok)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> async.Return)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> async.Return)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Ok |> async.Return)

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
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Async<Skippable<(ResourceDefinition<'ctx> * BoxedEntity) list>>) option


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
        member _.Get(ctx, req) =
          this.idGetter.Get(ctx, req)
          |> AsyncResult.bind (
              Option.traverseAsyncResult (
                List.indexed
                >> List.traverseAsyncResultA (fun (i, id) ->
                    this.getRelated.GetById ctx id
                    |> AsyncResult.mapError (List.map (Error.setSourcePointer ("/data/relationships/" + this.name + "/data/" + string i)))
                    |> AsyncResult.requireSome [relatedResourceNotFound ("/data/relationships/" + this.name + "/data/" + string i)]
                )
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      this.Optional.Get(ctx, req)

  interface RequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      let pointer =
        match req.Document.Value with
        | Error _ -> ""  // Won't be used
        | Ok None -> ""
        | Ok (Some { data = None }) -> ""
        | Ok (Some { data = Some { relationships = Skip } }) -> "/data"
        | Ok (Some { data = Some { relationships = Include _ } }) -> "/data/relationships"
      this.Optional.Get(ctx, req)
      |> AsyncResult.requireSome [reqParserMissingRequiredRel this.name pointer]


type ToManyRelationship<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Async<Result<'relatedId, Error list>>> option
  get: ('ctx -> 'entity -> Async<'relatedEntity list Skippable>) option
  setAll: ('ctx -> Pointer -> 'relatedId list -> 'entity -> Async<Result<'entity, Error list>>) option
  add: ('ctx -> Pointer -> 'relatedId list -> 'entity -> Async<Result<'entity, Error list>>) option
  remove: ('ctx -> Pointer -> 'relatedId list -> 'entity -> Async<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>
  beforeModifySelf: 'ctx -> 'entity -> Async<Result<'entity, Error list>>
  afterModifySelf: ('ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) option
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
      getConstraints = fun _ _ -> async.Return []
      beforeModifySelf = fun _ e -> Ok e |> async.Return
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

  member private _.toIdSetter (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) (entitySetter: 'ctx -> 'relatedEntity list -> 'entity -> Async<Result<'entity, Error list>>) =
    fun ctx (dataPointer: Pointer) relatedIds entity ->
      relatedIds
      |> List.map (getRelated.GetById ctx)
      |> Async.Parallel
      |> Async.map (
          Array.indexed
          >> Array.toList
          >> List.traverseResultA (fun (i, t) ->
              t
              |> Result.mapError (List.map (Error.setSourcePointer (dataPointer + "/" + string i)))
              |> Result.bind (Result.requireSome [relatedResourceNotFound (dataPointer + "/" + string i)])
          )
      )
      |> AsyncResult.bind (fun r ->
          entitySetter ctx r entity
          |> AsyncResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId list option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwithf "Attempted to parse resource ID for polymorphic relationship '%s', but no ID parsers have been specified." this.name)
          match req.Document.Value with
          | Error errs -> Error errs |> async.Return
          | Ok (Some { data = Some { relationships = Include rels } }) ->
              match rels.TryGetValue this.name with
              | true, (:? ToMany as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name ("/data/relationships/" + this.name)] |> async.Return
                  | { data = Include list } ->
                      list
                      |> List.indexed
                      |> List.traverseAsyncResultA (fun (i, identifier) ->
                          match idParsers.TryGetValue identifier.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = "/data/relationships/" + this.name + "/data/" + string i + "/type"
                              Error [relInvalidType this.name identifier.``type`` allowedTypes pointer] |> async.Return
                          | true, parseId ->
                              parseId ctx identifier.id
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data/" + string i)])
                      )
                      |> AsyncResult.map Some
              | true, x -> failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToMany>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> async.Return
          | _ -> None |> Ok |> async.Return
    }

  interface OptionalRequestGetter<'ctx, 'relatedId list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      this.Optional.Get(ctx, req)


  interface RequestGetter<'ctx, 'relatedId list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      let pointer =
        match req.Document.Value with
        | Error _ -> ""  // Won't be used
        | Ok None -> ""
        | Ok (Some { data = None }) -> ""
        | Ok (Some { data = Some { relationships = Skip } }) -> "/data"
        | Ok (Some { data = Some { relationships = Include _ } }) -> "/data/relationships"
      this.Optional.Get(ctx, req)
      |> AsyncResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors req =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          [reqParserProhibitedRel this.name ("/data/relationships/" + this.name)]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      async {
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
                      |> List.traverseAsyncResultA (fun (i, id) ->
                          match idParsers.TryGetValue id.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = "/data/relationships/" + this.name + "/data/" + string i + "/type"
                              Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> async.Return
                          | true, parseId ->
                              parseId ctx id.id
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/relationships/" + this.name + "/data/" + string i)])
                      )
                      |> AsyncResult.bind (fun domain ->
                          set ctx ("/data/relationships/" + this.name + "/data") domain (unbox<'entity> entity)
                      )
                      |> AsyncResult.map box<'entity>
            | Some _, Some rel -> return failwithf "Framework bug: Expected relationship '%s' to be deserialized to %s, but was %s" this.name typeof<ToMany>.FullName (rel.GetType().FullName)
        | _ -> return Ok entity  // no attributes provided
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
          get ctx (unbox<'entity> entity) |> Async.map (Skippable.map (List.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          )))
      )


  member this.Related (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) =
    ToManyRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(this.name, this.Optional, getRelated)


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
          task {
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
                            |> List.traverseAsyncResultA (fun (i, id) ->
                                match idParsers.TryGetValue id.``type`` with
                                | false, _ ->
                                    let allowedTypes = idParsers |> Map.toList |> List.map fst
                                    let pointer = "/data/" + string i + "/type"
                                    Error [relInvalidTypeSelf id.``type`` allowedTypes pointer] |> async.Return
                                | true, parseId ->
                                    parseId ctx id.id
                                    // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                    // this just means that the resource does not exist, which is a more helpful result.
                                    |> AsyncResult.mapError (fun _ -> [relatedResourceNotFound ("/data/" + string i)])
                            )
                            |> AsyncResult.bind (fun domain -> f ctx "/data" domain (unbox<'entity> entity1))
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
                                          >=> jsonApiWithETag doc
                                        return! handler next httpCtx
          }
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
            task {
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
                      >=> jsonApiWithETag doc
                    return! handler next httpCtx
            }
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity ->
          fun next httpCtx ->
            task {
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
                      >=> jsonApiWithETag doc
                    return! handler next httpCtx
            }
      )

    member this.PostSelf =
      this.ModifySelfHandler this.add this.modifyPostSelfOkResponse this.modifyPostSelfAcceptedResponse

    member this.PatchSelf =
      this.ModifySelfHandler this.setAll this.modifyPatchSelfOkResponse this.modifyPatchSelfAcceptedResponse
      |> Option.map (fun patch -> fun ctx req _ entity -> patch ctx req entity)
      
    member this.DeleteSelf =
      this.ModifySelfHandler this.remove this.modifyDeleteSelfOkResponse this.modifyDeleteSelfAcceptedResponse


  member this.Name = this.name

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity list Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity list>>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> Async.map Include)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity list>>) =
    this.GetAsyncSkip(fun _ r -> get.Invoke r |> Async.map Include)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity list>>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> async.Return)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity list>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> async.Return)

  member this.Get (get: Func<'entity, 'relatedEntity list>) =
    this.GetAsyncSkip(fun ctx r -> get.Invoke r |> Include |> async.Return)

  member private this.SetAllAsyncRes (setAll: Func<'ctx, Pointer, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with setAll = Some (fun ctx ptr relIds e -> setAll.Invoke(ctx, ptr, relIds, e)) }

  member this.SetAllAsyncRes (setAll: Func<'ctx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllAsyncRes(fun ctx pointer relIds e -> setAll.Invoke(ctx, relIds, e) |> AsyncResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetAllAsyncRes (setAll: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllAsyncRes(fun _ ids e -> setAll.Invoke(ids, e))

  member this.SetAllAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllAsyncRes(this.toIdSetter getRelated (fun ctx rels e -> setAll.Invoke(ctx, rels, e)))

  member this.SetAllAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllAsyncRes(this.toIdSetter getRelated (fun _ ids e -> setAll.Invoke(ids, e)))

  member this.SetAllAsync (setAll: Func<'ctx, 'relatedId list, 'entity, Async<'entity>>) =
    this.SetAllAsyncRes(fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.SetAllAsync (setAll: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.SetAllAsyncRes(fun _ related entity -> setAll.Invoke(related, entity) |> Async.map Ok)

  member this.SetAllAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Async<'entity>>) =
    this.SetAllAsyncRes(getRelated, fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.SetAllAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Async<'entity>>) =
    this.SetAllAsyncRes(getRelated, fun _ related entity -> setAll.Invoke(related, entity) |> Async.map Ok)

  member this.SetAllRes (setAll: Func<'ctx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.SetAllAsyncRes(fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> async.Return)

  member this.SetAllRes (setAll: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.SetAllAsyncRes(fun _ related entity -> setAll.Invoke(related, entity) |> async.Return)

  member this.SetAllRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.SetAllAsyncRes(getRelated, fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> async.Return)

  member this.SetAllRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.SetAllAsyncRes(getRelated, fun _ related entity -> setAll.Invoke(related, entity) |> async.Return)

  member this.SetAll (setAll: Func<'ctx, 'relatedId list, 'entity, 'entity>) =
    this.SetAllAsyncRes(fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.SetAll (setAll: Func<'relatedId list, 'entity, 'entity>) =
    this.SetAllAsyncRes(fun _ related entity -> setAll.Invoke(related, entity) |> Ok |> async.Return)

  member this.SetAll (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'ctx, 'relatedEntity list, 'entity, 'entity>) =
    this.SetAllAsyncRes(getRelated, fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.SetAll (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, setAll: Func<'relatedEntity list, 'entity, 'entity>) =
    this.SetAllAsyncRes(getRelated, fun _ related entity -> setAll.Invoke(related, entity) |> Ok |> async.Return)

  member private this.AddAsyncRes (add: Func<'ctx, Pointer, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwith "Can only add setter if the polymorphic resource definition contains ID parsers."
    if this.get.IsNone then
      failwith "Can only add POST to relationship if it contains a getter."
    { this with add = Some (fun ctx ptr relIds e -> add.Invoke(ctx, ptr, relIds, e)) }

  member this.AddAsyncRes (add: Func<'ctx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddAsyncRes(fun ctx pointer relIds e -> add.Invoke(ctx, relIds, e) |> AsyncResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.AddAsyncRes (add: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddAsyncRes(fun _ ids e -> add.Invoke(ids, e))

  member this.AddAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddAsyncRes(this.toIdSetter getRelated (fun ctx rels e -> add.Invoke(ctx, rels, e)))

  member this.AddAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddAsyncRes(this.toIdSetter getRelated (fun _ ids e -> add.Invoke(ids, e)))

  member this.AddAsync (add: Func<'ctx, 'relatedId list, 'entity, Async<'entity>>) =
    this.AddAsyncRes(fun ctx related entity -> add.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.AddAsync (add: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.AddAsyncRes(fun _ related entity -> add.Invoke(related, entity) |> Async.map Ok)

  member this.AddAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Async<'entity>>) =
    this.AddAsyncRes(getRelated, fun ctx related entity -> add.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.AddAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Async<'entity>>) =
    this.AddAsyncRes(getRelated, fun _ related entity -> add.Invoke(related, entity) |> Async.map Ok)

  member this.AddRes (add: Func<'ctx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.AddAsyncRes(fun ctx related entity -> add.Invoke(ctx, related, entity) |> async.Return)

  member this.AddRes (add: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.AddAsyncRes(fun _ related entity -> add.Invoke(related, entity) |> async.Return)

  member this.AddRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.AddAsyncRes(getRelated, fun ctx related entity -> add.Invoke(ctx, related, entity) |> async.Return)

  member this.AddRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.AddAsyncRes(getRelated, fun _ related entity -> add.Invoke(related, entity) |> async.Return)

  member this.Add (add: Func<'ctx, 'relatedId list, 'entity, 'entity>) =
    this.AddAsyncRes(fun ctx related entity -> add.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Add (add: Func<'relatedId list, 'entity, 'entity>) =
    this.AddAsyncRes(fun _ related entity -> add.Invoke(related, entity) |> Ok |> async.Return)

  member this.Add (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'ctx, 'relatedEntity list, 'entity, 'entity>) =
    this.AddAsyncRes(getRelated, fun ctx related entity -> add.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Add (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, add: Func<'relatedEntity list, 'entity, 'entity>) =
    this.AddAsyncRes(getRelated, fun _ related entity -> add.Invoke(related, entity) |> Ok |> async.Return)

  member private this.RemoveAsyncRes (remove: Func<'ctx, Pointer, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    if this.get.IsNone then
      failwith "Can only add DELETE to relationship if it contains a getter."
    { this with remove = Some (fun ctx ptr relIds e -> remove.Invoke(ctx, ptr, relIds, e)) }

  member this.RemoveAsyncRes (remove: Func<'ctx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveAsyncRes(fun ctx pointer relIds e -> remove.Invoke(ctx, relIds, e) |> AsyncResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.RemoveAsyncRes (remove: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveAsyncRes(fun _ ids e -> remove.Invoke(ids, e))

  member this.RemoveAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'ctx, 'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveAsyncRes(this.toIdSetter getRelated (fun ctx relIds e -> remove.Invoke(ctx, relIds, e)))

  member this.RemoveAsyncRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'relatedEntity list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveAsyncRes(this.toIdSetter getRelated (fun _ ids e -> remove.Invoke(ids, e)))

  member this.RemoveAsync (remove: Func<'ctx, 'relatedId list, 'entity, Async<'entity>>) =
    this.RemoveAsyncRes(fun ctx related entity -> remove.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.RemoveAsync (remove: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.RemoveAsyncRes(fun _ related entity -> remove.Invoke(related, entity) |> Async.map Ok)

  member this.RemoveAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'ctx, 'relatedEntity list, 'entity, Async<'entity>>) =
    this.RemoveAsyncRes(getRelated, fun ctx related entity -> remove.Invoke(ctx, related, entity) |> Async.map Ok)

  member this.RemoveAsync (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'relatedEntity list, 'entity, Async<'entity>>) =
    this.RemoveAsyncRes(getRelated, fun _ related entity -> remove.Invoke(related, entity) |> Async.map Ok)

  member this.RemoveRes (remove: Func<'ctx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.RemoveAsyncRes(fun ctx related entity -> remove.Invoke(ctx, related, entity) |> async.Return)

  member this.RemoveRes (remove: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.RemoveAsyncRes(fun _ related entity -> remove.Invoke(related, entity) |> async.Return)

  member this.RemoveRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'ctx, 'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.RemoveAsyncRes(getRelated, fun ctx related entity -> remove.Invoke(ctx, related, entity) |> async.Return)

  member this.RemoveRes (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'relatedEntity list, 'entity, Result<'entity, Error list>>) =
    this.RemoveAsyncRes(getRelated, fun _ related entity -> remove.Invoke(related, entity) |> async.Return)

  member this.Remove (remove: Func<'ctx, 'relatedId list, 'entity, 'entity>) =
    this.RemoveAsyncRes(fun ctx related entity -> remove.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Remove (remove: Func<'relatedId list, 'entity, 'entity>) =
    this.RemoveAsyncRes(fun _ related entity -> remove.Invoke(related, entity) |> Ok |> async.Return)

  member this.Remove (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'ctx, 'relatedEntity list, 'entity, 'entity>) =
    this.RemoveAsyncRes(getRelated, fun ctx related entity -> remove.Invoke(ctx, related, entity) |> Ok |> async.Return)

  member this.Remove (getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>, remove: Func<'relatedEntity list, 'entity, 'entity>) =
    this.RemoveAsyncRes(getRelated, fun _ related entity -> remove.Invoke(related, entity) |> Ok |> async.Return)

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            async {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsAsync(fun ctx e -> getConstraints ctx e |> async.Return)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsAsync(fun ctx e -> [name, box (getValue ctx e)] |> async.Return)

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

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> AsyncResult.map (fun () -> e))

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'ctx, 'entity, unit>) =
    this.BeforeModifySelfAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew) }

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> AsyncResult.map (fun () -> eNew))

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfAsyncRes(fun ctx _ eNew -> f ctx eNew)

  member this.AfterModifySelfAsyncRes(f: 'ctx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> AsyncResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfAsyncRes(fun _ e -> f e)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfAsyncRes(fun _ e -> f e |> AsyncResult.map (fun () -> e))

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'ctx -> 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Async.map Ok)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Async.map Ok)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> async.Return)

  member this.AfterModifySelfRes(f: 'ctx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> async.Return)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> async.Return)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun ctx eOld eNew -> f ctx eOld eNew |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'ctx -> 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun ctx e -> f ctx e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Ok |> async.Return)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfAsyncRes(fun e -> f e |> Ok |> async.Return)

  member this.ModifySelfReturn202Accepted () =
    { this with modifySelfReturn202Accepted = true }



type PolymorphicRelationshipHelper<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Async<Result<'relatedId, Error list>>> option
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
              resDef.id.toDomain ctx resId |> AsyncResult.map mapId)
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
