namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.AspNetCore.Http
open Microsoft.Net.Http.Headers
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Errors


type internal Preconditions<'ctx> =
  abstract Validate: HttpContext -> 'ctx -> BoxedEntity -> Result<unit, Error list>


type Preconditions<'ctx, 'entity> = internal {
  getETag: 'ctx -> 'entity -> EntityTagHeaderValue option
  getLastModified: 'ctx -> 'entity -> DateTimeOffset option
  isOptional: bool
} with

  static member internal Create () : Preconditions<'ctx, 'entity> = {
    getETag = fun _ _ -> None
    getLastModified = fun _ _ -> None
    isOptional = false
  }


  interface Preconditions<'ctx> with
    member this.Validate httpCtx ctx entity =
      let eTag = this.getETag ctx (unbox<'entity> entity)
      let lastModified = this.getLastModified ctx (unbox<'entity> entity)

      // TODO: Simplify after https://github.com/giraffe-fsharp/Giraffe/issues/402 is fixed
      let preconditionsDefined = eTag.IsSome || lastModified.IsSome
      let preconditionsSupplied =
        httpCtx.TryGetRequestHeader "If-Match" |> Option.isSome
        || httpCtx.TryGetRequestHeader "If-Unmodified-Since" |> Option.isSome
      if not preconditionsDefined then Ok ()
      elif not preconditionsSupplied && this.isOptional then Ok ()
      elif not preconditionsSupplied then Error [preconditionRequired eTag.IsSome lastModified.IsSome]
      else
        let res = httpCtx.ValidatePreconditions eTag lastModified
        // Clear headers because response-level ETag/Last-Modified headers don't
        // necessarily make sense in JSON:API due to compound documents; these values
        // should be communicated as attributes or meta.
        httpCtx.Response.Headers.Remove "ETag" |> ignore
        httpCtx.Response.Headers.Remove "Last-Modified" |> ignore
        if res = ConditionFailed then Error [preconditionFailed eTag.IsSome lastModified.IsSome]
        else Ok ()

  member this.ETag(getETag: Func<'ctx, 'entity, EntityTagHeaderValue>) =
    { this with getETag = fun ctx e -> getETag.Invoke(ctx, e) |> Some }

  member this.ETag(getETag: Func<'entity, EntityTagHeaderValue>) =
    this.ETag(fun _ e -> getETag.Invoke e)

  member this.LastModified(getLastModified: Func<'ctx, 'entity, DateTimeOffset>) =
    { this with getLastModified = fun ctx e -> getLastModified.Invoke(ctx, e) |> Some }

  member this.LastModified(getLastModified: Func<'entity, DateTimeOffset>) =
    this.LastModified(fun _ e -> getLastModified.Invoke e)

  member this.Optional =
    { this with isOptional = true }


type internal ResSpecificResourceLookup<'ctx> =
  abstract GetByIdBoxed: 'ctx -> ResourceId -> Async<Result<(ResourceDefinition<'ctx> * BoxedEntity) option, Error list>>


type ResourceLookup<'ctx, 'entity, 'id> =
  abstract GetById: 'ctx -> 'id -> Async<Result<'entity option, Error list>>


type internal ResourceLookup<'ctx> =
  abstract GetByIdBoxed: ResourceDefinition<'ctx> -> 'ctx -> ResourceId -> Async<Result<BoxedEntity option, Error list>>


type ResourceLookup<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  getById: 'ctx -> 'id -> Async<Result<'entity option, Error list>>
} with

  static member internal Create(mapCtx, getById) : ResourceLookup<'originalCtx, 'ctx, 'entity, 'id> =
    {
      mapCtx = mapCtx
      getById = getById
    }

  interface ResourceLookup<'ctx, 'entity, 'id> with
    member this.GetById ctx id = this.getById ctx id

  interface ResourceLookup<'originalCtx> with
    member this.GetByIdBoxed rDef ctx rawId =
      async {
        match! rDef.ParseIdBoxed ctx rawId with
        // Ignore ID parsing errors; in the context of fetching a resource by ID, this
        // just means that the resource does not exist, which is a more helpful result.
        | Error _ -> return Ok None
        | Ok domainId ->
            match! this.mapCtx ctx with
            | Error errs -> return Error errs
            | Ok mappedCtx ->
                let! entity = domainId |> unbox<'id> |> this.getById mappedCtx
                return entity |> Result.map (Option.map box)
      }



type internal PolymorphicResourceLookup<'ctx> =
  abstract GetByIdBoxed: ResourceDefinition<'ctx> -> 'ctx -> ResourceId -> Async<Result<(ResourceDefinition<'ctx> * BoxedEntity) option, Error list>>


type PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  getById: 'ctx -> 'id -> Async<Result<'entity option, Error list>>
  getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
} with

  static member internal Create(mapCtx, getById, getPolyBuilder) : PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id> =
    {
      mapCtx = mapCtx
      getById = getById
      getPolyBuilder = getPolyBuilder
    }

  interface ResourceLookup<'ctx, 'entity, 'id> with
    member this.GetById ctx id = this.getById ctx id

  interface PolymorphicResourceLookup<'originalCtx> with
    member this.GetByIdBoxed rDef ctx rawId =
      async {
        match! rDef.ParseIdBoxed ctx rawId with
        // Ignore ID parsing errors; in the context of fetching a resource by ID, this
        // just means that the resource does not exist, which is a more helpful result
        | Error _ -> return Ok None
        | Ok domainId ->
            match! this.mapCtx ctx with
            | Error errs -> return Error errs
            | Ok mappedCtx ->
                let! entity = domainId |> unbox<'id> |> this.getById mappedCtx
                return entity |> Result.map (Option.map (fun e ->
                  let b = this.getPolyBuilder e
                  b.resourceDef, b.entity
                ))
      }



type internal GetResourceOperation<'ctx> =
  abstract Run: ResourceDefinition<'ctx> -> 'ctx -> Request -> BoxedEntity -> ResponseBuilder<'ctx> -> HttpHandler


type GetResourceOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  modifyResponse: 'ctx -> 'entity -> HttpHandler
} with

  static member internal Create(mapCtx) : GetResourceOperation<'originalCtx, 'ctx, 'entity, 'id> =
    {
      mapCtx = mapCtx
      modifyResponse = fun _ _ -> fun next ctx -> next ctx
    }

  interface GetResourceOperation<'originalCtx> with
    member this.Run resDef ctx req entity resp =
      fun next httpCtx ->
        task {
          match! this.mapCtx ctx with
          | Error errors -> return! handleErrors errors next httpCtx
          | Ok mappedCtx ->
              let! doc = resp.Write ctx req (resDef, entity)
              let handler =
                setStatusCode 200
                >=> this.modifyResponse mappedCtx (unbox<'entity> entity)
                >=> jsonApiWithETag doc
              return! handler next httpCtx
        }

  member this.ModifyResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ _ -> handler)



type internal GetCollectionOperation<'ctx> =
  abstract Run: ResourceDefinition<'ctx> -> 'ctx -> Request -> ResponseBuilder<'ctx> -> HttpHandler


type GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  getCollection: 'ctx -> Request -> Async<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity list, Error list>>
  modifyResponse: 'ctx -> 'entity list -> HttpHandler
} with

  static member internal Create(mapCtx, getCollection) : GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> =
    {
      mapCtx = mapCtx
      getCollection = getCollection
      modifyResponse = fun _ _ -> fun next ctx -> next ctx
    }

  interface GetCollectionOperation<'originalCtx> with
    member this.Run resDef ctx req resp =
      fun next httpCtx ->
        task {
          match! this.mapCtx ctx with
          | Error errors -> return! handleErrors errors next httpCtx
          | Ok mappedCtx ->
              match! this.getCollection mappedCtx req with
              | Error errors -> return! handleErrors errors next httpCtx
              | Ok (_, queryNames, entities) ->
                  match httpCtx.TryGetQueryStringValue "sort", queryNames.Contains "sort" with
                  | Some _, false -> return! handleErrors [sortNotSupported] next httpCtx
                  | _ -> 
                      let! doc = resp.WriteList ctx req (entities |> List.map (fun e -> resDef, e))
                      let handler =
                        setStatusCode 200
                        >=> this.modifyResponse mappedCtx entities
                        >=> jsonApiWithETag doc
                      return! handler next httpCtx
        }

  member this.ModifyResponse(getHandler: 'ctx -> 'entity list -> HttpHandler) =
    { this with modifyResponse = fun ctx es -> getHandler ctx es }

  member this.ModifyResponse(f: 'ctx -> 'entity list -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx es -> (fun next httpCtx -> f ctx es httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ _ -> handler)


type internal PolymorphicGetCollectionOperation<'ctx> =
  abstract Run: 'ctx -> Request -> ResponseBuilder<'ctx> -> HttpHandler


type PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  getCollection: 'ctx -> Request -> Async<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity list, Error list>>
  getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
  modifyResponse: 'ctx -> 'entity list -> HttpHandler
} with

  static member internal Create(mapCtx, getCollection, getPolyBuilder) : PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> =
    {
      mapCtx = mapCtx
      getCollection = getCollection
      getPolyBuilder = getPolyBuilder
      modifyResponse = fun _ _ -> fun next ctx -> next ctx
    }

  interface PolymorphicGetCollectionOperation<'originalCtx> with
    member this.Run ctx req resp =
      fun next httpCtx ->
        task {
          match! this.mapCtx ctx with
          | Error errors -> return! handleErrors errors next httpCtx
          | Ok mappedCtx ->
              match! this.getCollection mappedCtx req with
              | Error errors -> return! handleErrors errors next httpCtx
              | Ok (_, queryNames, entities) ->
                  match httpCtx.TryGetQueryStringValue "sort", queryNames.Contains "sort" with
                  | Some _, false -> return! handleErrors [sortNotSupported] next httpCtx
                  | _ -> 
                      let! doc = resp.WriteList ctx req (entities |> List.map this.getPolyBuilder |> List.map (fun b -> b.resourceDef, b.entity))
                      let handler =
                        setStatusCode 200
                        >=> this.modifyResponse mappedCtx entities
                        >=> jsonApiWithETag doc
                      return! handler next httpCtx
        }

  member this.ModifyResponse(getHandler: 'ctx -> 'entity list -> HttpHandler) =
    { this with modifyResponse = fun ctx es -> getHandler ctx es }

  member this.ModifyResponse(f: 'ctx -> 'entity list -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx es -> (fun next httpCtx -> f ctx es httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ _ -> handler)


type internal PostOperation<'ctx> =
  abstract Run: CollectionName -> ResourceDefinition<'ctx> -> 'ctx -> Request -> BoxedPatcher<'ctx> -> ResponseBuilder<'ctx> -> HttpHandler
  abstract HasPersist: bool


type PostOperation<'originalCtx, 'ctx, 'entity> = internal {
  mapCtx: 'originalCtx -> Request -> Async<Result<'ctx, Error list>>
  create: 'ctx -> Request -> Async<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity, Error list>>
  afterCreate: ('ctx -> 'entity -> Async<Result<'entity, Error list>>) option
  modifyResponse: 'ctx -> 'entity -> HttpHandler
  return202Accepted: bool
} with

  static member internal Create(mapCtx, create) : PostOperation<'originalCtx, 'ctx, 'entity> =
    {
      mapCtx = mapCtx
      create = create
      afterCreate = None
      modifyResponse = fun _ _ -> fun next ctx -> next ctx
      return202Accepted = false
    }

  interface PostOperation<'originalCtx> with
    member this.HasPersist = this.afterCreate.IsSome
    member this.Run collName rDef ctx req patch resp =
      fun next httpCtx ->
        let afterCreate =
          this.afterCreate
          |> Option.defaultWith (fun () -> failwithf "Framework bug: POST operation defined without AfterCreate. This should be caught at startup.")
        task {
          match! this.mapCtx ctx req with
          | Error errors -> return! handleErrors errors next httpCtx
          | Ok mappedCtx ->
              match! this.create mappedCtx req |> AsyncResult.bind (fun (fieldNames, _, e) -> box e |> patch ctx req fieldNames |> AsyncResult.map (fun e -> fieldNames, e)) with
              | Error errors -> return! handleErrors errors next httpCtx
              | Ok (ns, entity0) ->
                  match req.Document.Value with
                  | Ok (Some { data = Some { id = Include _ } }) when not <| ns.Contains "id" ->
                      return! handleErrors [collPostClientIdNotAllowed collName rDef.TypeName] next httpCtx
                  | _ ->
                      match! afterCreate mappedCtx (unbox<'entity> entity0) with
                      | Error errors -> return! handleErrors errors next httpCtx
                      | Ok entity1 ->
                          if this.return202Accepted then
                            let handler =
                              setStatusCode 202
                              >=> this.modifyResponse mappedCtx (unbox<'entity> entity1)
                            return! handler next httpCtx
                          else
                            let! doc = resp.Write ctx req (rDef, entity1)
                            let setLocationHeader =
                              match doc with
                              | { data = Some { links = Include links } } ->
                                  match links.TryGetValue "self" with
                                  | true, { href = Some url } -> setHttpHeader "Location" (url.ToString())
                                  | _ -> fun next ctx -> next ctx
                              | _ -> fun next ctx -> next ctx
                            let handler =
                              setStatusCode 201
                              >=> setLocationHeader
                              >=> this.modifyResponse mappedCtx entity1
                              >=> jsonApiWithETag doc
                            return! handler next httpCtx
        }

  member this.AfterCreateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    { this with afterCreate = Some (fun ctx e -> f.Invoke(ctx, e)) }

  member this.AfterCreateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.AfterCreateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> AsyncResult.map (fun () -> e))

  member this.AfterCreateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.AfterCreateAsyncRes(fun _ e -> f.Invoke e)

  member this.AfterCreateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.AfterCreateAsyncRes(fun _ e -> f.Invoke e |> AsyncResult.map (fun () -> e))

  member this.AfterCreateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.AfterCreateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.AfterCreateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.AfterCreateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.AfterCreateAsync(f: Func<'entity, Async<'entity>>) =
    this.AfterCreateAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.AfterCreateAsync(f: Func<'entity, Async<unit>>) =
    this.AfterCreateAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.AfterCreateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.AfterCreateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.AfterCreateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.AfterCreateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.AfterCreateRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.AfterCreateAsyncRes(fun e -> f.Invoke e |> async.Return)

  member this.AfterCreateRes(f: Func<'entity, Result<unit, Error list>>) =
    this.AfterCreateAsyncRes(fun e -> f.Invoke e |> async.Return)

  member this.AfterCreate(f: Func<'ctx, 'entity, 'entity>) =
    this.AfterCreateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.AfterCreate(f: Func<'ctx, 'entity, unit>) =
    this.AfterCreateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.AfterCreate(f: Func<'entity, 'entity>) =
    this.AfterCreateAsyncRes(fun e -> f.Invoke e |> Ok |> async.Return)

  member this.AfterCreate(f: Func<'entity, unit>) =
    this.AfterCreateAsyncRes(fun e -> f.Invoke e |> Ok |> async.Return)

  member this.Return202Accepted () =
    { this with return202Accepted = true }

  member this.ModifyResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ _ -> handler)


type internal PatchOperation<'ctx> =
  abstract HasPersist: bool
  abstract Run: ResourceDefinition<'ctx> -> 'ctx -> Request -> Preconditions<'ctx> -> BoxedEntity -> BoxedPatcher<'ctx> -> ResponseBuilder<'ctx> -> HttpHandler


type PatchOperation<'originalCtx, 'ctx, 'entity> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  beforeUpdate: 'ctx -> 'entity -> Async<Result<'entity, Error list>>
  afterUpdate: ('ctx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) option
  modifyResponse: 'ctx -> 'entity -> HttpHandler
  return202Accepted: bool
} with

  static member internal Create (mapCtx) : PatchOperation<'originalCtx, 'ctx, 'entity> =
    {
      mapCtx = mapCtx
      beforeUpdate = fun _ x -> Ok x |> async.Return
      afterUpdate = None
      modifyResponse = fun _ _ -> fun next ctx -> next ctx
      return202Accepted = false
    }


  interface PatchOperation<'originalCtx> with
    member this.HasPersist = this.afterUpdate.IsSome
    member this.Run rDef ctx req preconditions entity0 patch resp =
      let afterUpdate =
        this.afterUpdate
          |> Option.defaultWith (fun () -> failwithf "Framework bug: PATCH operation defined without AfterUpdate. This should be caught at startup.")
      fun next httpCtx ->
        task {
          let errs = [
            match req.Document.Value with
            | Error errs ->
                yield! errs
            | Ok None -> resPatchMissingResourceObject ""
            | Ok (Some { data = None }) -> resPatchMissingResourceObject "/data"
            | Ok (Some { data = Some res }) ->
                if res.``type`` <> rDef.TypeName then
                  resPatchTypeMismatch res.``type`` rDef.TypeName |> Error.setSourcePointer "/data/type"
                match res.id with
                | Skip -> resPatchMissingResourceId "/data"
                | Include id when id <> rDef.GetIdBoxed entity0 ->
                    resPatchIdMismatch id (rDef.GetIdBoxed entity0) |> Error.setSourcePointer "/data/id"
                | Include _ -> ()
          ]

          if not errs.IsEmpty then
            return! handleErrors errs next httpCtx
          else
              match! this.mapCtx ctx with
              | Error errors -> return! handleErrors errors next httpCtx
              | Ok mappedCtx ->
                  match preconditions.Validate httpCtx ctx entity0 with
                  | Error errors -> return! handleErrors errors next httpCtx
                  | Ok () ->
                      match! this.beforeUpdate mappedCtx (unbox<'entity> entity0) with
                      | Error errors -> return! handleErrors errors next httpCtx
                      | Ok entity1 ->
                          match! patch ctx req Set.empty entity1 with
                          | Error errors -> return! handleErrors errors next httpCtx
                          | Ok entity2 ->
                              match! afterUpdate mappedCtx (unbox<'entity> entity0) (unbox<'entity> entity2) with
                              | Error errors -> return! handleErrors errors next httpCtx
                              | Ok entity3 ->
                                  if this.return202Accepted then
                                    let handler =
                                      setStatusCode 202
                                      >=> this.modifyResponse mappedCtx (unbox<'entity> entity3)
                                    return! handler next httpCtx
                                  else
                                    let! doc = resp.Write ctx req (rDef, entity3)
                                    let handler =
                                      setStatusCode 200
                                      >=> this.modifyResponse mappedCtx entity3
                                      >=> jsonApiWithETag doc
                                    return! handler next httpCtx
        }


  member this.BeforeUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    { this with beforeUpdate = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> AsyncResult.map (fun () -> e))

  member this.BeforeUpdateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeUpdateAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeUpdateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeUpdateAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeUpdateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeUpdateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeUpdateAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeUpdateAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeUpdateAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeUpdateAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeUpdateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeUpdateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeUpdateRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeUpdateAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeUpdateRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeUpdateAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeUpdate(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeUpdate(f: Func<'ctx, 'entity, unit>) =
    this.BeforeUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeUpdate(f: Func<'entity, 'entity>) =
    this.BeforeUpdateAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.BeforeUpdate(f: Func<'entity, unit>) =
    this.BeforeUpdateAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, 'entity, Async<Result<'entity, Error list>>>) =
    { this with afterUpdate = Some (fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew)) }

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, 'entity, Async<Result<unit, Error list>>>) =
    this.AfterUpdateAsyncRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> AsyncResult.map (fun () -> eNew))

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.AfterUpdateAsyncRes(fun ctx _ eNew -> f.Invoke(ctx, eNew))

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.AfterUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> AsyncResult.map (fun () -> e))

  member this.AfterUpdateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.AfterUpdateAsyncRes(fun _ e -> f.Invoke e)

  member this.AfterUpdateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.AfterUpdateAsyncRes(fun _ e -> f.Invoke e |> AsyncResult.map (fun () -> e))

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, 'entity, Async<'entity>>) =
    this.AfterUpdateAsyncRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Async.map Ok)

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, 'entity, Async<unit>>) =
    this.AfterUpdateAsyncRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Async.map Ok)

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.AfterUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.AfterUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.AfterUpdateAsync(f: Func<'entity, Async<'entity>>) =
    this.AfterUpdateAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.AfterUpdateAsync(f: Func<'entity, Async<unit>>) =
    this.AfterUpdateAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, 'entity, Result<'entity, Error list>>) =
    this.AfterUpdateAsyncRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> async.Return)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, 'entity, Result<unit, Error list>>) =
    this.AfterUpdateAsyncRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> async.Return)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.AfterUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.AfterUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.AfterUpdateRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.AfterUpdateAsyncRes(fun e -> f.Invoke e |> async.Return)

  member this.AfterUpdateRes(f: Func<'entity, Result<unit, Error list>>) =
    this.AfterUpdateAsyncRes(fun e -> f.Invoke e |> async.Return)

  member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity, 'entity>) =
    this.AfterUpdateAsyncRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Ok |> async.Return)

  member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity, unit>) =
    this.AfterUpdateAsyncRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Ok |> async.Return)

  member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity>) =
    this.AfterUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.AfterUpdate(f: Func<'ctx, 'entity, unit>) =
    this.AfterUpdateAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.AfterUpdate(f: Func<'entity, 'entity>) =
    this.AfterUpdateAsyncRes(fun e -> f.Invoke e |> Ok |> async.Return)

  member this.AfterUpdate(f: Func<'entity, unit>) =
    this.AfterUpdateAsyncRes(fun e -> f.Invoke e |> Ok |> async.Return)

  member this.Return202Accepted () =
    { this with return202Accepted = true }

  member this.ModifyResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyResponse = getHandler }

  member this.ModifyResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ _ -> handler)




type internal DeleteOperation<'ctx> =
  abstract Run: ResourceDefinition<'ctx> -> 'ctx -> Request -> Preconditions<'ctx> -> BoxedEntity -> ResponseBuilder<'ctx> -> HttpHandler



type DeleteOperation<'originalCtx, 'ctx, 'entity> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  beforeDelete: 'ctx -> 'entity -> Async<Result<'entity, Error list>>
  delete: 'ctx -> Request -> 'entity -> Async<Result<unit, Error list>>
  modifyResponse: 'ctx -> HttpHandler
  return202Accepted: bool
} with

  static member internal Create(mapCtx, delete) : DeleteOperation<'originalCtx, 'ctx, 'entity> =
    {
      mapCtx = mapCtx
      beforeDelete = fun _ x -> Ok x |> async.Return
      delete = delete
      modifyResponse = fun _ -> fun next ctx -> next ctx
      return202Accepted = false
    }


  interface DeleteOperation<'originalCtx> with
    member this.Run resDef ctx req preconditions entity0 resp =
      fun next httpCtx ->
        task {
          match! this.mapCtx ctx with
          | Error errors -> return! handleErrors errors next httpCtx
          | Ok mappedCtx ->
              match preconditions.Validate httpCtx ctx entity0 with
              | Error errors -> return! handleErrors errors next httpCtx
              | Ok () ->
                  match! this.beforeDelete mappedCtx (unbox<'entity> entity0) with
                  | Error errors -> return! handleErrors errors next httpCtx
                  | Ok entity1 ->
                      match! this.delete mappedCtx req (unbox<'entity> entity1) with
                      | Error errors -> return! handleErrors errors next httpCtx
                      | Ok () ->
                          if this.return202Accepted then
                            let handler =
                              setStatusCode 202
                              >=> this.modifyResponse mappedCtx
                            return! handler next httpCtx
                          else
                            let handler =
                              setStatusCode 204
                              >=> this.modifyResponse mappedCtx
                            return! handler next httpCtx
        }


  member this.BeforeDeleteAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    { this with beforeDelete = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeDeleteAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeDeleteAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> AsyncResult.map (fun () -> e))

  member this.BeforeDeleteAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeDeleteAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeDeleteAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeDeleteAsyncRes(fun _ e -> f.Invoke e)

  member this.BeforeDeleteAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeDeleteAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeDeleteAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeDeleteAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Async.map Ok)

  member this.BeforeDeleteAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeDeleteAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeDeleteAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeDeleteAsyncRes(fun e -> f.Invoke e |> Async.map Ok)

  member this.BeforeDeleteRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeDeleteAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeDeleteRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeDeleteAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> async.Return)

  member this.BeforeDeleteRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeDeleteAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeDeleteRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeDeleteAsyncRes(fun _ e -> f.Invoke e |> async.Return)

  member this.BeforeDelete(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeDeleteAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeDelete(f: Func<'ctx, 'entity, unit>) =
    this.BeforeDeleteAsyncRes(fun ctx e -> f.Invoke(ctx, e) |> Ok |> async.Return)

  member this.BeforeDelete(f: Func<'entity, 'entity>) =
    this.BeforeDeleteAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.BeforeDelete(f: Func<'entity, unit>) =
    this.BeforeDeleteAsyncRes(fun _ e -> f.Invoke e |> Ok |> async.Return)

  member this.ModifyResponse(getHandler: 'ctx -> HttpHandler) =
    { this with modifyResponse = fun ctx -> getHandler ctx }

  member this.ModifyResponse(f: 'ctx -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx -> (fun next httpCtx -> f ctx httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ -> handler)

  member this.Return202Accepted () =
    { this with return202Accepted = true }



type internal CustomOperation<'ctx> =
  abstract Name: LinkName
  abstract HrefAndMeta: 'ctx -> Uri -> BoxedEntity -> Async<(Uri option * Map<string, obj> option)>
  abstract Get: ('ctx -> Request -> Responder<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract Post: ('ctx -> Request -> Responder<'ctx> -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract Patch: ('ctx -> Request -> Responder<'ctx> -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract Delete: ('ctx -> Request -> Responder<'ctx> -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option



type CustomOperation<'originalCtx, 'ctx, 'entity> = internal {
  mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>
  name: string
  getMeta: ('ctx -> 'entity -> Map<string, obj>) option
  condition: 'ctx -> 'entity -> Async<Result<unit, Error list>>
  get: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Async<Result<HttpHandler, Error list>>) option
  post: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Async<Result<HttpHandler, Error list>>) option
  patch: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Async<Result<HttpHandler, Error list>>) option
  delete: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Async<Result<HttpHandler, Error list>>) option
} with

  static member internal Create(mapCtx, name) : CustomOperation<'originalCtx, 'ctx, 'entity> =
    {
      mapCtx = mapCtx
      name = name
      getMeta = None
      condition = fun _ _ -> Ok () |> async.Return
      get = None
      post = None
      patch = None
      delete = None
    }


  member private this.handler operation ctx req responder (preconditions: Preconditions<'originalCtx>) (entity: obj) =
    fun next httpCtx ->
      task {
        match! this.mapCtx ctx with
        | Error errors -> return! handleErrors errors next httpCtx
        | Ok mappedCtx ->
            match! this.condition mappedCtx (unbox<'entity> entity) with
            | Error errors -> return! handleErrors errors next httpCtx
            | Ok () ->
                match preconditions.Validate httpCtx ctx entity with
                | Error errors -> return! handleErrors errors next httpCtx
                | Ok () ->
                    match! operation mappedCtx req responder (unbox<'entity> entity) with
                    | Ok handler -> return! handler next httpCtx
                    | Error errors -> return! handleErrors errors next httpCtx
      }


  interface CustomOperation<'originalCtx> with
    member this.Name = this.name

    member this.HrefAndMeta ctx selfUrl entity =
      async {
        if this.get.IsNone && this.post.IsNone && this.patch.IsNone && this.delete.IsNone then
          return None, None
        else
          match! this.mapCtx ctx with
          | Error _ -> return None, None
          | Ok mappedCtx ->
              let meta =
                this.getMeta
                |> Option.map (fun getMeta ->
                    getMeta mappedCtx (unbox<'entity> entity)
                )
                |> Option.filter (not << Map.isEmpty)

              match! this.condition mappedCtx (unbox<'entity> entity) with
              | Ok () -> return Some (selfUrl |> Uri.addSegment this.name), meta
              | Error _ -> return None, meta
      }
      

    member this.Get =
      let prec = { new Preconditions<'originalCtx> with member _.Validate _ _ _ = Ok () }
      this.get |> Option.map (fun get ->
        fun ctx req resp e -> this.handler get ctx req resp prec e
      )

    member this.Post = this.post |> Option.map this.handler

    member this.Patch = this.patch |> Option.map this.handler

    member this.Delete = this.delete |> Option.map this.handler


  member this.ConditionAsyncRes(predicate: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    { this with condition = fun ctx e -> predicate.Invoke(ctx, e) }

  member this.ConditionAsyncRes(predicate: Func<'entity, Async<Result<unit, Error list>>>) =
    this.ConditionAsyncRes(fun _ e -> predicate.Invoke e)

  member this.ConditionRes(predicate: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.ConditionAsyncRes(fun ctx e -> predicate.Invoke(ctx, e) |> async.Return)

  member this.ConditionRes(predicate: Func<'entity, Result<unit, Error list>>) =
    this.ConditionAsyncRes(fun _ e -> predicate.Invoke e |> async.Return)

  member this.Condition(predicate: Func<'ctx, 'entity, bool>) =
    this.ConditionAsyncRes(fun ctx e -> (if predicate.Invoke(ctx, e) then Ok () else Error [customOpConditionFalse]) |> async.Return)

  member this.Condition(predicate: Func<'entity, bool>) =
    this.ConditionAsyncRes(fun _ e -> (if predicate.Invoke e then Ok () else Error [customOpConditionFalse]) |> async.Return)

  member this.AddMeta(key: string, getValue: 'ctx -> 'entity -> 'a, ?condition: 'ctx -> 'entity -> bool) =
    let condition = defaultArg condition (fun _ _ -> true)
    let getMeta = this.getMeta |> Option.defaultValue (fun _ _ -> Map.empty)
    { this with getMeta = Some (fun ctx e -> if condition ctx e then getMeta ctx e |> Map.add key (getValue ctx e |> box) else getMeta ctx e) }

  member this.AddMeta(key: string, getValue: 'entity -> 'a, ?condition: 'entity -> bool) =
    let condition = defaultArg condition (fun _ -> true)
    this.AddMeta(key, (fun _ e -> getValue e), fun _ e -> condition e)

  member this.AddMetaOpt(key: string, getValue: 'ctx -> 'entity -> 'a option) =
    let getMeta = this.getMeta |> Option.defaultValue (fun _ _ -> Map.empty)
    this.AddMeta(key, fun ctx e -> match getValue ctx e with None -> getMeta ctx e | Some m -> getMeta ctx e |> Map.add key (box m))

  member this.AddMetaOpt(key: string, getValue: 'entity -> 'a option) =
    this.AddMetaOpt(key, fun _ e -> getValue e)

  member this.Get(get: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    { this with get = Some (fun ctx req responder entity -> get.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }

  member this.Post(post: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    { this with post = Some (fun ctx req responder entity -> post.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }

  member this.Patch(patch: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    { this with patch = Some (fun ctx req responder entity -> patch.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }

  member this.Delete(delete: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    { this with delete = Some (fun ctx req responder entity -> delete.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }



type PolymorphicOperationHelper<'originalCtx, 'ctx, 'entity, 'id> internal (mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>) =

  member _.LookupAsyncRes(getById: Func<'ctx, 'id, Async<Result<'entity option, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, (fun ctx id -> getById.Invoke(ctx, id)), getPolyBuilder)

  member this.LookupAsyncRes(getById: Func<'id, Async<Result<'entity option, Error list>>>, getPolyBuilder: 'entity  -> PolymorphicBuilder<'originalCtx>) =
    this.LookupAsyncRes((fun _ id -> getById.Invoke id), getPolyBuilder)

  member this.LookupAsync(getById: Func<'ctx, 'id, Async<'entity option>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupAsyncRes((fun ctx id -> getById.Invoke(ctx, id) |> Async.map Ok), getPolyBuilder)

  member this.LookupAsync(getById: Func<'id, Async<'entity option>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupAsyncRes((fun _ id -> getById.Invoke id |> Async.map Ok), getPolyBuilder)

  member this.LookupRes(getById: Func<'ctx, 'id, Result<'entity option, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupAsyncRes((fun ctx id -> getById.Invoke(ctx, id) |> async.Return), getPolyBuilder)

  member this.LookupRes(getById: Func<'id, Result<'entity option, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupAsyncRes((fun _ id -> getById.Invoke id |> async.Return), getPolyBuilder)

  member this.Lookup(getById: Func<'ctx, 'id, 'entity option>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupAsyncRes((fun ctx id -> getById.Invoke(ctx, id) |> Ok |> async.Return), getPolyBuilder)

  member this.Lookup(getById: Func<'id, 'entity option>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupAsyncRes((fun _ id -> getById.Invoke id |> Ok |> async.Return), getPolyBuilder)

  member _.GetCollectionAsyncRes(getCollection: Func<'ctx, Async<Result<'entity list, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, (fun ctx req -> getCollection.Invoke ctx |> AsyncResult.map (fun xs -> Set.empty, Set.empty, xs)), getPolyBuilder)

  member this.GetCollectionAsyncRes(getCollection: Func<unit, Async<Result<'entity list, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes((fun (_: 'ctx) -> getCollection.Invoke ()), getPolyBuilder)

  member _.GetCollectionAsyncRes(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity list>, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(
      mapCtx,
      (fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req))
        |> AsyncResult.bind (fun p -> p.ParseWithConsumed ())
      ),
      getPolyBuilder
    )

  member this.GetCollectionAsync(getCollection: Func<'ctx, Async<'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Async.map Ok, getPolyBuilder)

  member this.GetCollectionAsync(getCollection: Func<unit, Async<'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Async.map Ok, getPolyBuilder)

  member this.GetCollectionAsync(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes((fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Async.map Ok), getPolyBuilder)

  member this.GetCollectionRes(getCollection: Func<'ctx, Result<'entity list, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> async.Return, getPolyBuilder)

  member this.GetCollectionRes(getCollection: Func<unit, Result<'entity list, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> async.Return, getPolyBuilder)

  member this.GetCollectionRes(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity list>, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes((fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> async.Return), getPolyBuilder)

  member this.GetCollection(getCollection: Func<'ctx, 'entity list>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Ok >> async.Return, getPolyBuilder)

  member this.GetCollection(getCollection: Func<unit, 'entity list>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Ok >> async.Return, getPolyBuilder)

  member this.GetCollection(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionAsyncRes((fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Ok |> async.Return), getPolyBuilder)



type OperationHelper<'originalCtx, 'ctx, 'entity, 'id> internal (mapCtx: 'originalCtx -> Async<Result<'ctx, Error list>>) =

  member _.Polymorphic = PolymorphicOperationHelper<'originalCtx, 'ctx, 'entity, 'id>(mapCtx)

  member _.ForContextAsyncRes (mapCtx: 'originalCtx -> Async<Result<'mappedCtx, Error list>>) =
    OperationHelper<'originalCtx, 'mappedCtx, 'entity, 'id>(mapCtx)

  member this.ForContextAsyncOpt (mapCtx: 'originalCtx -> Async<'mappedCtx option>) =
    this.ForContextAsyncRes(mapCtx >> Async.map (Result.requireSome [opMapCtxFailedNone]))

  member this.ForContextAsync (mapCtx: 'originalCtx -> Async<'mappedCtx>) =
    this.ForContextAsyncRes(mapCtx >> Async.map Ok)

  member this.ForContextRes (mapCtx: 'originalCtx -> Result<'mappedCtx, Error list>) =
    this.ForContextAsyncRes(mapCtx >> async.Return)

  member this.ForContextOpt (mapCtx: 'originalCtx -> 'mappedCtx option) =
    this.ForContextAsyncRes(mapCtx >> Result.requireSome [opMapCtxFailedNone] >> async.Return)

  member this.ForContext (mapCtx: 'originalCtx -> 'mappedCtx) =
    this.ForContextAsyncRes(mapCtx >> Ok >> async.Return)

  member _.LookupAsyncRes (getById: Func<'ctx, 'id, Async<Result<'entity option, Error list>>>) =
    ResourceLookup<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, fun ctx id -> getById.Invoke(ctx, id))

  member this.LookupAsyncRes (getById: Func<'id, Async<Result<'entity option, Error list>>>) =
    this.LookupAsyncRes(fun _ id -> getById.Invoke id)

  member this.LookupAsync (getById: Func<'ctx, 'id, Async<'entity option>>) =
    this.LookupAsyncRes(fun ctx id -> getById.Invoke(ctx, id) |> Async.map Ok)

  member this.LookupAsync (getById: Func<'id, Async<'entity option>>) =
    this.LookupAsyncRes(fun _ id -> getById.Invoke id |> Async.map Ok)

  member this.LookupRes (getById: Func<'ctx, 'id, Result<'entity option, Error list>>) =
    this.LookupAsyncRes(fun ctx id -> getById.Invoke(ctx, id) |> async.Return)

  member this.LookupRes (getById: Func<'id, Result<'entity option, Error list>>) =
    this.LookupAsyncRes(fun _ id -> getById.Invoke id |> async.Return)

  member this.Lookup (getById: Func<'ctx, 'id, 'entity option>) =
    this.LookupAsyncRes(fun ctx id -> getById.Invoke(ctx, id) |> Ok |> async.Return)

  member this.Lookup (getById: Func<'id, 'entity option>) =
    this.LookupAsyncRes(fun _ id -> getById.Invoke id |> Ok |> async.Return)

  member _.GetResource () =
    GetResourceOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx)

  member _.GetCollectionAsyncRes (getCollection: Func<'ctx, Async<Result<'entity list, Error list>>>) =
    GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, fun ctx req -> getCollection.Invoke ctx |> AsyncResult.map (fun xs -> Set.empty, Set.empty, xs))

  member this.GetCollectionAsyncRes (getCollection: Func<unit, Async<Result<'entity list, Error list>>>) =
    this.GetCollectionAsyncRes(fun (_: 'ctx) -> getCollection.Invoke ())

  member _.GetCollectionAsyncRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity list>, Error list>>>) =
    GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(
      mapCtx,
      fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req))
        |> AsyncResult.bind (fun p -> p.ParseWithConsumed ())
    )

  member this.GetCollectionAsync (getCollection: Func<'ctx, Async<'entity list>>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Async.map Ok)

  member this.GetCollectionAsync (getCollection: Func<unit, Async<'entity list>>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Async.map Ok)

  member this.GetCollectionAsync (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity list>>>) =
    this.GetCollectionAsyncRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Async.map Ok)

  member this.GetCollectionRes (getCollection: Func<'ctx, Result<'entity list, Error list>>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> async.Return)

  member this.GetCollectionRes (getCollection: Func<unit, Result<'entity list, Error list>>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> async.Return)

  member this.GetCollectionRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity list>, Error list>>) =
    this.GetCollectionAsyncRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> async.Return)

  member this.GetCollection (getCollection: Func<'ctx, 'entity list>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Ok >> async.Return)

  member this.GetCollection (getCollection: Func<unit, 'entity list>) =
    this.GetCollectionAsyncRes(getCollection.Invoke >> Ok >> async.Return)

  member this.GetCollection (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity list>>) =
    this.GetCollectionAsyncRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Ok |> async.Return)

  member _.PostAsyncRes (createEntity: Func<'ctx, Async<Result<'entity, Error list>>>) =
    PostOperation<'originalCtx, 'ctx, 'entity>.Create((fun ctx res -> mapCtx ctx), fun ctx res -> createEntity.Invoke ctx |> AsyncResult.map (fun e -> Set.empty, Set.empty, e))

  member this.PostAsyncRes (createEntity: Func<unit, Async<Result<'entity, Error list>>>) =
    this.PostAsyncRes(fun (ctx: 'ctx) -> createEntity.Invoke ())

  member _.PostAsyncRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    PostOperation<'originalCtx, 'ctx, 'entity>.Create(
      (fun ctx _ -> mapCtx ctx),
      fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req))
        |> AsyncResult.bind (fun p -> p.ParseWithConsumed ())
    )

  member this.PostAsync (createEntity: Func<'ctx, Async<'entity>>) =
    this.PostAsyncRes(createEntity.Invoke >> Async.map Ok)

  member this.PostAsync (createEntity: Func<unit, Async<'entity>>) =
    this.PostAsyncRes(createEntity.Invoke >> Async.map Ok)

  member this.PostAsync (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity>>>) =
    this.PostAsyncRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Async.map Ok)

  member this.PostRes (createEntity: Func<'ctx, Result<'entity, Error list>>) =
    this.PostAsyncRes(createEntity.Invoke >> async.Return)

  member this.PostRes (createEntity: Func<unit, Result<'entity, Error list>>) =
    this.PostAsyncRes(createEntity.Invoke >> async.Return)

  member this.PostRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity>, Error list>>) =
    this.PostAsyncRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> async.Return)

  member this.Post (createEntity: Func<'ctx, 'entity>) =
    this.PostAsyncRes(createEntity.Invoke >> Ok >> async.Return)

  member this.Post (createEntity: Func<unit, 'entity>) =
    this.PostAsyncRes(createEntity.Invoke >> Ok >> async.Return)

  member this.Post (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity>>) =
    this.PostAsyncRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Ok |> async.Return)

  member _.PostBackRefAsyncRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Async<Result<'entity, Error list>>>) =
    let mapCtxWithBackRef ctx req =
      mapCtx ctx
      |> AsyncResult.bind (fun mappedCtx ->
          backRef.Get(ctx, req)
          |> AsyncResult.map (fun e -> mappedCtx, e)
      )
    let consumedFieldNames = match backRef.FieldName with None -> Set.empty | Some fn -> Set.empty.Add fn
    PostOperation<'originalCtx, 'ctx * 'backRefEntity, 'entity>.Create(mapCtxWithBackRef, fun ctx res -> createEntity.Invoke ctx |> AsyncResult.map (fun e -> consumedFieldNames, Set.empty, e))

  member this.PostBackRefAsyncRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Async<Result<'entity, Error list>>>) =
    this.PostBackRefAsyncRes(backRef, fun (ctx: 'ctx, br: 'backRefEntity) -> createEntity.Invoke ())

  member _.PostBackRefAsyncRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    let mapCtxWithBackRef ctx req =
      mapCtx ctx
      |> AsyncResult.bind (fun mappedCtx ->
          backRef.Get(ctx, req)
          |> AsyncResult.map (fun e -> mappedCtx, e)
      )
    let addBackRefFieldName = match backRef.FieldName with None -> id | Some fn -> Set.add fn
    PostOperation<'originalCtx, 'ctx * 'backRefEntity, 'entity>.Create(
      mapCtxWithBackRef,
      fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(fst ctx, req))
        |> AsyncResult.bind (fun p -> p.ParseWithConsumed ())
        |> AsyncResult.map (fun (fns, qns, e) -> addBackRefFieldName fns, qns, e)
    )

  member this.PostBackRefAsync (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Async<'entity>>) =
    this.PostBackRefAsyncRes(backRef, createEntity.Invoke >> Async.map Ok)

  member this.PostBackRefAsync (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Async<'entity>>) =
    this.PostBackRefAsyncRes(backRef, createEntity.Invoke >> Async.map Ok)

  member this.PostBackRefAsync (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity>>>) =
    this.PostBackRefAsyncRes(backRef, fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Async.map Ok)

  member this.PostBackRefRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Result<'entity, Error list>>) =
    this.PostBackRefAsyncRes(backRef, createEntity.Invoke >> async.Return)

  member this.PostBackRefRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Result<'entity, Error list>>) =
    this.PostBackRefAsyncRes(backRef, createEntity.Invoke >> async.Return)

  member this.PostBackRefRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity>, Error list>>) =
    this.PostBackRefAsyncRes(backRef, fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> async.Return)

  member this.PostBackRef (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, 'entity>) =
    this.PostBackRefAsyncRes(backRef, createEntity.Invoke >> Ok >> async.Return)

  member this.PostBackRef (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, 'entity>) =
    this.PostBackRefAsyncRes(backRef, createEntity.Invoke >> Ok >> async.Return)

  member this.PostBackRef (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity>>) =
    this.PostBackRefAsyncRes(backRef, fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Ok |> async.Return)

  member _.Patch() =
    PatchOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx)

  member _.DeleteAsyncRes(delete: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    DeleteOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx, fun ctx _ entity -> delete.Invoke(ctx, entity))

  member this.DeleteAsyncRes(delete: Func<'entity, Async<Result<unit, Error list>>>) =
    this.DeleteAsyncRes(fun _ e -> delete.Invoke e)

  member _.DeleteAsyncRes(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, unit>, Error list>>>) =
    DeleteOperation<'originalCtx, 'ctx, 'entity>.Create(
      mapCtx,
      fun ctx req entity ->
        getRequestParser.Invoke(ctx, entity, RequestParserHelper<'ctx>(ctx, req))
        |> AsyncResult.bind (fun p -> p.Parse ())
    )

  member this.DeleteAsyncRes(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, unit>, Error list>>>) =
    this.DeleteAsyncRes(fun ctx e parse -> getRequestParser.Invoke(e, parse))

  member this.DeleteAsync(delete: Func<'ctx, 'entity, Async<unit>>) =
    this.DeleteAsyncRes(fun ctx e -> delete.Invoke(ctx, e) |> Async.map Ok)

  member this.DeleteAsync(delete: Func<'entity, Async<unit>>) =
    this.DeleteAsyncRes(fun _ e -> delete.Invoke e |> Async.map Ok)

  member this.DeleteAsync(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, unit>>>) =
    this.DeleteAsyncRes(fun ctx e parse -> getRequestParser.Invoke(ctx, e, parse) |> Async.map Ok)

  member this.DeleteAsync(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, unit>>>) =
    this.DeleteAsyncRes(fun ctx e parse -> getRequestParser.Invoke(e, parse) |> Async.map Ok)

  member this.DeleteRes(delete: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.DeleteAsyncRes(fun ctx e -> delete.Invoke(ctx, e) |> async.Return)

  member this.DeleteRes(delete: Func<'entity, Result<unit, Error list>>) =
    this.DeleteAsyncRes(fun _ e -> delete.Invoke e |> async.Return)

  member this.DeleteRes(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, unit>, Error list>>) =
    this.DeleteAsyncRes(fun ctx e parse -> getRequestParser.Invoke(ctx, e, parse) |> async.Return)

  member this.DeleteRes(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, unit>, Error list>>) =
    this.DeleteAsyncRes(fun ctx e parse -> getRequestParser.Invoke(e, parse) |> async.Return)

  member this.Delete(delete: Func<'ctx, 'entity, unit>) =
    this.DeleteAsyncRes(fun ctx e -> delete.Invoke(ctx, e) |> Ok |> async.Return)

  member this.Delete(delete: Func<'entity, unit>) =
    this.DeleteAsyncRes(fun _ e -> delete.Invoke e |> Ok |> async.Return)

  member this.Delete(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, RequestParser<'ctx, unit>>) =
    this.DeleteAsyncRes(fun ctx e parse -> getRequestParser.Invoke(ctx, e, parse) |> Ok |> async.Return)

  member this.Delete(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, RequestParser<'ctx, unit>>) =
    this.DeleteAsyncRes(fun ctx e parse -> getRequestParser.Invoke(e, parse) |> Ok |> async.Return)

  member _.CustomLink([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    CustomOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx, name)
