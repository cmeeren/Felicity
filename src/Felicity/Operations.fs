namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open Microsoft.Net.Http.Headers
open Hopac
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
  abstract GetByIdBoxed: 'ctx -> ResourceId -> Job<Result<(ResourceDefinition<'ctx> * BoxedEntity) option, Error list>>


type ResourceLookup<'ctx, 'entity, 'id> =
  abstract GetById: 'ctx -> 'id -> Job<Result<'entity option, Error list>>


type internal ResourceLookup<'ctx> =
  abstract GetByIdBoxed: ResourceDefinition<'ctx> -> 'ctx -> ResourceId -> Job<Result<BoxedEntity option, Error list>>


type ResourceLookup<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
  getById: 'ctx -> 'id -> Job<Result<'entity option, Error list>>
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
      job {
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
  abstract GetByIdBoxed: ResourceDefinition<'ctx> -> 'ctx -> ResourceId -> Job<Result<(ResourceDefinition<'ctx> * BoxedEntity) option, Error list>>


type PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
  getById: 'ctx -> 'id -> Job<Result<'entity option, Error list>>
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
      job {
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
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
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
        job {
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
        |> Job.startAsTask

  member this.ModifyResponse(getHandler: 'ctx -> 'entity -> HttpHandler) =
    { this with modifyResponse = fun ctx entity -> getHandler ctx entity }

  member this.ModifyResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ _ -> handler)



type internal GetCollectionOperation<'ctx> =
  abstract Run: ResourceDefinition<'ctx> -> 'ctx -> Request -> ResponseBuilder<'ctx> -> HttpHandler


type GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
  getCollection: 'ctx -> Request -> Job<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity list, Error list>>
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
        job {
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
        |> Job.startAsTask

  member this.ModifyResponse(getHandler: 'ctx -> 'entity list -> HttpHandler) =
    { this with modifyResponse = fun ctx es -> getHandler ctx es }

  member this.ModifyResponse(f: 'ctx -> 'entity list -> HttpContext -> unit) =
    this.ModifyResponse(fun ctx es -> (fun next httpCtx -> f ctx es httpCtx; next httpCtx))

  member this.ModifyResponse(handler: HttpHandler) =
    this.ModifyResponse(fun _ _ -> handler)


type internal PolymorphicGetCollectionOperation<'ctx> =
  abstract Run: 'ctx -> Request -> ResponseBuilder<'ctx> -> HttpHandler


type PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
  getCollection: 'ctx -> Request -> Job<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity list, Error list>>
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
        job {
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
        |> Job.startAsTask

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
  mapCtx: 'originalCtx -> Request -> Job<Result<'ctx, Error list>>
  create: 'ctx -> Request -> Job<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity, Error list>>
  afterCreate: ('ctx -> 'entity -> Job<Result<'entity, Error list>>) option
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
        job {
          match! this.mapCtx ctx req with
          | Error errors -> return! handleErrors errors next httpCtx
          | Ok mappedCtx ->
              match! this.create mappedCtx req |> JobResult.bind (fun (fieldNames, _, e) -> box e |> patch ctx req fieldNames |> JobResult.map (fun e -> fieldNames, e)) with
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
        |> Job.startAsTask

  member this.AfterCreateJobRes(f: Func<'ctx, 'entity, Job<Result<'entity, Error list>>>) =
    { this with afterCreate = Some (fun ctx e -> f.Invoke(ctx, e)) }

  member this.AfterCreateJobRes(f: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    this.AfterCreateJobRes(fun ctx e -> f.Invoke(ctx, e) |> JobResult.map (fun () -> e))

  member this.AfterCreateJobRes(f: Func<'entity, Job<Result<'entity, Error list>>>) =
    this.AfterCreateJobRes(fun _ e -> f.Invoke e)

  member this.AfterCreateJobRes(f: Func<'entity, Job<Result<unit, Error list>>>) =
    this.AfterCreateJobRes(fun _ e -> f.Invoke e |> JobResult.map (fun () -> e))

  member this.AfterCreateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.AfterCreateJobRes(Job.liftAsyncFunc2 f)

  member this.AfterCreateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.AfterCreateJobRes(Job.liftAsyncFunc2 f)

  member this.AfterCreateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.AfterCreateJobRes(Job.liftAsyncFunc f)

  member this.AfterCreateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.AfterCreateJobRes(Job.liftAsyncFunc f)

  member this.AfterCreateJob(f: Func<'ctx, 'entity, Job<'entity>>) =
    this.AfterCreateJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.AfterCreateJob(f: Func<'ctx, 'entity, Job<unit>>) =
    this.AfterCreateJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.AfterCreateJob(f: Func<'entity, Job<'entity>>) =
    this.AfterCreateJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.AfterCreateJob(f: Func<'entity, Job<unit>>) =
    this.AfterCreateJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.AfterCreateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.AfterCreateJob(Job.liftAsyncFunc2 f)

  member this.AfterCreateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.AfterCreateJob(Job.liftAsyncFunc2 f)

  member this.AfterCreateAsync(f: Func<'entity, Async<'entity>>) =
    this.AfterCreateJob(Job.liftAsyncFunc f)

  member this.AfterCreateAsync(f: Func<'entity, Async<unit>>) =
    this.AfterCreateJob(Job.liftAsyncFunc f)

  member this.AfterCreateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.AfterCreateJobRes(Job.liftFunc2 f)

  member this.AfterCreateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.AfterCreateJobRes(Job.liftFunc2 f)

  member this.AfterCreateRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.AfterCreateJobRes(Job.liftFunc f)

  member this.AfterCreateRes(f: Func<'entity, Result<unit, Error list>>) =
    this.AfterCreateJobRes(Job.liftFunc f)

  member this.AfterCreate(f: Func<'ctx, 'entity, 'entity>) =
    this.AfterCreateJobRes(JobResult.liftFunc2 f)

  member this.AfterCreate(f: Func<'ctx, 'entity, unit>) =
    this.AfterCreateJobRes(JobResult.liftFunc2 f)

  member this.AfterCreate(f: Func<'entity, 'entity>) =
    this.AfterCreateJobRes(JobResult.liftFunc f)

  member this.AfterCreate(f: Func<'entity, unit>) =
    this.AfterCreateJobRes(JobResult.liftFunc f)

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
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
  beforeUpdate: 'ctx -> 'entity -> Job<Result<'entity, Error list>>
  customSetter: 'ctx -> Request -> 'entity -> Job<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity, Error list>>
  afterUpdate: ('ctx -> 'entity -> 'entity -> Job<Result<'entity, Error list>>) option
  modifyResponse: 'ctx -> 'entity -> HttpHandler
  return202Accepted: bool
} with

  static member internal Create (mapCtx) : PatchOperation<'originalCtx, 'ctx, 'entity> =
    {
      mapCtx = mapCtx
      beforeUpdate = fun _ x -> Ok x |> Job.result
      customSetter = fun _ _ e -> Ok (Set.empty, Set.empty, e) |> Job.result
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
        job {
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
                          match! this.customSetter mappedCtx req entity1 with
                          | Error errors -> return! handleErrors errors next httpCtx
                          | Ok (fns, _, entity2) ->
                              match! patch ctx req fns entity2 with
                              | Error errors -> return! handleErrors errors next httpCtx
                              | Ok entity3 ->
                                  match! afterUpdate mappedCtx (unbox<'entity> entity0) (unbox<'entity> entity3) with
                                  | Error errors -> return! handleErrors errors next httpCtx
                                  | Ok entity4 ->
                                      if this.return202Accepted then
                                        let handler =
                                          setStatusCode 202
                                          >=> this.modifyResponse mappedCtx (unbox<'entity> entity4)
                                        return! handler next httpCtx
                                      else
                                        let! doc = resp.Write ctx req (rDef, entity4)
                                        let handler =
                                          setStatusCode 200
                                          >=> this.modifyResponse mappedCtx entity4
                                          >=> jsonApiWithETag doc
                                        return! handler next httpCtx
        }
        |> Job.startAsTask


  member this.BeforeUpdateJobRes(f: Func<'ctx, 'entity, Job<Result<'entity, Error list>>>) =
    { this with beforeUpdate = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeUpdateJobRes(f: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    this.BeforeUpdateJobRes(fun ctx e -> f.Invoke(ctx, e) |> JobResult.map (fun () -> e))

  member this.BeforeUpdateJobRes(f: Func<'entity, Job<Result<'entity, Error list>>>) =
    this.BeforeUpdateJobRes(fun _ e -> f.Invoke e)

  member this.BeforeUpdateJobRes(f: Func<'entity, Job<Result<unit, Error list>>>) =
    this.BeforeUpdateJobRes(fun _ e -> f.Invoke e)

  member this.BeforeUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeUpdateJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeUpdateJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeUpdateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeUpdateJobRes(Job.liftAsyncFunc f)

  member this.BeforeUpdateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeUpdateJobRes(Job.liftAsyncFunc f)

  member this.BeforeUpdateJob(f: Func<'ctx, 'entity, Job<'entity>>) =
    this.BeforeUpdateJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeUpdateJob(f: Func<'ctx, 'entity, Job<unit>>) =
    this.BeforeUpdateJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeUpdateJob(f: Func<'entity, Job<'entity>>) =
    this.BeforeUpdateJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeUpdateJob(f: Func<'entity, Job<unit>>) =
    this.BeforeUpdateJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeUpdateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeUpdateJob(Job.liftAsyncFunc2 f)

  member this.BeforeUpdateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeUpdateJob(Job.liftAsyncFunc2 f)

  member this.BeforeUpdateAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeUpdateJob(Job.liftAsyncFunc f)

  member this.BeforeUpdateAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeUpdateJob(Job.liftAsyncFunc f)

  member this.BeforeUpdateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeUpdateJobRes(Job.liftFunc2 f)

  member this.BeforeUpdateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeUpdateJobRes(Job.liftFunc2 f)

  member this.BeforeUpdateRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeUpdateJobRes(Job.liftFunc f)

  member this.BeforeUpdateRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeUpdateJobRes(Job.liftFunc f)

  member this.BeforeUpdate(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeUpdateJobRes(JobResult.liftFunc2 f)

  member this.BeforeUpdate(f: Func<'ctx, 'entity, unit>) =
    this.BeforeUpdateJobRes(JobResult.liftFunc2 f)

  member this.BeforeUpdate(f: Func<'entity, 'entity>) =
    this.BeforeUpdateJobRes(JobResult.liftFunc f)

  member this.BeforeUpdate(f: Func<'entity, unit>) =
    this.BeforeUpdateJobRes(JobResult.liftFunc f)

  member this.AddCustomSetterJobRes (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Job<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    { this with
        customSetter =
          fun ctx req e ->
            this.customSetter ctx req e
            |> JobResult.bind (fun (fns, qns, e) ->
                getRequestParser.Invoke(ctx, e, RequestParserHelper<'ctx>(ctx, req))
                |> JobResult.bind (fun p -> p.ParseWithConsumed ())
                |> JobResult.map (fun (fns', qns', e) -> Set.union fns fns', Set.union qns qns', e)
            )
    }

  member this.AddCustomSetterAsyncRes (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    this.AddCustomSetterJobRes(Job.liftAsyncFunc3 getRequestParser)

  member this.AddCustomSetterJob (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Job<RequestParser<'ctx, 'entity>>>) =
    this.AddCustomSetterJobRes(fun ctx e parser -> getRequestParser.Invoke(ctx, e, parser) |> Job.map Ok)

  member this.AddCustomSetterAsync (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity>>>) =
    this.AddCustomSetterJob(Job.liftAsyncFunc3 getRequestParser)

  member this.AddCustomSetterRes (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity>, Error list>>) =
    this.AddCustomSetterJobRes(Job.liftFunc3 getRequestParser)

  member this.AddCustomSetter (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity>>) =
    this.AddCustomSetterJobRes(JobResult.liftFunc3 getRequestParser)

  member this.AfterUpdateJobRes(f: Func<'ctx, 'entity, 'entity, Job<Result<'entity, Error list>>>) =
    { this with afterUpdate = Some (fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew)) }

  member this.AfterUpdateJobRes(f: Func<'entity, 'entity, Job<Result<'entity, Error list>>>) =
    this.AfterUpdateJobRes(fun _ eOld eNew -> f.Invoke(eOld, eNew))

  member this.AfterUpdateJobRes(f: Func<'ctx, 'entity, 'entity, Job<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> JobResult.map (fun () -> eNew))

  member this.AfterUpdateJobRes(f: Func<'entity, 'entity, Job<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(fun _ eOld eNew -> f.Invoke(eOld, eNew) |> JobResult.map (fun () -> eNew))

  member this.AfterUpdateJobRes(f: Func<'ctx, 'entity, Job<Result<'entity, Error list>>>) =
    this.AfterUpdateJobRes(fun ctx _ eNew -> f.Invoke(ctx, eNew))

  member this.AfterUpdateJobRes(f: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(fun ctx e -> f.Invoke(ctx, e) |> JobResult.map (fun () -> e))

  member this.AfterUpdateJobRes(f: Func<'entity, Job<Result<'entity, Error list>>>) =
    this.AfterUpdateJobRes(fun _ _ e -> f.Invoke e)

  member this.AfterUpdateJobRes(f: Func<'entity, Job<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(fun _ _ e -> f.Invoke e |> JobResult.map (fun () -> e))

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, 'entity, Async<Result<'entity, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc3 f)

  member this.AfterUpdateAsyncRes(f: Func<'entity, 'entity, Async<Result<'entity, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, 'entity, Async<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc3 f)

  member this.AfterUpdateAsyncRes(f: Func<'entity, 'entity, Async<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc f)

  member this.AfterUpdateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.AfterUpdateJobRes(Job.liftAsyncFunc f)

  member this.AfterUpdateJob(f: Func<'ctx, 'entity, 'entity, Job<'entity>>) =
    this.AfterUpdateJobRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Job.map Ok)

  member this.AfterUpdateJob(f: Func<'entity, 'entity, Job<'entity>>) =
    this.AfterUpdateJobRes(fun eOld eNew -> f.Invoke(eOld, eNew) |> Job.map Ok)

  member this.AfterUpdateJob(f: Func<'ctx, 'entity, 'entity, Job<unit>>) =
    this.AfterUpdateJobRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Job.map Ok)

  member this.AfterUpdateJob(f: Func<'entity, 'entity, Job<unit>>) =
    this.AfterUpdateJobRes(fun _ eOld eNew -> f.Invoke(eOld, eNew) |> Job.map Ok)

  member this.AfterUpdateJob(f: Func<'ctx, 'entity, Job<'entity>>) =
    this.AfterUpdateJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.AfterUpdateJob(f: Func<'ctx, 'entity, Job<unit>>) =
    this.AfterUpdateJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.AfterUpdateJob(f: Func<'entity, Job<'entity>>) =
    this.AfterUpdateJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.AfterUpdateJob(f: Func<'entity, Job<unit>>) =
    this.AfterUpdateJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, 'entity, Async<'entity>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc3 f)

  member this.AfterUpdateAsync(f: Func<'entity, 'entity, Async<'entity>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, 'entity, Async<unit>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc3 f)

  member this.AfterUpdateAsync(f: Func<'entity, 'entity, Async<unit>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc2 f)

  member this.AfterUpdateAsync(f: Func<'entity, Async<'entity>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc f)

  member this.AfterUpdateAsync(f: Func<'entity, Async<unit>>) =
    this.AfterUpdateJob(Job.liftAsyncFunc f)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, 'entity, Result<'entity, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc3 f)

  member this.AfterUpdateRes(f: Func<'entity, 'entity, Result<'entity, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc2 f)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, 'entity, Result<unit, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc3 f)

  member this.AfterUpdateRes(f: Func<'entity, 'entity, Result<unit, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc2 f)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc2 f)

  member this.AfterUpdateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc2 f)

  member this.AfterUpdateRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc f)

  member this.AfterUpdateRes(f: Func<'entity, Result<unit, Error list>>) =
    this.AfterUpdateJobRes(Job.liftFunc f)

  member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity, 'entity>) =
    this.AfterUpdateJobRes(JobResult.liftFunc3 f)

  member this.AfterUpdate(f: Func<'entity, 'entity, 'entity>) =
    this.AfterUpdateJobRes(JobResult.liftFunc2 f)

  member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity, unit>) =
    this.AfterUpdateJobRes(JobResult.liftFunc3 f)

  member this.AfterUpdate(f: Func<'entity, 'entity, unit>) =
    this.AfterUpdateJobRes(JobResult.liftFunc2 f)

  member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity>) =
    this.AfterUpdateJobRes(JobResult.liftFunc2 f)

  member this.AfterUpdate(f: Func<'ctx, 'entity, unit>) =
    this.AfterUpdateJobRes(JobResult.liftFunc2 f)

  member this.AfterUpdate(f: Func<'entity, 'entity>) =
    this.AfterUpdateJobRes(JobResult.liftFunc f)

  member this.AfterUpdate(f: Func<'entity, unit>) =
    this.AfterUpdateJobRes(JobResult.liftFunc f)

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
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
  beforeDelete: 'ctx -> 'entity -> Job<Result<'entity, Error list>>
  delete: 'ctx -> Request -> 'entity -> Job<Result<unit, Error list>>
  modifyResponse: 'ctx -> HttpHandler
  return202Accepted: bool
} with

  static member internal Create(mapCtx, delete) : DeleteOperation<'originalCtx, 'ctx, 'entity> =
    {
      mapCtx = mapCtx
      beforeDelete = fun _ x -> Ok x |> Job.result
      delete = delete
      modifyResponse = fun _ -> fun next ctx -> next ctx
      return202Accepted = false
    }


  interface DeleteOperation<'originalCtx> with
    member this.Run resDef ctx req preconditions entity0 resp =
      fun next httpCtx ->
        job {
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
        |> Job.startAsTask


  member this.BeforeDeleteJobRes(f: Func<'ctx, 'entity, Job<Result<'entity, Error list>>>) =
    { this with beforeDelete = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeDeleteJobRes(f: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    this.BeforeDeleteJobRes(fun ctx e -> f.Invoke(ctx, e) |> JobResult.map (fun () -> e))

  member this.BeforeDeleteJobRes(f: Func<'entity, Job<Result<'entity, Error list>>>) =
    this.BeforeDeleteJobRes(fun _ e -> f.Invoke e)

  member this.BeforeDeleteJobRes(f: Func<'entity, Job<Result<unit, Error list>>>) =
    this.BeforeDeleteJobRes(fun _ e -> f.Invoke e)

  member this.BeforeDeleteAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeDeleteJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeDeleteAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeDeleteJobRes(Job.liftAsyncFunc2 f)

  member this.BeforeDeleteAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeDeleteJobRes(Job.liftAsyncFunc f)

  member this.BeforeDeleteAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeDeleteJobRes(Job.liftAsyncFunc f)

  member this.BeforeDeleteJob(f: Func<'ctx, 'entity, Job<'entity>>) =
    this.BeforeDeleteJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeDeleteJob(f: Func<'ctx, 'entity, Job<unit>>) =
    this.BeforeDeleteJobRes(fun ctx e -> f.Invoke(ctx, e) |> Job.map Ok)

  member this.BeforeDeleteJob(f: Func<'entity, Job<'entity>>) =
    this.BeforeDeleteJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeDeleteJob(f: Func<'entity, Job<unit>>) =
    this.BeforeDeleteJobRes(fun e -> f.Invoke e |> Job.map Ok)

  member this.BeforeDeleteAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
    this.BeforeDeleteJob(Job.liftAsyncFunc2 f)

  member this.BeforeDeleteAsync(f: Func<'ctx, 'entity, Async<unit>>) =
    this.BeforeDeleteJob(Job.liftAsyncFunc2 f)

  member this.BeforeDeleteAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeDeleteJob(Job.liftAsyncFunc f)

  member this.BeforeDeleteAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeDeleteJob(Job.liftAsyncFunc f)

  member this.BeforeDeleteRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
    this.BeforeDeleteJobRes(Job.liftFunc2 f)

  member this.BeforeDeleteRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.BeforeDeleteJobRes(Job.liftFunc2 f)

  member this.BeforeDeleteRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeDeleteJobRes(Job.liftFunc f)

  member this.BeforeDeleteRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeDeleteJobRes(Job.liftFunc f)

  member this.BeforeDelete(f: Func<'ctx, 'entity, 'entity>) =
    this.BeforeDeleteJobRes(JobResult.liftFunc2 f)

  member this.BeforeDelete(f: Func<'ctx, 'entity, unit>) =
    this.BeforeDeleteJobRes(JobResult.liftFunc2 f)

  member this.BeforeDelete(f: Func<'entity, 'entity>) =
    this.BeforeDeleteJobRes(JobResult.liftFunc f)

  member this.BeforeDelete(f: Func<'entity, unit>) =
    this.BeforeDeleteJobRes(JobResult.liftFunc f)

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
  abstract HrefAndMeta: 'ctx -> uri: string -> BoxedEntity -> Job<(string option * Map<string, obj> option)>
  abstract Get: ('ctx -> Request -> Responder<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract Post: ('ctx -> Request -> Responder<'ctx> -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract Patch: ('ctx -> Request -> Responder<'ctx> -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option
  abstract Delete: ('ctx -> Request -> Responder<'ctx> -> Preconditions<'ctx> -> BoxedEntity -> HttpHandler) option



type CustomOperation<'originalCtx, 'ctx, 'entity> = internal {
  mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>
  name: string
  getMeta: ('ctx -> 'entity -> Map<string, obj>) option
  condition: 'ctx -> 'entity -> Job<Result<unit, Error list>>
  get: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Job<Result<HttpHandler, Error list>>) option
  post: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Job<Result<HttpHandler, Error list>>) option
  patch: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Job<Result<HttpHandler, Error list>>) option
  delete: ('ctx -> Request -> Responder<'originalCtx> -> 'entity -> Job<Result<HttpHandler, Error list>>) option
} with

  static member internal Create(mapCtx, name) : CustomOperation<'originalCtx, 'ctx, 'entity> =
    {
      mapCtx = mapCtx
      name = name
      getMeta = None
      condition = fun _ _ -> Ok () |> Job.result
      get = None
      post = None
      patch = None
      delete = None
    }


  member private this.handler (operation: _ -> _ -> _ -> _ -> Job<Result<HttpHandler,_>>) ctx req responder (preconditions: Preconditions<'originalCtx>) (entity: obj) =
    fun next httpCtx ->
      job {
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
      |> Job.startAsTask


  interface CustomOperation<'originalCtx> with
    member this.Name = this.name

    member this.HrefAndMeta ctx selfUrl entity =
      job {
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
              | Ok () -> return Some (selfUrl + "/" + this.name), meta
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


  member this.ConditionJobRes(predicate: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    { this with condition = fun ctx e -> predicate.Invoke(ctx, e) }

  member this.ConditionJobRes(predicate: Func<'entity, Job<Result<unit, Error list>>>) =
    this.ConditionJobRes(fun _ e -> predicate.Invoke e)

  member this.ConditionAsyncRes(predicate: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.ConditionJobRes(Job.liftAsyncFunc2 predicate)

  member this.ConditionAsyncRes(predicate: Func<'entity, Async<Result<unit, Error list>>>) =
    this.ConditionJobRes(Job.liftAsyncFunc predicate)

  member this.ConditionRes(predicate: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.ConditionJobRes(Job.liftFunc2 predicate)

  member this.ConditionRes(predicate: Func<'entity, Result<unit, Error list>>) =
    this.ConditionJobRes(Job.liftFunc predicate)

  member this.Condition(predicate: Func<'ctx, 'entity, bool>) =
    this.ConditionJobRes(fun ctx e -> (if predicate.Invoke(ctx, e) then Ok () else Error [customOpConditionFalse]) |> Job.result)

  member this.Condition(predicate: Func<'entity, bool>) =
    this.ConditionJobRes(fun _ e -> (if predicate.Invoke e then Ok () else Error [customOpConditionFalse]) |> Job.result)

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

  member this.GetJob(get: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Job<Result<HttpHandler, Error list>>>) =
    { this with get = Some (fun ctx req responder entity -> get.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }

  member this.PostJob(post: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Job<Result<HttpHandler, Error list>>>) =
    { this with post = Some (fun ctx req responder entity -> post.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }

  member this.PatchJob(patch: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Job<Result<HttpHandler, Error list>>>) =
    { this with patch = Some (fun ctx req responder entity -> patch.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }

  member this.DeleteJob(delete: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Job<Result<HttpHandler, Error list>>>) =
    { this with delete = Some (fun ctx req responder entity -> delete.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req), responder, entity)) }

  // TODO: Add Async prefix

  member this.Get(get: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    this.GetJob(Job.liftAsyncFunc4 get)

  member this.Post(post: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    this.PostJob(Job.liftAsyncFunc4 post)

  member this.Patch(patch: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    this.PatchJob(Job.liftAsyncFunc4 patch)

  member this.Delete(delete: Func<'ctx, RequestParserHelper<'ctx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>) =
    this.DeleteJob(Job.liftAsyncFunc4 delete)



type PolymorphicOperationHelper<'originalCtx, 'ctx, 'entity, 'id> internal (mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>) =

  member _.LookupJobRes(getById: Func<'ctx, 'id, Job<Result<'entity option, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, (fun ctx id -> getById.Invoke(ctx, id)), getPolyBuilder)

  member this.LookupJobRes(getById: Func<'id, Job<Result<'entity option, Error list>>>, getPolyBuilder: 'entity  -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes((fun _ id -> getById.Invoke id), getPolyBuilder)

  member this.LookupAsyncRes(getById: Func<'ctx, 'id, Async<Result<'entity option, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes(Job.liftAsyncFunc2 getById, getPolyBuilder)

  member this.LookupAsyncRes(getById: Func<'id, Async<Result<'entity option, Error list>>>, getPolyBuilder: 'entity  -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes(Job.liftAsyncFunc getById, getPolyBuilder)

  member this.LookupJob(getById: Func<'ctx, 'id, Job<'entity option>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes((fun ctx id -> getById.Invoke(ctx, id) |> Job.map Ok), getPolyBuilder)

  member this.LookupJob(getById: Func<'id, Job<'entity option>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes((fun _ id -> getById.Invoke id |> Job.map Ok), getPolyBuilder)

  member this.LookupAsync(getById: Func<'ctx, 'id, Async<'entity option>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJob(Job.liftAsyncFunc2 getById, getPolyBuilder)

  member this.LookupAsync(getById: Func<'id, Async<'entity option>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJob(Job.liftAsyncFunc getById, getPolyBuilder)

  member this.LookupRes(getById: Func<'ctx, 'id, Result<'entity option, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes(Job.liftFunc2 getById, getPolyBuilder)

  member this.LookupRes(getById: Func<'id, Result<'entity option, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes(Job.liftFunc getById, getPolyBuilder)

  member this.Lookup(getById: Func<'ctx, 'id, 'entity option>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes(JobResult.liftFunc2 getById, getPolyBuilder)

  member this.Lookup(getById: Func<'id, 'entity option>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.LookupJobRes(JobResult.liftFunc getById, getPolyBuilder)

  member _.GetCollectionJobRes(getCollection: Func<'ctx, Job<Result<'entity list, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, (fun ctx req -> getCollection.Invoke ctx |> JobResult.map (fun xs -> Set.empty, Set.empty, xs)), getPolyBuilder)

  member this.GetCollectionJobRes(getCollection: Func<unit, Job<Result<'entity list, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes((fun (_: 'ctx) -> getCollection.Invoke ()), getPolyBuilder)

  member _.GetCollectionJobRes(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Job<Result<RequestParser<'ctx, 'entity list>, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(
      mapCtx,
      (fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req))
        |> JobResult.bind (fun p -> p.ParseWithConsumed ())
      ),
      getPolyBuilder
    )

  member this.GetCollectionAsyncRes(getCollection: Func<'ctx, Async<Result<'entity list, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(Job.liftAsyncFunc getCollection, getPolyBuilder)

  member this.GetCollectionAsyncRes(getCollection: Func<unit, Async<Result<'entity list, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(Job.liftAsyncFunc getCollection, getPolyBuilder)

  member this.GetCollectionAsyncRes(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity list>, Error list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(Job.liftAsyncFunc2 getRequestParser, getPolyBuilder)

  member this.GetCollectionJob(getCollection: Func<'ctx, Job<'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(getCollection.Invoke >> Job.map Ok, getPolyBuilder)

  member this.GetCollectionJob(getCollection: Func<unit, Job<'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(getCollection.Invoke >> Job.map Ok, getPolyBuilder)

  member this.GetCollectionJob(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Job<RequestParser<'ctx, 'entity list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes((fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Job.map Ok), getPolyBuilder)

  member this.GetCollectionAsync(getCollection: Func<'ctx, Async<'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJob(Job.liftAsyncFunc getCollection, getPolyBuilder)

  member this.GetCollectionAsync(getCollection: Func<unit, Async<'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJob(Job.liftAsyncFunc getCollection, getPolyBuilder)

  member this.GetCollectionAsync(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity list>>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJob(Job.liftAsyncFunc2 getRequestParser, getPolyBuilder)

  member this.GetCollectionRes(getCollection: Func<'ctx, Result<'entity list, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(Job.liftFunc getCollection, getPolyBuilder)

  member this.GetCollectionRes(getCollection: Func<unit, Result<'entity list, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(Job.liftFunc getCollection, getPolyBuilder)

  member this.GetCollectionRes(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity list>, Error list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(Job.liftFunc2 getRequestParser, getPolyBuilder)

  member this.GetCollection(getCollection: Func<'ctx, 'entity list>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(JobResult.liftFunc getCollection, getPolyBuilder)

  member this.GetCollection(getCollection: Func<unit, 'entity list>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(JobResult.liftFunc getCollection, getPolyBuilder)

  member this.GetCollection(getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity list>>, getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>) =
    this.GetCollectionJobRes(JobResult.liftFunc2 getRequestParser, getPolyBuilder)



type OperationHelper<'originalCtx, 'ctx, 'entity, 'id> internal (mapCtx: 'originalCtx -> Job<Result<'ctx, Error list>>) =

  member _.Polymorphic = PolymorphicOperationHelper<'originalCtx, 'ctx, 'entity, 'id>(mapCtx)

  member _.ForContextJobRes (mapCtx: 'originalCtx -> Job<Result<'mappedCtx, Error list>>) =
    OperationHelper<'originalCtx, 'mappedCtx, 'entity, 'id>(mapCtx)

  member this.ForContextAsyncRes (mapCtx: 'originalCtx -> Async<Result<'mappedCtx, Error list>>) =
    this.ForContextJobRes(Job.liftAsync mapCtx)

  member this.ForContextJobOpt (mapCtx: 'originalCtx -> Job<'mappedCtx option>) =
    this.ForContextJobRes(mapCtx >> Job.map (Result.requireSome [opMapCtxFailedNone]))

  member this.ForContextAsyncOpt (mapCtx: 'originalCtx -> Async<'mappedCtx option>) =
    this.ForContextJobOpt(Job.liftAsync mapCtx)

  member this.ForContextJob (mapCtx: 'originalCtx -> Job<'mappedCtx>) =
    this.ForContextJobRes(mapCtx >> Job.map Ok)

  member this.ForContextAsync (mapCtx: 'originalCtx -> Async<'mappedCtx>) =
    this.ForContextJob(Job.liftAsync mapCtx)

  member this.ForContextRes (mapCtx: 'originalCtx -> Result<'mappedCtx, Error list>) =
    this.ForContextJobRes(Job.lift mapCtx)

  member this.ForContextOpt (mapCtx: 'originalCtx -> 'mappedCtx option) =
    this.ForContextJobOpt(Job.lift mapCtx)

  member this.ForContext (mapCtx: 'originalCtx -> 'mappedCtx) =
    this.ForContextJobRes(JobResult.lift mapCtx)

  member _.LookupJobRes (getById: Func<'ctx, 'id, Job<Result<'entity option, Error list>>>) =
    ResourceLookup<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, fun ctx id -> getById.Invoke(ctx, id))

  member this.LookupJobRes (getById: Func<'id, Job<Result<'entity option, Error list>>>) =
    this.LookupJobRes(fun _ id -> getById.Invoke id)

  member this.LookupAsyncRes (getById: Func<'ctx, 'id, Async<Result<'entity option, Error list>>>) =
    this.LookupJobRes(Job.liftAsyncFunc2 getById)

  member this.LookupAsyncRes (getById: Func<'id, Async<Result<'entity option, Error list>>>) =
    this.LookupJobRes(Job.liftAsyncFunc getById)

  member this.LookupJob (getById: Func<'ctx, 'id, Job<'entity option>>) =
    this.LookupJobRes(fun ctx id -> getById.Invoke(ctx, id) |> Job.map Ok)

  member this.LookupJob (getById: Func<'id, Job<'entity option>>) =
    this.LookupJobRes(fun _ id -> getById.Invoke id |> Job.map Ok)

  member this.LookupAsync (getById: Func<'ctx, 'id, Async<'entity option>>) =
    this.LookupJob(Job.liftAsyncFunc2 getById)

  member this.LookupAsync (getById: Func<'id, Async<'entity option>>) =
    this.LookupJob(Job.liftAsyncFunc getById)

  member this.LookupRes (getById: Func<'ctx, 'id, Result<'entity option, Error list>>) =
    this.LookupJobRes(Job.liftFunc2 getById)

  member this.LookupRes (getById: Func<'id, Result<'entity option, Error list>>) =
    this.LookupJobRes(Job.liftFunc getById)

  member this.Lookup (getById: Func<'ctx, 'id, 'entity option>) =
    this.LookupJobRes(JobResult.liftFunc2 getById)

  member this.Lookup (getById: Func<'id, 'entity option>) =
    this.LookupJobRes(JobResult.liftFunc getById)

  member _.GetResource () =
    GetResourceOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx)

  member _.GetCollectionJobRes (getCollection: Func<'ctx, Job<Result<'entity list, Error list>>>) =
    GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx, fun ctx req -> getCollection.Invoke ctx |> JobResult.map (fun xs -> Set.empty, Set.empty, xs))

  member this.GetCollectionJobRes (getCollection: Func<unit, Job<Result<'entity list, Error list>>>) =
    this.GetCollectionJobRes(fun (_: 'ctx) -> getCollection.Invoke ())

  member _.GetCollectionJobRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Job<Result<RequestParser<'ctx, 'entity list>, Error list>>>) =
    GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(
      mapCtx,
      fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req))
        |> JobResult.bind (fun p -> p.ParseWithConsumed ())
    )

  member this.GetCollectionAsyncRes (getCollection: Func<'ctx, Async<Result<'entity list, Error list>>>) =
    this.GetCollectionJobRes(Job.liftAsyncFunc getCollection)

  member this.GetCollectionAsyncRes (getCollection: Func<unit, Async<Result<'entity list, Error list>>>) =
    this.GetCollectionJobRes(Job.liftAsyncFunc getCollection)

  member this.GetCollectionAsyncRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity list>, Error list>>>) =
    this.GetCollectionJobRes(Job.liftAsyncFunc2 getRequestParser)

  member this.GetCollectionJob (getCollection: Func<'ctx, Job<'entity list>>) =
    this.GetCollectionJobRes(getCollection.Invoke >> Job.map Ok)

  member this.GetCollectionJob (getCollection: Func<unit, Job<'entity list>>) =
    this.GetCollectionJobRes(getCollection.Invoke >> Job.map Ok)

  member this.GetCollectionJob (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Job<RequestParser<'ctx, 'entity list>>>) =
    this.GetCollectionJobRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Job.map Ok)

  member this.GetCollectionAsync (getCollection: Func<'ctx, Async<'entity list>>) =
    this.GetCollectionJob(Job.liftAsyncFunc getCollection)

  member this.GetCollectionAsync (getCollection: Func<unit, Async<'entity list>>) =
    this.GetCollectionJob(Job.liftAsyncFunc getCollection)

  member this.GetCollectionAsync (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity list>>>) =
    this.GetCollectionJob(Job.liftAsyncFunc2 getRequestParser)

  member this.GetCollectionRes (getCollection: Func<'ctx, Result<'entity list, Error list>>) =
    this.GetCollectionJobRes(Job.liftFunc getCollection)

  member this.GetCollectionRes (getCollection: Func<unit, Result<'entity list, Error list>>) =
    this.GetCollectionJobRes(Job.liftFunc getCollection)

  member this.GetCollectionRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity list>, Error list>>) =
    this.GetCollectionJobRes(Job.liftFunc2 getRequestParser)

  member this.GetCollection (getCollection: Func<'ctx, 'entity list>) =
    this.GetCollectionJobRes(JobResult.liftFunc getCollection)

  member this.GetCollection (getCollection: Func<unit, 'entity list>) =
    this.GetCollectionJobRes(JobResult.liftFunc getCollection)

  member this.GetCollection (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity list>>) =
    this.GetCollectionJobRes(JobResult.liftFunc2 getRequestParser)

  member _.PostJobRes (createEntity: Func<'ctx, Job<Result<'entity, Error list>>>) =
    PostOperation<'originalCtx, 'ctx, 'entity>.Create((fun ctx res -> mapCtx ctx), fun ctx res -> createEntity.Invoke ctx |> JobResult.map (fun e -> Set.empty, Set.empty, e))

  member this.PostJobRes (createEntity: Func<unit, Job<Result<'entity, Error list>>>) =
    this.PostJobRes(fun (ctx: 'ctx) -> createEntity.Invoke ())

  member _.PostJobRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Job<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    PostOperation<'originalCtx, 'ctx, 'entity>.Create(
      (fun ctx _ -> mapCtx ctx),
      fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(ctx, req))
        |> JobResult.bind (fun p -> p.ParseWithConsumed ())
    )

  member this.PostAsyncRes (createEntity: Func<'ctx, Async<Result<'entity, Error list>>>) =
    this.PostJobRes(Job.liftAsyncFunc createEntity)

  member this.PostAsyncRes (createEntity: Func<unit, Async<Result<'entity, Error list>>>) =
    this.PostJobRes(Job.liftAsyncFunc createEntity)

  member this.PostAsyncRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    this.PostJobRes(Job.liftAsyncFunc2 getRequestParser)

  member this.PostJob (createEntity: Func<'ctx, Job<'entity>>) =
    this.PostJobRes(createEntity.Invoke >> Job.map Ok)

  member this.PostJob (createEntity: Func<unit, Job<'entity>>) =
    this.PostJobRes(createEntity.Invoke >> Job.map Ok)

  member this.PostJob (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Job<RequestParser<'ctx, 'entity>>>) =
    this.PostJobRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Job.map Ok)

  member this.PostAsync (createEntity: Func<'ctx, Async<'entity>>) =
    this.PostJob(Job.liftAsyncFunc createEntity)

  member this.PostAsync (createEntity: Func<unit, Async<'entity>>) =
    this.PostJob(Job.liftAsyncFunc createEntity)

  member this.PostAsync (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity>>>) =
    this.PostJob(Job.liftAsyncFunc2 getRequestParser)

  member this.PostRes (createEntity: Func<'ctx, Result<'entity, Error list>>) =
    this.PostJobRes(Job.liftFunc createEntity)

  member this.PostRes (createEntity: Func<unit, Result<'entity, Error list>>) =
    this.PostJobRes(Job.liftFunc createEntity)

  member this.PostRes (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity>, Error list>>) =
    this.PostJobRes(Job.liftFunc2 getRequestParser)

  member this.Post (createEntity: Func<'ctx, 'entity>) =
    this.PostJobRes(JobResult.liftFunc createEntity)

  member this.Post (createEntity: Func<unit, 'entity>) =
    this.PostJobRes(JobResult.liftFunc createEntity)

  member this.Post (getRequestParser: Func<'ctx, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity>>) =
    this.PostJobRes(JobResult.liftFunc2 getRequestParser)

  member _.PostBackRefJobRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Job<Result<'entity, Error list>>>) =
    let mapCtxWithBackRef ctx req =
      mapCtx ctx
      |> JobResult.bind (fun mappedCtx ->
          backRef.Get(ctx, req, None)
          |> JobResult.map (fun e -> mappedCtx, e)
      )
    let consumedFieldNames = match backRef.FieldName with None -> Set.empty | Some fn -> Set.empty.Add fn
    PostOperation<'originalCtx, 'ctx * 'backRefEntity, 'entity>.Create(mapCtxWithBackRef, fun ctx res -> createEntity.Invoke ctx |> JobResult.map (fun e -> consumedFieldNames, Set.empty, e))

  member this.PostBackRefJobRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Job<Result<'entity, Error list>>>) =
    this.PostBackRefJobRes(backRef, fun (ctx: 'ctx, br: 'backRefEntity) -> createEntity.Invoke ())

  member _.PostBackRefJobRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Job<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    let mapCtxWithBackRef ctx req =
      mapCtx ctx
      |> JobResult.bind (fun mappedCtx ->
          backRef.Get(ctx, req, None)
          |> JobResult.map (fun e -> mappedCtx, e)
      )
    let addBackRefFieldName = match backRef.FieldName with None -> id | Some fn -> Set.add fn
    PostOperation<'originalCtx, 'ctx * 'backRefEntity, 'entity>.Create(
      mapCtxWithBackRef,
      fun ctx req ->
        getRequestParser.Invoke(ctx, RequestParserHelper<'ctx>(fst ctx, req))
        |> JobResult.bind (fun p -> p.ParseWithConsumed ())
        |> JobResult.map (fun (fns, qns, e) -> addBackRefFieldName fns, qns, e)
    )

  member this.PostBackRefAsyncRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Async<Result<'entity, Error list>>>) =
    this.PostBackRefJobRes(backRef, Job.liftAsyncFunc createEntity)

  member this.PostBackRefAsyncRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Async<Result<'entity, Error list>>>) =
    this.PostBackRefJobRes(backRef, Job.liftAsyncFunc createEntity)

  member this.PostBackRefAsyncRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, 'entity>, Error list>>>) =
    this.PostBackRefJobRes(backRef, Job.liftAsyncFunc2 getRequestParser)

  member this.PostBackRefJob (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Job<'entity>>) =
    this.PostBackRefJobRes(backRef, createEntity.Invoke >> Job.map Ok)

  member this.PostBackRefJob (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Job<'entity>>) =
    this.PostBackRefJobRes(backRef, createEntity.Invoke >> Job.map Ok)

  member this.PostBackRefJob (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Job<RequestParser<'ctx, 'entity>>>) =
    this.PostBackRefJobRes(backRef, fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Job.map Ok)

  member this.PostBackRefAsync (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Async<'entity>>) =
    this.PostBackRefJob(backRef, Job.liftAsyncFunc createEntity)

  member this.PostBackRefAsync (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Async<'entity>>) =
    this.PostBackRefJob(backRef, Job.liftAsyncFunc createEntity)

  member this.PostBackRefAsync (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, 'entity>>>) =
    this.PostBackRefJob(backRef, Job.liftAsyncFunc2 getRequestParser)

  member this.PostBackRefRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, Result<'entity, Error list>>) =
    this.PostBackRefJobRes(backRef, Job.liftFunc createEntity)

  member this.PostBackRefRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, Result<'entity, Error list>>) =
    this.PostBackRefJobRes(backRef, Job.liftFunc createEntity)

  member this.PostBackRefRes (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, 'entity>, Error list>>) =
    this.PostBackRefJobRes(backRef, Job.liftFunc2 getRequestParser)

  member this.PostBackRef (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<'ctx * 'backRefEntity, 'entity>) =
    this.PostBackRefJobRes(backRef, JobResult.liftFunc createEntity)

  member this.PostBackRef (backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, 'entity>) =
    this.PostBackRefJobRes(backRef, JobResult.liftFunc createEntity)

  member this.PostBackRef (backRef: RequestGetter<'originalCtx, 'backRefEntity>, getRequestParser: Func<'ctx * 'backRefEntity, RequestParserHelper<'ctx>, RequestParser<'ctx, 'entity>>) =
    this.PostBackRefJobRes(backRef, JobResult.liftFunc2 getRequestParser)

  member _.Patch() =
    PatchOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx)

  member _.DeleteJobRes(delete: Func<'ctx, 'entity, Job<Result<unit, Error list>>>) =
    DeleteOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx, fun ctx _ entity -> delete.Invoke(ctx, entity))

  member this.DeleteJobRes(delete: Func<'entity, Job<Result<unit, Error list>>>) =
    this.DeleteJobRes(fun _ e -> delete.Invoke e)

  member _.DeleteJobRes(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Job<Result<RequestParser<'ctx, unit>, Error list>>>) =
    DeleteOperation<'originalCtx, 'ctx, 'entity>.Create(
      mapCtx,
      fun ctx req entity ->
        getRequestParser.Invoke(ctx, entity, RequestParserHelper<'ctx>(ctx, req))
        |> JobResult.bind (fun p -> p.Parse ())
    )

  member this.DeleteJobRes(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Job<Result<RequestParser<'ctx, unit>, Error list>>>) =
    this.DeleteJobRes(fun _ e p -> getRequestParser.Invoke(e, p))

  member this.DeleteAsyncRes(delete: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
    this.DeleteJobRes(Job.liftAsyncFunc2 delete)

  member this.DeleteAsyncRes(delete: Func<'entity, Async<Result<unit, Error list>>>) =
    this.DeleteJobRes(Job.liftAsyncFunc delete)

  member this.DeleteAsyncRes(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, unit>, Error list>>>) =
    this.DeleteJobRes(Job.liftAsyncFunc3 getRequestParser)

  member this.DeleteAsyncRes(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Async<Result<RequestParser<'ctx, unit>, Error list>>>) =
    this.DeleteJobRes(Job.liftAsyncFunc2 getRequestParser)

  member this.DeleteJob(delete: Func<'ctx, 'entity, Job<unit>>) =
    this.DeleteJobRes(fun ctx e -> delete.Invoke(ctx, e) |> Job.map Ok)

  member this.DeleteJob(delete: Func<'entity, Job<unit>>) =
    this.DeleteJobRes(fun _ e -> delete.Invoke e |> Job.map Ok)

  member this.DeleteJob(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Job<RequestParser<'ctx, unit>>>) =
    this.DeleteJobRes(fun ctx e parse -> getRequestParser.Invoke(ctx, e, parse) |> Job.map Ok)

  member this.DeleteJob(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Job<RequestParser<'ctx, unit>>>) =
    this.DeleteJobRes(fun ctx e parse -> getRequestParser.Invoke(e, parse) |> Job.map Ok)

  member this.DeleteAsync(delete: Func<'ctx, 'entity, Async<unit>>) =
    this.DeleteJob(Job.liftAsyncFunc2 delete)

  member this.DeleteAsync(delete: Func<'entity, Async<unit>>) =
    this.DeleteJob(Job.liftAsyncFunc delete)

  member this.DeleteAsync(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, unit>>>) =
    this.DeleteJob(Job.liftAsyncFunc3 getRequestParser)

  member this.DeleteAsync(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Async<RequestParser<'ctx, unit>>>) =
    this.DeleteJob(Job.liftAsyncFunc2 getRequestParser)

  member this.DeleteRes(delete: Func<'ctx, 'entity, Result<unit, Error list>>) =
    this.DeleteJobRes(Job.liftFunc2 delete)

  member this.DeleteRes(delete: Func<'entity, Result<unit, Error list>>) =
    this.DeleteJobRes(Job.liftFunc delete)

  member this.DeleteRes(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, unit>, Error list>>) =
    this.DeleteJobRes(Job.liftFunc3 getRequestParser)

  member this.DeleteRes(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, Result<RequestParser<'ctx, unit>, Error list>>) =
    this.DeleteJobRes(Job.liftFunc2 getRequestParser)

  member this.Delete(delete: Func<'ctx, 'entity, unit>) =
    this.DeleteJobRes(JobResult.liftFunc2 delete)

  member this.Delete(delete: Func<'entity, unit>) =
    this.DeleteJobRes(JobResult.liftFunc delete)

  member this.Delete(getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'ctx>, RequestParser<'ctx, unit>>) =
    this.DeleteJobRes(JobResult.liftFunc3 getRequestParser)

  member this.Delete(getRequestParser: Func<'entity, RequestParserHelper<'ctx>, RequestParser<'ctx, unit>>) =
    this.DeleteJobRes(JobResult.liftFunc2 getRequestParser)

  member _.CustomLink([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    CustomOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx, name)
