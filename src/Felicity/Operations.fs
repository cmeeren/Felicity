namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Net.Http.Headers
open Giraffe
open Errors



module private PreconditionHelpers =


    let validate (httpCtx: HttpContext) ctx getETag getLastModified isOptional =
        let eTag = getETag ctx
        let lastModified = getLastModified ctx
        let res = httpCtx.ValidatePreconditions(eTag, lastModified)
        // Clear headers because response-level ETag/Last-Modified headers don't
        // necessarily make sense in JSON:API due to compound documents; these values
        // should be communicated as attributes or meta.
        httpCtx.Response.Headers.Remove "ETag" |> ignore
        httpCtx.Response.Headers.Remove "Last-Modified" |> ignore

        match res with
        | ConditionFailed -> Error [ preconditionFailed eTag.IsSome lastModified.IsSome ]
        | NoConditionsSpecified when not isOptional -> Error [ preconditionRequired eTag.IsSome lastModified.IsSome ]
        | _ -> Ok()


module internal StrictModeHelpers =


    [<RequiresExplicitTypeArguments>]
    let checkForUnknownQueryParameters<'ctx>
        (httpCtx: HttpContext)
        (req: Request)
        (consumed: Set<ConsumedQueryParamName>)
        =
        let strictMode = httpCtx.GetService<UnknownQueryParamStrictMode<'ctx>>()
        let linkConfig = httpCtx.GetService<LinkConfig<'ctx>>()

        // Quick return if strict mode is not enabled
        if strictMode = UnknownQueryParamStrictMode.Ignore then
            Ok()
        else
            let inRequest =
                httpCtx.Request.Query.Keys
                |> Set.ofSeq
                // Ignore sparse fieldset, include, and skip link parameters
                |> Set.filter (fun s ->
                    s <> "include"
                    && not (req.Fieldsets.Keys |> Seq.exists (fun tn -> "fields[" + tn + "]" = s))
                    && not (linkConfig.QueryParamNames |> Seq.contains s))

            let inRequestButNotConsumed = inRequest - consumed

            if inRequestButNotConsumed.IsEmpty then
                Ok()
            else
                match strictMode with
                | UnknownQueryParamStrictMode.Ignore -> Ok()
                | UnknownQueryParamStrictMode.Warn logLevel ->
                    inRequestButNotConsumed
                    |> Seq.iter (fun paramName ->
                        let logger = httpCtx.GetService<ILoggerFactory>().CreateLogger "Felicity.StrictMode"
                        logger.Log(logLevel, "Request contained unknown query parameter {ParamName}", paramName))

                    Ok()
                | UnknownQueryParamStrictMode.Error ->
                    inRequestButNotConsumed
                    |> Set.toList
                    |> List.map strictModeUnknownOrUnusedQueryParam
                    |> Error



type internal Preconditions<'ctx> =
    abstract Validate: HttpContext -> 'ctx -> BoxedEntity -> Result<unit, Error list>


module internal Preconditions =

    let noop =
        { new Preconditions<'ctx> with
            member _.Validate _ _ _ = Ok()
        }


type Preconditions<'ctx, 'entity> = internal {

    getETag: 'ctx * 'entity -> EntityTagHeaderValue option
    getLastModified: 'ctx * 'entity -> DateTimeOffset option
    isOptional: bool
} with

    static member internal Create() : Preconditions<'ctx, 'entity> = {
        getETag = fun _ -> None
        getLastModified = fun _ -> None
        isOptional = false
    }


    interface Preconditions<'ctx> with
        member this.Validate httpCtx ctx entity =
            PreconditionHelpers.validate
                httpCtx
                (ctx, unbox<'entity> entity)
                this.getETag
                this.getLastModified
                this.isOptional

    member this.ETag(getETag: Func<'ctx, 'entity, EntityTagHeaderValue>) = {
        this with
            getETag = fun (ctx, e) -> getETag.Invoke(ctx, e) |> Some
    }

    member this.ETag(getETag: Func<'entity, EntityTagHeaderValue>) = this.ETag(fun _ e -> getETag.Invoke e)

    member this.LastModified(getLastModified: Func<'ctx, 'entity, DateTimeOffset>) = {
        this with
            getLastModified = fun (ctx, e) -> getLastModified.Invoke(ctx, e) |> Some
    }

    member this.LastModified(getLastModified: Func<'entity, DateTimeOffset>) =
        this.LastModified(fun _ e -> getLastModified.Invoke e)

    member this.Optional = { this with isOptional = true }


type internal ResSpecificResourceLookup<'ctx> =
    abstract GetByIdBoxed:
        'ctx -> ResourceId -> Task<Result<(ResourceDefinition<'ctx> * BoxedEntity) option, Error list>>


type ResourceLookup<'ctx, 'entity, 'id> =
    abstract GetById: 'ctx -> 'id -> Task<Result<'entity option, Error list>>


type internal ResourceLookup<'ctx> =
    abstract GetByIdBoxed:
        ResourceDefinition<'ctx> -> 'ctx -> ResourceId -> Task<Result<BoxedEntity option, Error list>>


type ResourceLookup<'originalCtx, 'ctx, 'entity, 'id> = internal {
    mapCtx: 'originalCtx -> Task<Result<'ctx, Error list>>
    getById: 'ctx -> 'id -> Task<Result<'entity option, Error list>>
} with

    static member internal Create(mapCtx, getById) : ResourceLookup<'originalCtx, 'ctx, 'entity, 'id> = {
        mapCtx = mapCtx
        getById = getById
    }

    interface ResourceLookup<'ctx, 'entity, 'id> with
        member this.GetById ctx id = this.getById ctx id

    interface ResourceLookup<'originalCtx> with
        member this.GetByIdBoxed rDef ctx rawId =
            task {
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
    abstract GetByIdBoxed:
        ResourceDefinition<'ctx> ->
        'ctx ->
        ResourceId ->
            Task<Result<(ResourceDefinition<'ctx> * BoxedEntity) option, Error list>>


type PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id> = internal {
    mapCtx: 'originalCtx -> Task<Result<'ctx, Error list>>
    getById: 'ctx -> 'id -> Task<Result<'entity option, Error list>>
    getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
} with

    static member internal Create
        (
            mapCtx,
            getById,
            getPolyBuilder
        ) : PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id> =
        {
            mapCtx = mapCtx
            getById = getById
            getPolyBuilder = getPolyBuilder
        }

    interface ResourceLookup<'ctx, 'entity, 'id> with
        member this.GetById ctx id = this.getById ctx id

    interface PolymorphicResourceLookup<'originalCtx> with
        member this.GetByIdBoxed rDef ctx rawId =
            task {
                match! rDef.ParseIdBoxed ctx rawId with
                // Ignore ID parsing errors; in the context of fetching a resource by ID, this
                // just means that the resource does not exist, which is a more helpful result
                | Error _ -> return Ok None
                | Ok domainId ->
                    match! this.mapCtx ctx with
                    | Error errs -> return Error errs
                    | Ok mappedCtx ->
                        let! entity = domainId |> unbox<'id> |> this.getById mappedCtx

                        return
                            entity
                            |> Result.map (
                                Option.map (fun e ->
                                    let b = this.getPolyBuilder e
                                    b.resourceDef, b.entity)
                            )
            }



type internal GetResourceOperation<'ctx> =
    abstract Run: ResourceDefinition<'ctx> -> 'ctx -> Request -> BoxedEntity -> ResponseBuilder<'ctx> -> HttpHandler


type GetResourceOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
    mapCtx: 'originalCtx -> 'entity -> Task<Result<'ctx, Error list>>
    modifyResponse: 'ctx -> 'entity -> HttpHandler
} with

    static member internal Create(mapCtx) : GetResourceOperation<'originalCtx, 'ctx, 'entity, 'id> = {
        mapCtx = mapCtx
        modifyResponse = fun _ _ -> fun next ctx -> next ctx
    }

    interface GetResourceOperation<'originalCtx> with
        member this.Run resDef ctx req entity resp =
            fun next httpCtx ->
                task {
                    match! this.mapCtx ctx (unbox<'entity> entity) with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok mappedCtx ->
                        match StrictModeHelpers.checkForUnknownQueryParameters<'originalCtx> httpCtx req Set.empty with
                        | Error errs -> return! handleErrors errs next httpCtx
                        | Ok() ->
                            let! doc = resp.Write httpCtx ctx req (resDef, entity)

                            let! fieldTrackerHandler =
                                httpCtx.RequestServices
                                    .GetRequiredService<FieldTracker<'originalCtx>>()
                                    .TrackFields([ resDef.TypeName ], ctx, req)

                            let handler =
                                setStatusCode 200
                                >=> this.modifyResponse mappedCtx (unbox<'entity> entity)
                                >=> fieldTrackerHandler
                                >=> jsonApiWithETag<'originalCtx> doc

                            return! handler next httpCtx
                }

    member this.ModifyResponse(getHandler: 'ctx -> 'entity -> HttpHandler) = {
        this with
            modifyResponse = getHandler
    }

    member this.ModifyResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
        this.ModifyResponse(fun ctx e ->
            (fun next httpCtx ->
                f ctx e httpCtx
                next httpCtx))

    member this.ModifyResponse(handler: HttpHandler) = this.ModifyResponse(fun _ _ -> handler)



type internal GetCollectionOperation<'ctx> =
    abstract Run: ResourceDefinition<'ctx> -> 'ctx -> Request -> ResponseBuilder<'ctx> -> HttpHandler


type GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
    mapCtx: 'originalCtx -> Task<Result<'ctx, Error list>>
    getCollection:
        'originalCtx
            -> 'ctx
            -> Request
            -> Task<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity list, Error list>>
    modifyResponse: 'ctx -> 'entity list -> HttpHandler
} with

    static member internal Create(mapCtx, getCollection) : GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> = {
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
                        match! this.getCollection ctx mappedCtx req with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok(_, queryNames, entities) ->
                            match httpCtx.TryGetQueryStringValue "sort", queryNames.Contains "sort" with
                            | Some _, false -> return! handleErrors [ sortNotSupported () ] next httpCtx
                            | _ ->
                                match
                                    StrictModeHelpers.checkForUnknownQueryParameters<'originalCtx>
                                        httpCtx
                                        req
                                        queryNames
                                with
                                | Error errs -> return! handleErrors errs next httpCtx
                                | Ok() ->
                                    let! doc =
                                        resp.WriteList httpCtx ctx req (entities |> List.map (fun e -> resDef, e))

                                    let! fieldTrackerHandler =
                                        httpCtx.RequestServices
                                            .GetRequiredService<FieldTracker<'originalCtx>>()
                                            .TrackFields([ resDef.TypeName ], ctx, req)

                                    let handler =
                                        setStatusCode 200
                                        >=> this.modifyResponse mappedCtx entities
                                        >=> fieldTrackerHandler
                                        >=> jsonApiWithETag<'originalCtx> doc

                                    return! handler next httpCtx
                }

    member this.ModifyResponse(getHandler: 'ctx -> 'entity list -> HttpHandler) = {
        this with
            modifyResponse = getHandler
    }

    member this.ModifyResponse(f: 'ctx -> 'entity list -> HttpContext -> unit) =
        this.ModifyResponse(fun ctx es ->
            (fun next httpCtx ->
                f ctx es httpCtx
                next httpCtx))

    member this.ModifyResponse(handler: HttpHandler) = this.ModifyResponse(fun _ _ -> handler)


type internal PolymorphicGetCollectionOperation<'ctx> =
    abstract Run: ResourceTypeName list -> 'ctx -> Request -> ResponseBuilder<'ctx> -> HttpHandler


type PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> = internal {
    mapCtx: 'originalCtx -> Task<Result<'ctx, Error list>>
    getCollection:
        'originalCtx
            -> 'ctx
            -> Request
            -> Task<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity list, Error list>>
    getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
    modifyResponse: 'ctx -> 'entity list -> HttpHandler
    polymorphicResourceTypesForFieldTracking: ResourceTypeName list
} with

    static member internal Create
        (
            mapCtx,
            getCollection,
            getPolyBuilder
        ) : PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id> =
        {
            mapCtx = mapCtx
            getCollection = getCollection
            getPolyBuilder = getPolyBuilder
            modifyResponse = fun _ _ -> fun next ctx -> next ctx
            polymorphicResourceTypesForFieldTracking = []
        }

    interface PolymorphicGetCollectionOperation<'originalCtx> with
        member this.Run collectionResTypes ctx req resp =
            fun next httpCtx ->
                task {
                    match! this.mapCtx ctx with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok mappedCtx ->
                        match! this.getCollection ctx mappedCtx req with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok(_, queryNames, entities) ->
                            match httpCtx.TryGetQueryStringValue "sort", queryNames.Contains "sort" with
                            | Some _, false -> return! handleErrors [ sortNotSupported () ] next httpCtx
                            | _ ->
                                match
                                    StrictModeHelpers.checkForUnknownQueryParameters<'originalCtx>
                                        httpCtx
                                        req
                                        queryNames
                                with
                                | Error errs -> return! handleErrors errs next httpCtx
                                | Ok() ->
                                    let! doc =
                                        resp.WriteList
                                            httpCtx
                                            ctx
                                            req
                                            (entities
                                             |> List.map this.getPolyBuilder
                                             |> List.map (fun b -> b.resourceDef, b.entity))

                                    let collectionResTypes =
                                        if this.polymorphicResourceTypesForFieldTracking.IsEmpty then
                                            collectionResTypes
                                        else
                                            this.polymorphicResourceTypesForFieldTracking

                                    let! fieldTrackerHandler =
                                        httpCtx.RequestServices
                                            .GetRequiredService<FieldTracker<'originalCtx>>()
                                            .TrackFields(collectionResTypes, ctx, req)

                                    let handler =
                                        setStatusCode 200
                                        >=> this.modifyResponse mappedCtx entities
                                        >=> fieldTrackerHandler
                                        >=> jsonApiWithETag<'originalCtx> doc

                                    return! handler next httpCtx
                }

    member this.ModifyResponse(getHandler: 'ctx -> 'entity list -> HttpHandler) = {
        this with
            modifyResponse = getHandler
    }

    member this.ModifyResponse(f: 'ctx -> 'entity list -> HttpContext -> unit) =
        this.ModifyResponse(fun ctx es ->
            (fun next httpCtx ->
                f ctx es httpCtx
                next httpCtx))

    member this.ModifyResponse(handler: HttpHandler) = this.ModifyResponse(fun _ _ -> handler)

    /// If you use field tracking (i.e., have called TrackFieldUsage when configuring Felicity) and at least one of the
    /// resource types that can be returned by this collection belongs to another collection (i.e., does not have the same
    /// collection name as the resource definition this operation belongs to), call this method for each resource type
    /// that can be returned by this operation. Otherwise, field tracking won't work properly for this operation.
    member this.RegisterResourceType(resourceDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>) = {
        this with
            polymorphicResourceTypesForFieldTracking =
                resourceDef.name :: this.polymorphicResourceTypesForFieldTracking
                |> List.distinct
    }


type internal PostOperation<'ctx> =
    abstract Run:
        CollectionName ->
        ResourceDefinition<'ctx> ->
        'ctx ->
        Request ->
        BoxedPatcher<'ctx> ->
        ResponseBuilder<'ctx> ->
            HttpHandler

    abstract HasPersist: bool


type PostOperation<'originalCtx, 'ctx, 'entity> = internal {
    mapCtx: 'originalCtx -> Request -> Task<Result<'ctx, Error list>>
    create:
        'originalCtx
            -> 'ctx
            -> Request
            -> Task<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity, Error list>>
    afterCreate: ('ctx -> 'entity -> Task<Result<'entity, Error list>>) option
    modifyResponse: 'ctx -> 'entity -> HttpHandler
    return202Accepted: bool
    validatePreconditions: bool
    preconditionsOptional: bool
    getETag: 'ctx -> EntityTagHeaderValue option
    getLastModified: 'ctx -> DateTimeOffset option
} with

    static member internal Create(mapCtx, create) : PostOperation<'originalCtx, 'ctx, 'entity> = {
        mapCtx = mapCtx
        create = create
        afterCreate = None
        modifyResponse = fun _ _ -> fun next ctx -> next ctx
        return202Accepted = false
        validatePreconditions = false
        preconditionsOptional = false
        getETag = fun _ -> None
        getLastModified = fun _ -> None
    }

    interface PostOperation<'originalCtx> with
        member this.HasPersist = this.afterCreate.IsSome

        member this.Run collName rDef ctx req patch resp =
            fun next httpCtx ->
                let afterCreate =
                    this.afterCreate
                    |> Option.defaultWith (fun () ->
                        failwithf
                            "Framework bug: POST operation defined without AfterCreate. This should be caught at startup.")

                task {
                    match! this.mapCtx ctx req with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok mappedCtx ->
                        let validatePreconditions () =
                            if this.validatePreconditions then
                                PreconditionHelpers.validate
                                    httpCtx
                                    mappedCtx
                                    this.getETag
                                    this.getLastModified
                                    this.preconditionsOptional
                            else
                                Ok()

                        match validatePreconditions () with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok() ->
                            match!
                                this.create ctx mappedCtx req
                                |> TaskResult.bind (fun (fieldNames, qns, e) ->
                                    box e
                                    |> patch ctx req fieldNames
                                    |> TaskResult.map (fun (e, fns) -> Set.union fns fieldNames, qns, e))
                            with
                            | Error errors -> return! handleErrors errors next httpCtx
                            | Ok(ns, queryNames, entity0) ->
                                match req.Document.Value with
                                | Ok(Some { data = Some { id = Include _ } }) when not <| ns.Contains "id" ->
                                    return!
                                        handleErrors [ collPostClientIdNotAllowed collName rDef.TypeName ] next httpCtx
                                | _ ->
                                    match
                                        StrictModeHelpers.checkForUnknownQueryParameters<'originalCtx>
                                            httpCtx
                                            req
                                            queryNames
                                    with
                                    | Error errs -> return! handleErrors errs next httpCtx
                                    | Ok() ->
                                        match! afterCreate mappedCtx (unbox<'entity> entity0) with
                                        | Error errors -> return! handleErrors errors next httpCtx
                                        | Ok entity1 ->
                                            if this.return202Accepted then
                                                let! fieldTrackerHandler =
                                                    httpCtx.RequestServices
                                                        .GetRequiredService<FieldTracker<'originalCtx>>()
                                                        .TrackFields(
                                                            [],
                                                            ctx,
                                                            req,
                                                            consumedFieldNamesWithType = (rDef.TypeName, ns)
                                                        )

                                                let handler =
                                                    setStatusCode 202
                                                    >=> this.modifyResponse mappedCtx (unbox<'entity> entity1)
                                                    >=> fieldTrackerHandler

                                                return! handler next httpCtx
                                            else
                                                let! doc = resp.Write httpCtx ctx req (rDef, entity1)

                                                let setLocationHeader =
                                                    match doc with
                                                    | {
                                                          data = Some { links = Include links }
                                                      } ->
                                                        match links.TryGetValue "self" with
                                                        | true, { href = Some url } ->
                                                            setHttpHeader "Location" (url.ToString())
                                                        | _ -> fun next ctx -> next ctx
                                                    | _ -> fun next ctx -> next ctx

                                                let! fieldTrackerHandler =
                                                    httpCtx.RequestServices
                                                        .GetRequiredService<FieldTracker<'originalCtx>>()
                                                        .TrackFields(
                                                            [ rDef.TypeName ],
                                                            ctx,
                                                            req,
                                                            consumedFieldNamesWithType = (rDef.TypeName, ns)
                                                        )

                                                let handler =
                                                    setStatusCode 201
                                                    >=> setLocationHeader
                                                    >=> this.modifyResponse mappedCtx entity1
                                                    >=> fieldTrackerHandler
                                                    >=> jsonApiWithETag<'originalCtx> doc

                                                return! handler next httpCtx
                }

    member this.AfterCreateTaskRes(f: Func<'ctx, 'entity, Task<Result<'entity, Error list>>>) = {
        this with
            afterCreate = Some(fun ctx e -> f.Invoke(ctx, e))
    }

    member this.AfterCreateTaskRes(f: Func<'ctx, 'entity, Task<Result<unit, Error list>>>) =
        this.AfterCreateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> TaskResult.map (fun () -> e))

    member this.AfterCreateTaskRes(f: Func<'entity, Task<Result<'entity, Error list>>>) =
        this.AfterCreateTaskRes(fun _ e -> f.Invoke e)

    member this.AfterCreateTaskRes(f: Func<'entity, Task<Result<unit, Error list>>>) =
        this.AfterCreateTaskRes(fun _ e -> f.Invoke e |> TaskResult.map (fun () -> e))

    member this.AfterCreateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
        this.AfterCreateTaskRes(Task.liftAsyncFunc2 f)

    member this.AfterCreateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
        this.AfterCreateTaskRes(Task.liftAsyncFunc2 f)

    member this.AfterCreateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
        this.AfterCreateTaskRes(Task.liftAsyncFunc f)

    member this.AfterCreateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
        this.AfterCreateTaskRes(Task.liftAsyncFunc f)

    member this.AfterCreateTask(f: Func<'ctx, 'entity, Task<'entity>>) =
        this.AfterCreateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.AfterCreateTask(f: Func<'ctx, 'entity, Task<unit>>) =
        this.AfterCreateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.AfterCreateTask(f: Func<'entity, Task<'entity>>) =
        this.AfterCreateTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.AfterCreateTask(f: Func<'entity, Task<unit>>) =
        this.AfterCreateTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.AfterCreateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
        this.AfterCreateTask(Task.liftAsyncFunc2 f)

    member this.AfterCreateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
        this.AfterCreateTask(Task.liftAsyncFunc2 f)

    member this.AfterCreateAsync(f: Func<'entity, Async<'entity>>) =
        this.AfterCreateTask(Task.liftAsyncFunc f)

    member this.AfterCreateAsync(f: Func<'entity, Async<unit>>) =
        this.AfterCreateTask(Task.liftAsyncFunc f)

    member this.AfterCreateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
        this.AfterCreateTaskRes(Task.liftFunc2 f)

    member this.AfterCreateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
        this.AfterCreateTaskRes(Task.liftFunc2 f)

    member this.AfterCreateRes(f: Func<'entity, Result<'entity, Error list>>) =
        this.AfterCreateTaskRes(Task.liftFunc f)

    member this.AfterCreateRes(f: Func<'entity, Result<unit, Error list>>) =
        this.AfterCreateTaskRes(Task.liftFunc f)

    member this.AfterCreate(f: Func<'ctx, 'entity, 'entity>) =
        this.AfterCreateTaskRes(TaskResult.liftFunc2 f)

    member this.AfterCreate(f: Func<'ctx, 'entity, unit>) =
        this.AfterCreateTaskRes(TaskResult.liftFunc2 f)

    member this.AfterCreate(f: Func<'entity, 'entity>) =
        this.AfterCreateTaskRes(TaskResult.liftFunc f)

    member this.AfterCreate(f: Func<'entity, unit>) =
        this.AfterCreateTaskRes(TaskResult.liftFunc f)

    member this.Return202Accepted() = { this with return202Accepted = true }

    member this.ModifyResponse(getHandler: 'ctx -> 'entity -> HttpHandler) = {
        this with
            modifyResponse = getHandler
    }

    member this.ModifyResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
        this.ModifyResponse(fun ctx e ->
            (fun next httpCtx ->
                f ctx e httpCtx
                next httpCtx))

    member this.ModifyResponse(handler: HttpHandler) = this.ModifyResponse(fun _ _ -> handler)

    /// Validate preconditions using the specified function to get the ETag value. May be
    /// combined with PreconditionsLastModified.
    member this.PreconditionsETag(getETag: Func<'ctx, EntityTagHeaderValue>) = {
        this with
            validatePreconditions = true
            getETag = fun ctx -> getETag.Invoke(ctx) |> Some
    }

    /// Validate preconditions using the specified function to get the LastModified value.
    /// May be combined with PreconditionsETag.
    member this.PreconditionsLastModified(getLastModified: Func<'ctx, DateTimeOffset>) = {
        this with
            validatePreconditions = true
            getLastModified = fun ctx -> getLastModified.Invoke(ctx) |> Some
    }

    /// Make optional any preconditions supplied using PreconditionsETag or
    /// PreconditionsLastModified. By default, any supplied preconditions are required.
    member this.PreconditionsOptional = {
        this with
            preconditionsOptional = true
    }

type PostCustomHelper<'ctx, 'entity>
    internal
    (
        httpCtx: HttpContext,
        ctx: 'ctx,
        req: Request,
        collName: string,
        rDef: ResourceDefinition<'ctx>,
        patcher: BoxedPatcher<'ctx>,
        builder: ResponseBuilder<'ctx>
    ) =

    member val internal ConsumedFieldNames: Set<ConsumedFieldName> = Set.empty with get, set
    member val internal ConsumedQueryParams: Set<ConsumedQueryParamName> = Set.empty with get, set

    member this.ValidateRequest(?parser: RequestParser<'originalCtx, 'a>) =
        parser
        |> Option.iter (fun p ->
            this.ConsumedFieldNames <- Set.union this.ConsumedFieldNames (Set.ofSeq p.consumedFields)
            this.ConsumedQueryParams <- Set.union this.ConsumedQueryParams (Set.ofSeq p.consumedParams))

        let idParsed =
            parser
            |> Option.map (fun p -> Set.ofSeq p.consumedFields)
            |> Option.defaultValue Set.empty
            |> Set.contains "id"

        match req.Document.Value with
        | Ok(Some { data = Some { id = Include _ } }) when not idParsed ->
            Error [ collPostClientIdNotAllowed collName rDef.TypeName ]
        | _ -> Ok()

    member this.RunSettersTask(entity: 'entity, ?parser: RequestParser<'originalCtx, 'a>) =
        parser
        |> Option.iter (fun p ->
            this.ConsumedFieldNames <- Set.union this.ConsumedFieldNames (Set.ofSeq p.consumedFields)
            this.ConsumedQueryParams <- Set.union this.ConsumedQueryParams (Set.ofSeq p.consumedParams))

        patcher
            ctx
            req
            (parser
             |> Option.map (fun p -> Set.ofSeq p.consumedFields)
             |> Option.defaultValue Set.empty)
            (box entity)
        |> Task.map (
            Result.map (fun (e, setFields) ->
                this.ConsumedFieldNames <- Set.union this.ConsumedFieldNames setFields
                unbox<'entity> e)
        )

    member this.RunSettersAsync(entity: 'entity, ?parser: RequestParser<'originalCtx, 'a>) =
        this.RunSettersTask(entity, ?parser = parser) |> Task.toAsync

    member this.Return202Accepted() : HttpHandler =
        fun next httpCtx ->
            task {
                let! fieldTrackerHandler =
                    httpCtx.RequestServices
                        .GetRequiredService<FieldTracker<'ctx>>()
                        .TrackFields(
                            [],
                            ctx,
                            req,
                            consumedFieldNamesWithType = (rDef.TypeName, this.ConsumedFieldNames)
                        )

                return! (setStatusCode 202 >=> fieldTrackerHandler) next httpCtx
            }

    member this.ReturnCreatedEntity(entity: 'entity) : HttpHandler =
        fun next httpCtx ->
            task {
                let! doc = builder.Write httpCtx ctx req (rDef, entity)

                let setLocationHeader =
                    match doc with
                    | {
                          data = Some { links = Include links }
                      } ->
                        match links.TryGetValue "self" with
                        | true, { href = Some url } -> setHttpHeader "Location" (url.ToString())
                        | _ -> fun next ctx -> next ctx
                    | _ -> fun next ctx -> next ctx

                let! fieldTrackerHandler =
                    httpCtx.RequestServices
                        .GetRequiredService<FieldTracker<'ctx>>()
                        .TrackFields(
                            [ rDef.TypeName ],
                            ctx,
                            req,
                            consumedFieldNamesWithType = (rDef.TypeName, this.ConsumedFieldNames)
                        )

                let handler =
                    setStatusCode 201
                    >=> setLocationHeader
                    >=> fieldTrackerHandler
                    >=> jsonApiWithETag<'ctx> doc

                return! handler next httpCtx
            }

    /// Validate preconditions using the specified ETag value. The preconditions are
    /// required unless passing `isOptional = true`.
    member _.ValidatePreconditions(eTag: EntityTagHeaderValue, ?isOptional: bool) =
        PreconditionHelpers.validate httpCtx ctx (fun _ -> Some eTag) (fun _ -> None) (defaultArg isOptional false)

    /// Validate preconditions using the specified LastModified value. The preconditions are
    /// required unless passing `isOptional = true`.
    member _.ValidatePreconditions(lastModified: DateTimeOffset, ?isOptional: bool) =
        PreconditionHelpers.validate
            httpCtx
            ctx
            (fun _ -> None)
            (fun _ -> Some lastModified)
            (defaultArg isOptional false)

    /// Validate preconditions using the specified ETag and LastModified values. The
    /// preconditions are required unless passing `isOptional = true`.
    member _.ValidatePreconditions(eTag: EntityTagHeaderValue, lastModified: DateTimeOffset, ?isOptional: bool) =
        PreconditionHelpers.validate
            httpCtx
            ctx
            (fun _ -> Some eTag)
            (fun _ -> Some lastModified)
            (defaultArg isOptional false)

    member this.ValidateStrictModeQueryParameters() =
        StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req this.ConsumedQueryParams



type CustomPostOperation<'originalCtx, 'ctx, 'entity> = internal {
    mapCtx: 'originalCtx -> Task<Result<'ctx, Error list>>
    operation:
        'originalCtx
            -> 'ctx
            -> Request
            -> PostCustomHelper<'originalCtx, 'entity>
            -> Task<Result<HttpHandler, Error list>>
} with

    static member internal Create(mapCtx, operation) : CustomPostOperation<'originalCtx, 'ctx, 'createResult> = {
        mapCtx = mapCtx
        operation = operation
    }

    interface PostOperation<'originalCtx> with
        member _.HasPersist = true

        member this.Run collName rDef ctx req patch resp =
            fun next httpCtx ->
                task {
                    match! this.mapCtx ctx with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok mappedCtx ->
                        let helper =
                            PostCustomHelper<'originalCtx, 'entity>(httpCtx, ctx, req, collName, rDef, patch, resp)

                        match! this.operation ctx mappedCtx req helper with
                        | Ok handler -> return! handler next httpCtx
                        | Error errors -> return! handleErrors errors next httpCtx
                }



type internal PatchOperation<'ctx> =
    abstract HasPersist: bool

    abstract Run:
        ResourceDefinition<'ctx> ->
        'ctx ->
        Request ->
        Preconditions<'ctx> ->
        BoxedEntity ->
        BoxedPatcher<'ctx> ->
        ResponseBuilder<'ctx> ->
            HttpHandler


type PatchOperation<'originalCtx, 'ctx, 'entity> = internal {
    mapCtx: 'originalCtx -> 'entity -> Task<Result<'ctx, Error list>>
    beforeUpdate: 'ctx -> 'entity -> Task<Result<'entity, Error list>>
    customSetter:
        'originalCtx
            -> 'ctx
            -> Request
            -> 'entity
            -> Task<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'entity, Error list>>
    afterUpdate: ('ctx -> 'entity -> 'entity -> Task<Result<'entity, Error list>>) option
    modifyResponse: 'ctx -> 'entity -> HttpHandler
    return202Accepted: bool
} with

    static member internal Create mapCtx : PatchOperation<'originalCtx, 'ctx, 'entity> = {
        mapCtx = mapCtx
        beforeUpdate = fun _ x -> Ok x |> Task.result
        customSetter = fun _ _ _ e -> Ok(Set.empty, Set.empty, e) |> Task.result
        afterUpdate = None
        modifyResponse = fun _ _ -> fun next ctx -> next ctx
        return202Accepted = false
    }


    interface PatchOperation<'originalCtx> with
        member this.HasPersist = this.afterUpdate.IsSome

        member this.Run rDef ctx req preconditions entity0 patch resp =
            let afterUpdate =
                this.afterUpdate
                |> Option.defaultWith (fun () ->
                    failwithf
                        "Framework bug: PATCH operation defined without AfterUpdate. This should be caught at startup.")

            fun next httpCtx ->
                task {
                    let errs = [
                        match req.Document.Value with
                        | Error errs -> yield! errs
                        | Ok None -> resPatchMissingResourceObject ""
                        | Ok(Some { data = None }) -> resPatchMissingResourceObject "/data"
                        | Ok(Some { data = Some res }) ->
                            if res.``type`` <> rDef.TypeName then
                                resPatchTypeMismatch res.``type`` rDef.TypeName
                                |> Error.setSourcePointer "/data/type"

                            match res.id with
                            | Skip -> resPatchMissingResourceId "/data"
                            | Include id when id <> rDef.GetIdBoxed entity0 ->
                                resPatchIdMismatch id (rDef.GetIdBoxed entity0)
                                |> Error.setSourcePointer "/data/id"
                            | Include _ -> ()
                    ]

                    if not errs.IsEmpty then
                        return! handleErrors errs next httpCtx
                    else
                        match! this.mapCtx ctx (unbox<'entity> entity0) with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok mappedCtx ->
                            match preconditions.Validate httpCtx ctx entity0 with
                            | Error errors -> return! handleErrors errors next httpCtx
                            | Ok() ->
                                match! this.beforeUpdate mappedCtx (unbox<'entity> entity0) with
                                | Error errors -> return! handleErrors errors next httpCtx
                                | Ok entity1 ->
                                    match! this.customSetter ctx mappedCtx req entity1 with
                                    | Error errors -> return! handleErrors errors next httpCtx
                                    | Ok(fns, queryNames, entity2) ->
                                        match
                                            StrictModeHelpers.checkForUnknownQueryParameters<'originalCtx>
                                                httpCtx
                                                req
                                                queryNames
                                        with
                                        | Error errs -> return! handleErrors errs next httpCtx
                                        | Ok() ->
                                            let setFns =
                                                fns
                                                |> Set.filter (fun fn ->
                                                    match req.Document.Value with
                                                    | Ok(Some {
                                                                  data = Some { attributes = Include attrVals }
                                                              }) -> attrVals.ContainsKey fn
                                                    | _ -> false)

                                            match! patch ctx req fns entity2 with
                                            | Error errors -> return! handleErrors errors next httpCtx
                                            | Ok(entity3, patchedFns) ->
                                                match!
                                                    afterUpdate
                                                        mappedCtx
                                                        (unbox<'entity> entity0)
                                                        (unbox<'entity> entity3)
                                                with
                                                | Error errors -> return! handleErrors errors next httpCtx
                                                | Ok entity4 ->
                                                    if this.return202Accepted then
                                                        let! fieldTrackerHandler =
                                                            httpCtx.RequestServices
                                                                .GetRequiredService<FieldTracker<'originalCtx>>()
                                                                .TrackFields(
                                                                    [],
                                                                    ctx,
                                                                    req,
                                                                    consumedFieldNamesWithType =
                                                                        (rDef.TypeName, Set.union setFns patchedFns)
                                                                )

                                                        let handler =
                                                            setStatusCode 202
                                                            >=> this.modifyResponse mappedCtx (unbox<'entity> entity4)
                                                            >=> fieldTrackerHandler

                                                        return! handler next httpCtx
                                                    else
                                                        let! doc = resp.Write httpCtx ctx req (rDef, entity4)

                                                        let! fieldTrackerHandler =
                                                            httpCtx.RequestServices
                                                                .GetRequiredService<FieldTracker<'originalCtx>>()
                                                                .TrackFields(
                                                                    [ rDef.TypeName ],
                                                                    ctx,
                                                                    req,
                                                                    consumedFieldNamesWithType =
                                                                        (rDef.TypeName, Set.union setFns patchedFns)
                                                                )

                                                        let handler =
                                                            setStatusCode 200
                                                            >=> this.modifyResponse mappedCtx entity4
                                                            >=> fieldTrackerHandler
                                                            >=> jsonApiWithETag<'originalCtx> doc

                                                        return! handler next httpCtx
                }


    member this.BeforeUpdateTaskRes(f: Func<'ctx, 'entity, Task<Result<'entity, Error list>>>) = {
        this with
            beforeUpdate = (fun ctx e -> f.Invoke(ctx, e))
    }

    member this.BeforeUpdateTaskRes(f: Func<'ctx, 'entity, Task<Result<unit, Error list>>>) =
        this.BeforeUpdateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> TaskResult.map (fun () -> e))

    member this.BeforeUpdateTaskRes(f: Func<'entity, Task<Result<'entity, Error list>>>) =
        this.BeforeUpdateTaskRes(fun _ e -> f.Invoke e)

    member this.BeforeUpdateTaskRes(f: Func<'entity, Task<Result<unit, Error list>>>) =
        this.BeforeUpdateTaskRes(fun _ e -> f.Invoke e)

    member this.BeforeUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
        this.BeforeUpdateTaskRes(Task.liftAsyncFunc2 f)

    member this.BeforeUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
        this.BeforeUpdateTaskRes(Task.liftAsyncFunc2 f)

    member this.BeforeUpdateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
        this.BeforeUpdateTaskRes(Task.liftAsyncFunc f)

    member this.BeforeUpdateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
        this.BeforeUpdateTaskRes(Task.liftAsyncFunc f)

    member this.BeforeUpdateTask(f: Func<'ctx, 'entity, Task<'entity>>) =
        this.BeforeUpdateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.BeforeUpdateTask(f: Func<'ctx, 'entity, Task<unit>>) =
        this.BeforeUpdateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.BeforeUpdateTask(f: Func<'entity, Task<'entity>>) =
        this.BeforeUpdateTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.BeforeUpdateTask(f: Func<'entity, Task<unit>>) =
        this.BeforeUpdateTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.BeforeUpdateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
        this.BeforeUpdateTask(Task.liftAsyncFunc2 f)

    member this.BeforeUpdateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
        this.BeforeUpdateTask(Task.liftAsyncFunc2 f)

    member this.BeforeUpdateAsync(f: Func<'entity, Async<'entity>>) =
        this.BeforeUpdateTask(Task.liftAsyncFunc f)

    member this.BeforeUpdateAsync(f: Func<'entity, Async<unit>>) =
        this.BeforeUpdateTask(Task.liftAsyncFunc f)

    member this.BeforeUpdateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
        this.BeforeUpdateTaskRes(Task.liftFunc2 f)

    member this.BeforeUpdateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
        this.BeforeUpdateTaskRes(Task.liftFunc2 f)

    member this.BeforeUpdateRes(f: Func<'entity, Result<'entity, Error list>>) =
        this.BeforeUpdateTaskRes(Task.liftFunc f)

    member this.BeforeUpdateRes(f: Func<'entity, Result<unit, Error list>>) =
        this.BeforeUpdateTaskRes(Task.liftFunc f)

    member this.BeforeUpdate(f: Func<'ctx, 'entity, 'entity>) =
        this.BeforeUpdateTaskRes(TaskResult.liftFunc2 f)

    member this.BeforeUpdate(f: Func<'ctx, 'entity, unit>) =
        this.BeforeUpdateTaskRes(TaskResult.liftFunc2 f)

    member this.BeforeUpdate(f: Func<'entity, 'entity>) =
        this.BeforeUpdateTaskRes(TaskResult.liftFunc f)

    member this.BeforeUpdate(f: Func<'entity, unit>) =
        this.BeforeUpdateTaskRes(TaskResult.liftFunc f)

    member this.AddCustomSetterTaskRes
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Task<Result<RequestParser<'originalCtx, 'entity>, Error list>>>)
        =
        {
            this with
                customSetter =
                    fun origCtx ctx req e ->
                        this.customSetter origCtx ctx req e
                        |> TaskResult.bind (fun (fns, qns, e) ->
                            getRequestParser.Invoke(ctx, e, RequestParserHelper<'originalCtx>(origCtx, req))
                            |> TaskResult.bind (fun p -> p.ParseWithConsumed())
                            |> TaskResult.map (fun (fns', qns', e) -> Set.union fns fns', Set.union qns qns', e))
        }

    member this.AddCustomSetterAsyncRes
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Async<Result<RequestParser<'originalCtx, 'entity>, Error list>>>)
        =
        this.AddCustomSetterTaskRes(Task.liftAsyncFunc3 getRequestParser)

    member this.AddCustomSetterTask
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Task<RequestParser<'originalCtx, 'entity>>>)
        =
        this.AddCustomSetterTaskRes(fun ctx e parser -> getRequestParser.Invoke(ctx, e, parser) |> Task.map Ok)

    member this.AddCustomSetterAsync
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Async<RequestParser<'originalCtx, 'entity>>>)
        =
        this.AddCustomSetterTask(Task.liftAsyncFunc3 getRequestParser)

    member this.AddCustomSetterRes
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Result<RequestParser<'originalCtx, 'entity>, Error list>>)
        =
        this.AddCustomSetterTaskRes(Task.liftFunc3 getRequestParser)

    member this.AddCustomSetter
        (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, RequestParser<'originalCtx, 'entity>>)
        =
        this.AddCustomSetterTaskRes(TaskResult.liftFunc3 getRequestParser)

    member this.AfterUpdateTaskRes(f: Func<'ctx, 'entity, 'entity, Task<Result<'entity, Error list>>>) = {
        this with
            afterUpdate = Some(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew))
    }

    member this.AfterUpdateTaskRes(f: Func<'entity, 'entity, Task<Result<'entity, Error list>>>) =
        this.AfterUpdateTaskRes(fun _ eOld eNew -> f.Invoke(eOld, eNew))

    member this.AfterUpdateTaskRes(f: Func<'ctx, 'entity, 'entity, Task<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> TaskResult.map (fun () -> eNew))

    member this.AfterUpdateTaskRes(f: Func<'entity, 'entity, Task<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(fun _ eOld eNew -> f.Invoke(eOld, eNew) |> TaskResult.map (fun () -> eNew))

    member this.AfterUpdateTaskRes(f: Func<'ctx, 'entity, Task<Result<'entity, Error list>>>) =
        this.AfterUpdateTaskRes(fun ctx _ eNew -> f.Invoke(ctx, eNew))

    member this.AfterUpdateTaskRes(f: Func<'ctx, 'entity, Task<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> TaskResult.map (fun () -> e))

    member this.AfterUpdateTaskRes(f: Func<'entity, Task<Result<'entity, Error list>>>) =
        this.AfterUpdateTaskRes(fun _ _ e -> f.Invoke e)

    member this.AfterUpdateTaskRes(f: Func<'entity, Task<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(fun _ _ e -> f.Invoke e |> TaskResult.map (fun () -> e))

    member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, 'entity, Async<Result<'entity, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc3 f)

    member this.AfterUpdateAsyncRes(f: Func<'entity, 'entity, Async<Result<'entity, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, 'entity, Async<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc3 f)

    member this.AfterUpdateAsyncRes(f: Func<'entity, 'entity, Async<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc f)

    member this.AfterUpdateAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
        this.AfterUpdateTaskRes(Task.liftAsyncFunc f)

    member this.AfterUpdateTask(f: Func<'ctx, 'entity, 'entity, Task<'entity>>) =
        this.AfterUpdateTaskRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Task.map Ok)

    member this.AfterUpdateTask(f: Func<'entity, 'entity, Task<'entity>>) =
        this.AfterUpdateTaskRes(fun eOld eNew -> f.Invoke(eOld, eNew) |> Task.map Ok)

    member this.AfterUpdateTask(f: Func<'ctx, 'entity, 'entity, Task<unit>>) =
        this.AfterUpdateTaskRes(fun ctx eOld eNew -> f.Invoke(ctx, eOld, eNew) |> Task.map Ok)

    member this.AfterUpdateTask(f: Func<'entity, 'entity, Task<unit>>) =
        this.AfterUpdateTaskRes(fun _ eOld eNew -> f.Invoke(eOld, eNew) |> Task.map Ok)

    member this.AfterUpdateTask(f: Func<'ctx, 'entity, Task<'entity>>) =
        this.AfterUpdateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.AfterUpdateTask(f: Func<'ctx, 'entity, Task<unit>>) =
        this.AfterUpdateTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.AfterUpdateTask(f: Func<'entity, Task<'entity>>) =
        this.AfterUpdateTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.AfterUpdateTask(f: Func<'entity, Task<unit>>) =
        this.AfterUpdateTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.AfterUpdateAsync(f: Func<'ctx, 'entity, 'entity, Async<'entity>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc3 f)

    member this.AfterUpdateAsync(f: Func<'entity, 'entity, Async<'entity>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsync(f: Func<'ctx, 'entity, 'entity, Async<unit>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc3 f)

    member this.AfterUpdateAsync(f: Func<'entity, 'entity, Async<unit>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsync(f: Func<'ctx, 'entity, Async<unit>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc2 f)

    member this.AfterUpdateAsync(f: Func<'entity, Async<'entity>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc f)

    member this.AfterUpdateAsync(f: Func<'entity, Async<unit>>) =
        this.AfterUpdateTask(Task.liftAsyncFunc f)

    member this.AfterUpdateRes(f: Func<'ctx, 'entity, 'entity, Result<'entity, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc3 f)

    member this.AfterUpdateRes(f: Func<'entity, 'entity, Result<'entity, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc2 f)

    member this.AfterUpdateRes(f: Func<'ctx, 'entity, 'entity, Result<unit, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc3 f)

    member this.AfterUpdateRes(f: Func<'entity, 'entity, Result<unit, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc2 f)

    member this.AfterUpdateRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc2 f)

    member this.AfterUpdateRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc2 f)

    member this.AfterUpdateRes(f: Func<'entity, Result<'entity, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc f)

    member this.AfterUpdateRes(f: Func<'entity, Result<unit, Error list>>) =
        this.AfterUpdateTaskRes(Task.liftFunc f)

    member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity, 'entity>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc3 f)

    member this.AfterUpdate(f: Func<'entity, 'entity, 'entity>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc2 f)

    member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity, unit>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc3 f)

    member this.AfterUpdate(f: Func<'entity, 'entity, unit>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc2 f)

    member this.AfterUpdate(f: Func<'ctx, 'entity, 'entity>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc2 f)

    member this.AfterUpdate(f: Func<'ctx, 'entity, unit>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc2 f)

    member this.AfterUpdate(f: Func<'entity, 'entity>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc f)

    member this.AfterUpdate(f: Func<'entity, unit>) =
        this.AfterUpdateTaskRes(TaskResult.liftFunc f)

    member this.Return202Accepted() = { this with return202Accepted = true }

    member this.ModifyResponse(getHandler: 'ctx -> 'entity -> HttpHandler) = {
        this with
            modifyResponse = getHandler
    }

    member this.ModifyResponse(f: 'ctx -> 'entity -> HttpContext -> unit) =
        this.ModifyResponse(fun ctx e ->
            (fun next httpCtx ->
                f ctx e httpCtx
                next httpCtx))

    member this.ModifyResponse(handler: HttpHandler) = this.ModifyResponse(fun _ _ -> handler)



type internal DeleteOperation<'ctx> =
    abstract Run:
        ResourceDefinition<'ctx> ->
        'ctx ->
        Request ->
        Preconditions<'ctx> ->
        BoxedEntity ->
        ResponseBuilder<'ctx> ->
            HttpHandler



type DeleteOperation<'originalCtx, 'ctx, 'entity> = internal {
    mapCtx: 'originalCtx -> 'entity -> Task<Result<'ctx, Error list>>
    beforeDelete: 'ctx -> 'entity -> Task<Result<'entity, Error list>>
    delete: 'originalCtx -> 'ctx -> Request -> 'entity -> Task<Result<unit, Error list>>
    modifyResponse: 'ctx -> HttpHandler
    return202Accepted: bool
} with

    static member internal Create(mapCtx, delete) : DeleteOperation<'originalCtx, 'ctx, 'entity> = {
        mapCtx = mapCtx
        beforeDelete = fun _ x -> Ok x |> Task.result
        delete = delete
        modifyResponse = fun _ -> fun next ctx -> next ctx
        return202Accepted = false
    }


    interface DeleteOperation<'originalCtx> with
        member this.Run _resDef ctx req preconditions entity0 resp =
            fun next httpCtx ->
                task {
                    match! this.mapCtx ctx (unbox<'entity> entity0) with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok mappedCtx ->
                        match StrictModeHelpers.checkForUnknownQueryParameters<'originalCtx> httpCtx req Set.empty with
                        | Error errs -> return! handleErrors errs next httpCtx
                        | Ok() ->
                            match preconditions.Validate httpCtx ctx entity0 with
                            | Error errors -> return! handleErrors errors next httpCtx
                            | Ok() ->
                                match! this.beforeDelete mappedCtx (unbox<'entity> entity0) with
                                | Error errors -> return! handleErrors errors next httpCtx
                                | Ok entity1 ->
                                    match! this.delete ctx mappedCtx req (unbox<'entity> entity1) with
                                    | Error errors -> return! handleErrors errors next httpCtx
                                    | Ok() ->
                                        if this.return202Accepted then
                                            let handler = setStatusCode 202 >=> this.modifyResponse mappedCtx
                                            return! handler next httpCtx
                                        else
                                            match! resp.WriteNoResource httpCtx ctx req with
                                            | None ->
                                                let handler = setStatusCode 204 >=> this.modifyResponse mappedCtx
                                                return! handler next httpCtx
                                            | Some doc ->
                                                let handler =
                                                    setStatusCode 200
                                                    >=> this.modifyResponse mappedCtx
                                                    >=> jsonApiWithETag<'ctx> doc

                                                return! handler next httpCtx
                }


    member this.BeforeDeleteTaskRes(f: Func<'ctx, 'entity, Task<Result<'entity, Error list>>>) = {
        this with
            beforeDelete = (fun ctx e -> f.Invoke(ctx, e))
    }

    member this.BeforeDeleteTaskRes(f: Func<'ctx, 'entity, Task<Result<unit, Error list>>>) =
        this.BeforeDeleteTaskRes(fun ctx e -> f.Invoke(ctx, e) |> TaskResult.map (fun () -> e))

    member this.BeforeDeleteTaskRes(f: Func<'entity, Task<Result<'entity, Error list>>>) =
        this.BeforeDeleteTaskRes(fun _ e -> f.Invoke e)

    member this.BeforeDeleteTaskRes(f: Func<'entity, Task<Result<unit, Error list>>>) =
        this.BeforeDeleteTaskRes(fun _ e -> f.Invoke e)

    member this.BeforeDeleteAsyncRes(f: Func<'ctx, 'entity, Async<Result<'entity, Error list>>>) =
        this.BeforeDeleteTaskRes(Task.liftAsyncFunc2 f)

    member this.BeforeDeleteAsyncRes(f: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
        this.BeforeDeleteTaskRes(Task.liftAsyncFunc2 f)

    member this.BeforeDeleteAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
        this.BeforeDeleteTaskRes(Task.liftAsyncFunc f)

    member this.BeforeDeleteAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
        this.BeforeDeleteTaskRes(Task.liftAsyncFunc f)

    member this.BeforeDeleteTask(f: Func<'ctx, 'entity, Task<'entity>>) =
        this.BeforeDeleteTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.BeforeDeleteTask(f: Func<'ctx, 'entity, Task<unit>>) =
        this.BeforeDeleteTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

    member this.BeforeDeleteTask(f: Func<'entity, Task<'entity>>) =
        this.BeforeDeleteTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.BeforeDeleteTask(f: Func<'entity, Task<unit>>) =
        this.BeforeDeleteTaskRes(fun e -> f.Invoke e |> Task.map Ok)

    member this.BeforeDeleteAsync(f: Func<'ctx, 'entity, Async<'entity>>) =
        this.BeforeDeleteTask(Task.liftAsyncFunc2 f)

    member this.BeforeDeleteAsync(f: Func<'ctx, 'entity, Async<unit>>) =
        this.BeforeDeleteTask(Task.liftAsyncFunc2 f)

    member this.BeforeDeleteAsync(f: Func<'entity, Async<'entity>>) =
        this.BeforeDeleteTask(Task.liftAsyncFunc f)

    member this.BeforeDeleteAsync(f: Func<'entity, Async<unit>>) =
        this.BeforeDeleteTask(Task.liftAsyncFunc f)

    member this.BeforeDeleteRes(f: Func<'ctx, 'entity, Result<'entity, Error list>>) =
        this.BeforeDeleteTaskRes(Task.liftFunc2 f)

    member this.BeforeDeleteRes(f: Func<'ctx, 'entity, Result<unit, Error list>>) =
        this.BeforeDeleteTaskRes(Task.liftFunc2 f)

    member this.BeforeDeleteRes(f: Func<'entity, Result<'entity, Error list>>) =
        this.BeforeDeleteTaskRes(Task.liftFunc f)

    member this.BeforeDeleteRes(f: Func<'entity, Result<unit, Error list>>) =
        this.BeforeDeleteTaskRes(Task.liftFunc f)

    member this.BeforeDelete(f: Func<'ctx, 'entity, 'entity>) =
        this.BeforeDeleteTaskRes(TaskResult.liftFunc2 f)

    member this.BeforeDelete(f: Func<'ctx, 'entity, unit>) =
        this.BeforeDeleteTaskRes(TaskResult.liftFunc2 f)

    member this.BeforeDelete(f: Func<'entity, 'entity>) =
        this.BeforeDeleteTaskRes(TaskResult.liftFunc f)

    member this.BeforeDelete(f: Func<'entity, unit>) =
        this.BeforeDeleteTaskRes(TaskResult.liftFunc f)

    member this.ModifyResponse(getHandler: 'ctx -> HttpHandler) = {
        this with
            modifyResponse = getHandler
    }

    member this.ModifyResponse(f: 'ctx -> HttpContext -> unit) =
        this.ModifyResponse(fun ctx ->
            (fun next httpCtx ->
                f ctx httpCtx
                next httpCtx))

    member this.ModifyResponse(handler: HttpHandler) = this.ModifyResponse(fun _ -> handler)

    member this.Return202Accepted() = { this with return202Accepted = true }



type internal RequestValidationConfig = {
    ValidateAccept: bool
    ValidateContentType: bool
    ValidateQueryParams: bool
}



type internal CustomOperation<'ctx> =
    abstract Name: LinkName
    abstract HasModifyingOperations: bool
    abstract HrefAndMeta: 'ctx -> uri: string -> BoxedEntity -> Task<(string option * Map<string, obj> option) option>

    abstract Get:
        ((RequestValidationConfig -> HttpHandler) -> 'ctx -> Request -> Responder<'ctx> -> BoxedEntity -> HttpHandler) option

    abstract Post:
        ((RequestValidationConfig -> HttpHandler)
            -> 'ctx
            -> Request
            -> Responder<'ctx>
            -> Preconditions<'ctx>
            -> BoxedEntity
            -> HttpHandler) option

    abstract Patch:
        ((RequestValidationConfig -> HttpHandler)
            -> 'ctx
            -> Request
            -> Responder<'ctx>
            -> Preconditions<'ctx>
            -> BoxedEntity
            -> HttpHandler) option

    abstract Delete:
        ((RequestValidationConfig -> HttpHandler)
            -> 'ctx
            -> Request
            -> Responder<'ctx>
            -> Preconditions<'ctx>
            -> BoxedEntity
            -> HttpHandler) option



type CustomOperation<'originalCtx, 'ctx, 'entity> = internal {
    mapCtx: 'originalCtx -> 'entity -> Task<Result<'ctx, Error list>>
    name: string
    getMeta: ('ctx -> 'entity -> Map<string, obj>) option
    condition: 'ctx -> 'entity -> Task<Result<unit, Error list>>
    validationConfig: RequestValidationConfig
    validateStrictModeAllowedQueryParams: Set<QueryParamName> option
    skipLink: bool
    get:
        ('originalCtx -> 'ctx -> Request -> Responder<'originalCtx> -> 'entity -> Task<Result<HttpHandler, Error list>>) option
    post:
        ('originalCtx -> 'ctx -> Request -> Responder<'originalCtx> -> 'entity -> Task<Result<HttpHandler, Error list>>) option
    patch:
        ('originalCtx -> 'ctx -> Request -> Responder<'originalCtx> -> 'entity -> Task<Result<HttpHandler, Error list>>) option
    delete:
        ('originalCtx -> 'ctx -> Request -> Responder<'originalCtx> -> 'entity -> Task<Result<HttpHandler, Error list>>) option
} with

    static member internal Create(mapCtx, name) : CustomOperation<'originalCtx, 'ctx, 'entity> = {
        mapCtx = mapCtx
        name = name
        getMeta = None
        condition = fun _ _ -> Ok() |> Task.result
        validationConfig = {
            ValidateAccept = true
            ValidateContentType = true
            ValidateQueryParams = true
        }
        validateStrictModeAllowedQueryParams = None
        skipLink = false
        get = None
        post = None
        patch = None
        delete = None
    }


    member private this.handler
        validateRequest
        (operation: _ -> _ -> _ -> _ -> _ -> Task<Result<HttpHandler, _>>)
        ctx
        req
        responder
        (preconditions: Preconditions<'originalCtx>)
        (entity: obj)
        =
        validateRequest this.validationConfig
        >=> fun next httpCtx ->
            task {
                match! this.mapCtx ctx (unbox<'entity> entity) with
                | Error errors -> return! handleErrors errors next httpCtx
                | Ok mappedCtx ->
                    match! this.condition mappedCtx (unbox<'entity> entity) with
                    | Error errors -> return! handleErrors errors next httpCtx
                    | Ok() ->
                        match preconditions.Validate httpCtx ctx entity with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok() ->
                            let validateQueryParamsResult =
                                match this.validateStrictModeAllowedQueryParams with
                                | None -> Ok()
                                | Some validQueryParams ->
                                    StrictModeHelpers.checkForUnknownQueryParameters<'originalCtx>
                                        httpCtx
                                        req
                                        validQueryParams

                            match validateQueryParamsResult with
                            | Error errs -> return! handleErrors errs next httpCtx
                            | Ok() ->
                                match! operation ctx mappedCtx req responder (unbox<'entity> entity) with
                                | Ok handler -> return! handler next httpCtx
                                | Error errors -> return! handleErrors errors next httpCtx
            }


    interface CustomOperation<'originalCtx> with
        member this.Name = this.name

        member this.HasModifyingOperations =
            this.post.IsSome || this.patch.IsSome || this.delete.IsSome

        member this.HrefAndMeta ctx selfUrl entity =
            task {
                if this.skipLink then
                    return None
                elif this.get.IsNone && this.post.IsNone && this.patch.IsNone && this.delete.IsNone then
                    return Some(None, None)
                else
                    match! this.mapCtx ctx (unbox<'entity> entity) with
                    | Error _ -> return Some(None, None)
                    | Ok mappedCtx ->
                        let meta =
                            this.getMeta
                            |> Option.map (fun getMeta -> getMeta mappedCtx (unbox<'entity> entity))
                            |> Option.filter (not << Map.isEmpty)

                        match! this.condition mappedCtx (unbox<'entity> entity) with
                        | Ok() -> return Some(Some(selfUrl + "/" + this.name), meta)
                        | Error _ -> return Some(None, meta)
            }


        member this.Get =
            let prec =
                { new Preconditions<'originalCtx> with
                    member _.Validate _ _ _ = Ok()
                }

            this.get
            |> Option.map (fun get ->
                fun getValidationHandler ctx req resp e -> this.handler getValidationHandler get ctx req resp prec e)

        member this.Post =
            this.post
            |> Option.map (fun post ->
                fun getValidationHandler ctx req resp prec e ->
                    this.handler getValidationHandler post ctx req resp prec e)

        member this.Patch =
            this.patch
            |> Option.map (fun patch ->
                fun getValidationHandler ctx req resp prec e ->
                    this.handler getValidationHandler patch ctx req resp prec e)

        member this.Delete =
            this.delete
            |> Option.map (fun delete ->
                fun getValidationHandler ctx req resp prec e ->
                    this.handler getValidationHandler delete ctx req resp prec e)


    /// Skips the requirement and validation of the JSON:API media type in the Accept request header for this custom operation
    /// (for all HTTP verbs if multiple are defined).
    member this.SkipStandardAcceptValidation() = {
        this with
            validationConfig = {
                this.validationConfig with
                    ValidateAccept = false
            }
    }

    /// Skips the requirement and validation of the JSON:API media type in the Content-Type request header for this custom
    /// operation (for all HTTP verbs if multiple are defined).
    member this.SkipStandardContentTypeValidation() = {
        this with
            validationConfig = {
                this.validationConfig with
                    ValidateContentType = false
            }
    }

    /// Skips the validation of query parameter names that requires them to conform to the requirements in the JSON:API
    /// specification.
    ///
    /// This also skips the validation of any "skip links" query parameters set up using SkipStandardLinksQueryParamName
    /// and SkipCustomLinksQueryParamName when configuring JSON:API.
    member this.SkipStandardQueryParamNameValidation() = {
        this with
            validationConfig = {
                this.validationConfig with
                    ValidateQueryParams = false
            }
    }

    member this.ConditionTaskRes(predicate: Func<'ctx, 'entity, Task<Result<unit, Error list>>>) = {
        this with
            condition = fun ctx e -> predicate.Invoke(ctx, e)
    }

    member this.ConditionTaskRes(predicate: Func<'entity, Task<Result<unit, Error list>>>) =
        this.ConditionTaskRes(fun _ e -> predicate.Invoke e)

    member this.ConditionAsyncRes(predicate: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
        this.ConditionTaskRes(Task.liftAsyncFunc2 predicate)

    member this.ConditionAsyncRes(predicate: Func<'entity, Async<Result<unit, Error list>>>) =
        this.ConditionTaskRes(Task.liftAsyncFunc predicate)

    member this.ConditionRes(predicate: Func<'ctx, 'entity, Result<unit, Error list>>) =
        this.ConditionTaskRes(Task.liftFunc2 predicate)

    member this.ConditionRes(predicate: Func<'entity, Result<unit, Error list>>) =
        this.ConditionTaskRes(Task.liftFunc predicate)

    member this.Condition(predicate: Func<'ctx, 'entity, bool>) =
        this.ConditionTaskRes(fun ctx e ->
            (if predicate.Invoke(ctx, e) then
                 Ok()
             else
                 Error [ customOpConditionFalse () ])
            |> Task.result)

    member this.Condition(predicate: Func<'entity, bool>) =
        this.ConditionTaskRes(fun _ e ->
            (if predicate.Invoke e then
                 Ok()
             else
                 Error [ customOpConditionFalse () ])
            |> Task.result)

    member this.AddMeta(key: string, getValue: 'ctx -> 'entity -> 'a, ?condition: 'ctx -> 'entity -> bool) =
        let condition = defaultArg condition (fun _ _ -> true)
        let getMeta = this.getMeta |> Option.defaultValue (fun _ _ -> Map.empty)

        {
            this with
                getMeta =
                    Some(fun ctx e ->
                        if condition ctx e then
                            getMeta ctx e |> Map.add key (getValue ctx e |> box)
                        else
                            getMeta ctx e)
        }

    member this.AddMeta(key: string, getValue: 'entity -> 'a, ?condition: 'entity -> bool) =
        let condition = defaultArg condition (fun _ -> true)
        this.AddMeta(key, (fun _ e -> getValue e), (fun _ e -> condition e))

    member this.AddMetaOpt(key: string, getValue: 'ctx -> 'entity -> 'a option) =
        let getMeta = this.getMeta |> Option.defaultValue (fun _ _ -> Map.empty)

        this.AddMeta(
            key,
            fun ctx e ->
                match getValue ctx e with
                | None -> getMeta ctx e
                | Some m -> getMeta ctx e |> Map.add key (box m)
        )

    member this.AddMetaOpt(key: string, getValue: 'entity -> 'a option) =
        this.AddMetaOpt(key, (fun _ e -> getValue e))

    member this.GetTask
        (get:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Task<Result<HttpHandler, Error list>>>)
        =
        {
            this with
                get =
                    Some(fun origCtx ctx req responder entity ->
                        get.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req), responder, entity))
        }

    member this.PostTask
        (post:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Task<Result<HttpHandler, Error list>>>)
        =
        {
            this with
                post =
                    Some(fun origCtx ctx req responder entity ->
                        post.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req), responder, entity))
        }

    member this.PatchTask
        (patch:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Task<Result<HttpHandler, Error list>>>)
        =
        {
            this with
                patch =
                    Some(fun origCtx ctx req responder entity ->
                        patch.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req), responder, entity))
        }

    member this.DeleteTask
        (delete:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Task<Result<HttpHandler, Error list>>>)
        =
        {
            this with
                delete =
                    Some(fun origCtx ctx req responder entity ->
                        delete.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req), responder, entity))
        }

    member this.GetAsync
        (get:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>)
        =
        this.GetTask(Task.liftAsyncFunc4 get)

    member this.PostAsync
        (post:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>)
        =
        this.PostTask(Task.liftAsyncFunc4 post)

    member this.PatchAsync
        (patch:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>)
        =
        this.PatchTask(Task.liftAsyncFunc4 patch)

    member this.DeleteAsync
        (delete:
            Func<'ctx, RequestParserHelper<'originalCtx>, Responder<'originalCtx>, 'entity, Async<Result<HttpHandler, Error list>>>)
        =
        this.DeleteTask(Task.liftAsyncFunc4 delete)

    /// If called, will enable query parameter validation for this custom operation by treating the specified query
    /// parameters as the only allowed query parameters (in addition to standard JSON:API query parameters).
    member this.ValidateStrictModeQueryParams([<ParamArray>] allowedQueryParamNames: string[]) = {
        this with
            validateStrictModeAllowedQueryParams = Some(Set.ofArray allowedQueryParamNames)
    }

    /// If called, Felicity does not add the link to the resource's 'links' object. Using this is required in order to be
    /// JSON:API compliant, but is not enabled by default for backward compatibility reasons.
    ///
    /// For more information, see https://github.com/json-api/json-api/issues/1656
    member this.SkipLink() = { this with skipLink = true }



type PolymorphicOperationHelper<'originalCtx, 'ctx, 'entity, 'id>
    internal (mapCtx: 'originalCtx -> Task<Result<'ctx, Error list>>) =

    member _.LookupTaskRes
        (
            getById: Func<'ctx, 'id, Task<Result<'entity option, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        PolymorphicResourceLookup<'originalCtx, 'ctx, 'entity, 'id>
            .Create(mapCtx, (fun ctx id -> getById.Invoke(ctx, id)), getPolyBuilder)

    member this.LookupTaskRes
        (
            getById: Func<'id, Task<Result<'entity option, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes((fun _ id -> getById.Invoke id), getPolyBuilder)

    member this.LookupAsyncRes
        (
            getById: Func<'ctx, 'id, Async<Result<'entity option, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes(Task.liftAsyncFunc2 getById, getPolyBuilder)

    member this.LookupAsyncRes
        (
            getById: Func<'id, Async<Result<'entity option, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes(Task.liftAsyncFunc getById, getPolyBuilder)

    member this.LookupTask
        (
            getById: Func<'ctx, 'id, Task<'entity option>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes((fun ctx id -> getById.Invoke(ctx, id) |> Task.map Ok), getPolyBuilder)

    member this.LookupTask
        (
            getById: Func<'id, Task<'entity option>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes((fun _ id -> getById.Invoke id |> Task.map Ok), getPolyBuilder)

    member this.LookupAsync
        (
            getById: Func<'ctx, 'id, Async<'entity option>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTask(Task.liftAsyncFunc2 getById, getPolyBuilder)

    member this.LookupAsync
        (
            getById: Func<'id, Async<'entity option>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTask(Task.liftAsyncFunc getById, getPolyBuilder)

    member this.LookupRes
        (
            getById: Func<'ctx, 'id, Result<'entity option, Error list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes(Task.liftFunc2 getById, getPolyBuilder)

    member this.LookupRes
        (
            getById: Func<'id, Result<'entity option, Error list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes(Task.liftFunc getById, getPolyBuilder)

    member this.Lookup
        (
            getById: Func<'ctx, 'id, 'entity option>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes(TaskResult.liftFunc2 getById, getPolyBuilder)

    member this.Lookup
        (
            getById: Func<'id, 'entity option>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.LookupTaskRes(TaskResult.liftFunc getById, getPolyBuilder)

    member _.GetCollectionTaskRes
        (
            getCollection: Func<'ctx, Task<Result<'entity list, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>
            .Create(
                mapCtx,
                (fun _origCtx ctx _req ->
                    getCollection.Invoke ctx |> TaskResult.map (fun xs -> Set.empty, Set.empty, xs)),
                getPolyBuilder
            )

    member this.GetCollectionTaskRes
        (
            getCollection: Func<unit, Task<Result<'entity list, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes((fun (_: 'ctx) -> getCollection.Invoke()), getPolyBuilder)

    member _.GetCollectionTaskRes
        (
            getRequestParser:
                Func<'ctx, RequestParserHelper<'originalCtx>, Task<Result<RequestParser<'originalCtx, 'entity list>, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        PolymorphicGetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>
            .Create(
                mapCtx,
                (fun origCtx ctx req ->
                    getRequestParser.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req))
                    |> TaskResult.bind (fun p -> p.ParseWithConsumed())),
                getPolyBuilder
            )

    member this.GetCollectionAsyncRes
        (
            getCollection: Func<'ctx, Async<Result<'entity list, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(Task.liftAsyncFunc getCollection, getPolyBuilder)

    member this.GetCollectionAsyncRes
        (
            getCollection: Func<unit, Async<Result<'entity list, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(Task.liftAsyncFunc getCollection, getPolyBuilder)

    member this.GetCollectionAsyncRes
        (
            getRequestParser:
                Func<'ctx, RequestParserHelper<'originalCtx>, Async<Result<RequestParser<'originalCtx, 'entity list>, Error list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(Task.liftAsyncFunc2 getRequestParser, getPolyBuilder)

    member this.GetCollectionTask
        (
            getCollection: Func<'ctx, Task<'entity list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(getCollection.Invoke >> Task.map Ok, getPolyBuilder)

    member this.GetCollectionTask
        (
            getCollection: Func<unit, Task<'entity list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(getCollection.Invoke >> Task.map Ok, getPolyBuilder)

    member this.GetCollectionTask
        (
            getRequestParser:
                Func<'ctx, RequestParserHelper<'originalCtx>, Task<RequestParser<'originalCtx, 'entity list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes((fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Task.map Ok), getPolyBuilder)

    member this.GetCollectionAsync
        (
            getCollection: Func<'ctx, Async<'entity list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTask(Task.liftAsyncFunc getCollection, getPolyBuilder)

    member this.GetCollectionAsync
        (
            getCollection: Func<unit, Async<'entity list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTask(Task.liftAsyncFunc getCollection, getPolyBuilder)

    member this.GetCollectionAsync
        (
            getRequestParser:
                Func<'ctx, RequestParserHelper<'originalCtx>, Async<RequestParser<'originalCtx, 'entity list>>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTask(Task.liftAsyncFunc2 getRequestParser, getPolyBuilder)

    member this.GetCollectionRes
        (
            getCollection: Func<'ctx, Result<'entity list, Error list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(Task.liftFunc getCollection, getPolyBuilder)

    member this.GetCollectionRes
        (
            getCollection: Func<unit, Result<'entity list, Error list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(Task.liftFunc getCollection, getPolyBuilder)

    member this.GetCollectionRes
        (
            getRequestParser:
                Func<'ctx, RequestParserHelper<'originalCtx>, Result<RequestParser<'originalCtx, 'entity list>, Error list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(Task.liftFunc2 getRequestParser, getPolyBuilder)

    member this.GetCollection
        (
            getCollection: Func<'ctx, 'entity list>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(TaskResult.liftFunc getCollection, getPolyBuilder)

    member this.GetCollection
        (
            getCollection: Func<unit, 'entity list>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(TaskResult.liftFunc getCollection, getPolyBuilder)

    member this.GetCollection
        (
            getRequestParser: Func<'ctx, RequestParserHelper<'originalCtx>, RequestParser<'originalCtx, 'entity list>>,
            getPolyBuilder: 'entity -> PolymorphicBuilder<'originalCtx>
        ) =
        this.GetCollectionTaskRes(TaskResult.liftFunc2 getRequestParser, getPolyBuilder)



type OperationHelperWithEntityMapCtx<'originalCtx, 'ctx, 'entity, 'id>
    internal (mapCtx: 'originalCtx -> 'entity -> Task<Result<'ctx, Error list>>) =

    member _.GetResource() =
        GetResourceOperation<'originalCtx, 'ctx, 'entity, 'id>.Create(mapCtx)

    member _.Patch() =
        PatchOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx)

    member _.DeleteTaskRes(delete: Func<'ctx, 'entity, Task<Result<unit, Error list>>>) =
        DeleteOperation<'originalCtx, 'ctx, 'entity>
            .Create(mapCtx, (fun _origCtx ctx _ entity -> delete.Invoke(ctx, entity)))

    member this.DeleteTaskRes(delete: Func<'entity, Task<Result<unit, Error list>>>) =
        this.DeleteTaskRes(fun _ e -> delete.Invoke e)

    member _.DeleteTaskRes
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Task<Result<RequestParser<'originalCtx, unit>, Error list>>>)
        =
        DeleteOperation<'originalCtx, 'ctx, 'entity>
            .Create(
                mapCtx,
                fun origCtx ctx req entity ->
                    getRequestParser.Invoke(ctx, entity, RequestParserHelper<'originalCtx>(origCtx, req))
                    |> TaskResult.bind (fun p -> p.ParseTask())
            )

    member this.DeleteTaskRes
        (getRequestParser:
            Func<'entity, RequestParserHelper<'originalCtx>, Task<Result<RequestParser<'originalCtx, unit>, Error list>>>)
        =
        this.DeleteTaskRes(fun _ e p -> getRequestParser.Invoke(e, p))

    member this.DeleteAsyncRes(delete: Func<'ctx, 'entity, Async<Result<unit, Error list>>>) =
        this.DeleteTaskRes(Task.liftAsyncFunc2 delete)

    member this.DeleteAsyncRes(delete: Func<'entity, Async<Result<unit, Error list>>>) =
        this.DeleteTaskRes(Task.liftAsyncFunc delete)

    member this.DeleteAsyncRes
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Async<Result<RequestParser<'originalCtx, unit>, Error list>>>)
        =
        this.DeleteTaskRes(Task.liftAsyncFunc3 getRequestParser)

    member this.DeleteAsyncRes
        (getRequestParser:
            Func<'entity, RequestParserHelper<'originalCtx>, Async<Result<RequestParser<'originalCtx, unit>, Error list>>>)
        =
        this.DeleteTaskRes(Task.liftAsyncFunc2 getRequestParser)

    member this.DeleteTask(delete: Func<'ctx, 'entity, Task<unit>>) =
        this.DeleteTaskRes(fun ctx e -> delete.Invoke(ctx, e) |> Task.map Ok)

    member this.DeleteTask(delete: Func<'entity, Task<unit>>) =
        this.DeleteTaskRes(fun _ e -> delete.Invoke e |> Task.map Ok)

    member this.DeleteTask
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Task<RequestParser<'originalCtx, unit>>>)
        =
        this.DeleteTaskRes(fun ctx e parse -> getRequestParser.Invoke(ctx, e, parse) |> Task.map Ok)

    member this.DeleteTask
        (getRequestParser: Func<'entity, RequestParserHelper<'originalCtx>, Task<RequestParser<'originalCtx, unit>>>)
        =
        this.DeleteTaskRes(fun ctx e parse -> getRequestParser.Invoke(e, parse) |> Task.map Ok)

    member this.DeleteAsync(delete: Func<'ctx, 'entity, Async<unit>>) =
        this.DeleteTask(Task.liftAsyncFunc2 delete)

    member this.DeleteAsync(delete: Func<'entity, Async<unit>>) =
        this.DeleteTask(Task.liftAsyncFunc delete)

    member this.DeleteAsync
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Async<RequestParser<'originalCtx, unit>>>)
        =
        this.DeleteTask(Task.liftAsyncFunc3 getRequestParser)

    member this.DeleteAsync
        (getRequestParser: Func<'entity, RequestParserHelper<'originalCtx>, Async<RequestParser<'originalCtx, unit>>>)
        =
        this.DeleteTask(Task.liftAsyncFunc2 getRequestParser)

    member this.DeleteRes(delete: Func<'ctx, 'entity, Result<unit, Error list>>) =
        this.DeleteTaskRes(Task.liftFunc2 delete)

    member this.DeleteRes(delete: Func<'entity, Result<unit, Error list>>) =
        this.DeleteTaskRes(Task.liftFunc delete)

    member this.DeleteRes
        (getRequestParser:
            Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, Result<RequestParser<'originalCtx, unit>, Error list>>)
        =
        this.DeleteTaskRes(Task.liftFunc3 getRequestParser)

    member this.DeleteRes
        (getRequestParser:
            Func<'entity, RequestParserHelper<'originalCtx>, Result<RequestParser<'originalCtx, unit>, Error list>>)
        =
        this.DeleteTaskRes(Task.liftFunc2 getRequestParser)

    member this.Delete(delete: Func<'ctx, 'entity, unit>) =
        this.DeleteTaskRes(TaskResult.liftFunc2 delete)

    member this.Delete(delete: Func<'entity, unit>) =
        this.DeleteTaskRes(TaskResult.liftFunc delete)

    member this.Delete
        (getRequestParser: Func<'ctx, 'entity, RequestParserHelper<'originalCtx>, RequestParser<'originalCtx, unit>>)
        =
        this.DeleteTaskRes(TaskResult.liftFunc3 getRequestParser)

    member this.Delete
        (getRequestParser: Func<'entity, RequestParserHelper<'originalCtx>, RequestParser<'originalCtx, unit>>)
        =
        this.DeleteTaskRes(TaskResult.liftFunc2 getRequestParser)

    member _.CustomLink([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
        CustomOperation<'originalCtx, 'ctx, 'entity>.Create(mapCtx, name)

    member _.Set2TaskRes
        (
            set: 'ctx -> 'field1 * 'field2 -> 'entity -> Task<Result<'entity, Error list>>,
            field1: OptionalRequestGetter<'originalCtx, 'field1>,
            field2: OptionalRequestGetter<'originalCtx, 'field2>,
            ?setOrder
        ) =
        match field1.FieldName, field2.FieldName with
        | Some fn1, Some fn2 ->
            { new FieldSetter<'originalCtx> with
                member _.Names = Set.ofList [ fn1; fn2 ]
                member _.SetOrder = defaultArg setOrder 0

                member _.Set ctx req entity _ =
                    task {
                        let! f1 = field1.Get(ctx, req, None)
                        let! f2 = field2.Get(ctx, req, None)

                        match f1, f2 with
                        | Error errs1, Error errs2 -> return Error(errs1 @ errs2)
                        | Error errs, Ok _
                        | Ok _, Error errs -> return Error errs
                        | Ok None, Ok None -> return (Ok(entity, false))
                        | Ok(Some _), Ok None -> return Error [ set2OneFieldMissing fn1 fn2 ]
                        | Ok None, Ok(Some _) -> return Error [ set2OneFieldMissing fn2 fn1 ]
                        | Ok(Some val1), Ok(Some val2) ->
                            match! mapCtx ctx (unbox<'entity> entity) with
                            | Error errs -> return Error errs
                            | Ok ctx ->
                                return!
                                    set ctx (val1, val2) (unbox<'entity> entity)
                                    |> TaskResult.map (fun e -> box e, true)
                    }
            }
        | _ -> failwith "Can only be called with fields, not query parameters or headers"

    member this.Set2AsyncRes
        (
            set: 'ctx -> 'field1 * 'field2 -> 'entity -> Async<Result<'entity, Error list>>,
            field1: OptionalRequestGetter<'originalCtx, 'field1>,
            field2: OptionalRequestGetter<'originalCtx, 'field2>,
            ?setOrder
        ) =
        this.Set2TaskRes(Task.liftAsync3 set, field1, field2, ?setOrder = setOrder)

    member this.Set2Task
        (
            set: 'ctx -> 'field1 * 'field2 -> 'entity -> Task<'entity>,
            field1: OptionalRequestGetter<'originalCtx, 'field1>,
            field2: OptionalRequestGetter<'originalCtx, 'field2>,
            ?setOrder
        ) =
        this.Set2TaskRes((fun ctx v e -> set ctx v e |> Task.map Ok), field1, field2, ?setOrder = setOrder)

    member this.Set2Async
        (
            set: 'ctx -> 'field1 * 'field2 -> 'entity -> Async<'entity>,
            field1: OptionalRequestGetter<'originalCtx, 'field1>,
            field2: OptionalRequestGetter<'originalCtx, 'field2>,
            ?setOrder
        ) =
        this.Set2Task(Task.liftAsync3 set, field1, field2, ?setOrder = setOrder)

    member this.Set2Res
        (
            set: 'ctx -> 'field1 * 'field2 -> 'entity -> Result<'entity, Error list>,
            field1: OptionalRequestGetter<'originalCtx, 'field1>,
            field2: OptionalRequestGetter<'originalCtx, 'field2>,
            ?setOrder
        ) =
        this.Set2TaskRes((fun ctx v e -> set ctx v e |> Task.result), field1, field2, ?setOrder = setOrder)

    member this.Set2
        (
            set: 'ctx -> 'field1 * 'field2 -> 'entity -> 'entity,
            field1: OptionalRequestGetter<'originalCtx, 'field1>,
            field2: OptionalRequestGetter<'originalCtx, 'field2>,
            ?setOrder
        ) =
        this.Set2TaskRes((fun ctx v e -> set ctx v e |> Ok |> Task.result), field1, field2, ?setOrder = setOrder)

    member _.Set2SameNullTaskRes
        (
            set: 'ctx -> ('field1 * 'field2) option -> 'entity -> Task<Result<'entity, Error list>>,
            field1: OptionalRequestGetter<'originalCtx, 'field1 option>,
            field2: OptionalRequestGetter<'originalCtx, 'field2 option>,
            ?setOrder
        ) =
        match field1.FieldName, field2.FieldName with
        | Some fn1, Some fn2 ->
            { new FieldSetter<'originalCtx> with
                member _.Names = Set.ofList [ fn1; fn2 ]
                member _.SetOrder = defaultArg setOrder 0

                member _.Set ctx req entity _ =
                    task {
                        let! f1 = field1.Get(ctx, req, None)
                        let! f2 = field2.Get(ctx, req, None)

                        match f1, f2 with
                        | Error errs1, Error errs2 -> return Error(errs1 @ errs2)
                        | Error errs, Ok _
                        | Ok _, Error errs -> return Error errs
                        | Ok None, Ok None -> return (Ok(entity, false))
                        | Ok(Some _), Ok None -> return Error [ set2OneFieldMissing fn1 fn2 ]
                        | Ok None, Ok(Some _) -> return Error [ set2OneFieldMissing fn2 fn1 ]
                        | Ok(Some None), Ok(Some(Some _))
                        | Ok(Some(Some _)), Ok(Some None) -> return Error [ set2DifferentNull fn1 fn2 ]
                        | Ok(Some(Some val1)), Ok(Some(Some val2)) ->
                            match! mapCtx ctx (unbox<'entity> entity) with
                            | Error errs -> return Error errs
                            | Ok ctx ->
                                return!
                                    set ctx (Some(val1, val2)) (unbox<'entity> entity)
                                    |> TaskResult.map (fun e -> box e, true)
                        | Ok(Some None), Ok(Some None) ->
                            match! mapCtx ctx (unbox<'entity> entity) with
                            | Error errs -> return Error errs
                            | Ok ctx ->
                                return! set ctx None (unbox<'entity> entity) |> TaskResult.map (fun e -> box e, true)
                    }
            }
        | _ -> failwith "Can only be called with fields, not query parameters or headers"

    member this.Set2SameNullAsyncRes
        (
            set: 'ctx -> ('field1 * 'field2) option -> 'entity -> Async<Result<'entity, Error list>>,
            field1: OptionalRequestGetter<'originalCtx, 'field1 option>,
            field2: OptionalRequestGetter<'originalCtx, 'field2 option>,
            ?setOrder
        ) =
        this.Set2SameNullTaskRes(Task.liftAsync3 set, field1, field2, ?setOrder = setOrder)

    member this.Set2SameNullTask
        (
            set: 'ctx -> ('field1 * 'field2) option -> 'entity -> Task<'entity>,
            field1: OptionalRequestGetter<'originalCtx, 'field1 option>,
            field2: OptionalRequestGetter<'originalCtx, 'field2 option>,
            ?setOrder
        ) =
        this.Set2SameNullTaskRes((fun ctx v e -> set ctx v e |> Task.map Ok), field1, field2, ?setOrder = setOrder)

    member this.Set2SameNullAsync
        (
            set: 'ctx -> ('field1 * 'field2) option -> 'entity -> Async<'entity>,
            field1: OptionalRequestGetter<'originalCtx, 'field1 option>,
            field2: OptionalRequestGetter<'originalCtx, 'field2 option>,
            ?setOrder
        ) =
        this.Set2SameNullTask(Task.liftAsync3 set, field1, field2, ?setOrder = setOrder)

    member this.Set2SameNullRes
        (
            set: 'ctx -> ('field1 * 'field2) option -> 'entity -> Result<'entity, Error list>,
            field1: OptionalRequestGetter<'originalCtx, 'field1 option>,
            field2: OptionalRequestGetter<'originalCtx, 'field2 option>,
            ?setOrder
        ) =
        this.Set2SameNullTaskRes((fun ctx v e -> set ctx v e |> Task.result), field1, field2, ?setOrder = setOrder)

    member this.Set2SameNull
        (
            set: 'ctx -> ('field1 * 'field2) option -> 'entity -> 'entity,
            field1: OptionalRequestGetter<'originalCtx, 'field1 option>,
            field2: OptionalRequestGetter<'originalCtx, 'field2 option>,
            ?setOrder
        ) =
        this.Set2SameNullTaskRes(
            (fun ctx v e -> set ctx v e |> Ok |> Task.result),
            field1,
            field2,
            ?setOrder = setOrder
        )



type OperationHelper<'originalCtx, 'ctx, 'entity, 'id> internal (mapCtx: 'originalCtx -> Task<Result<'ctx, Error list>>)
    =
    inherit OperationHelperWithEntityMapCtx<'originalCtx, 'ctx, 'entity, 'id>(fun ctx _ -> mapCtx ctx)

    member _.Polymorphic =
        PolymorphicOperationHelper<'originalCtx, 'ctx, 'entity, 'id>(mapCtx)

    member _.ForContextTaskRes(mapCtx: 'originalCtx -> 'entity -> Task<Result<'mappedCtx, Error list>>) =
        OperationHelperWithEntityMapCtx<'originalCtx, 'mappedCtx, 'entity, 'id>(mapCtx)

    member this.ForContextAsyncRes(mapCtx: 'originalCtx -> 'entity -> Async<Result<'mappedCtx, Error list>>) =
        this.ForContextTaskRes(Task.liftAsync2 mapCtx)

    member this.ForContextTaskOpt(mapCtx: 'originalCtx -> 'entity -> Task<'mappedCtx option>) =
        this.ForContextTaskRes(fun ctx e -> mapCtx ctx e |> Task.map (Result.requireSome [ opMapCtxFailedNone () ]))

    member this.ForContextAsyncOpt(mapCtx: 'originalCtx -> 'entity -> Async<'mappedCtx option>) =
        this.ForContextTaskOpt(Task.liftAsync2 mapCtx)

    member this.ForContextTask(mapCtx: 'originalCtx -> 'entity -> Task<'mappedCtx>) =
        this.ForContextTaskRes(fun ctx e -> mapCtx ctx e |> Task.map Ok)

    member this.ForContextAsync(mapCtx: 'originalCtx -> 'entity -> Async<'mappedCtx>) =
        this.ForContextTask(Task.liftAsync2 mapCtx)

    member this.ForContextRes(mapCtx: 'originalCtx -> 'entity -> Result<'mappedCtx, Error list>) =
        this.ForContextTaskRes(Task.lift2 mapCtx)

    member this.ForContextOpt(mapCtx: 'originalCtx -> 'entity -> 'mappedCtx option) =
        this.ForContextTaskOpt(Task.lift2 mapCtx)

    member this.ForContext(mapCtx: 'originalCtx -> 'entity -> 'mappedCtx) =
        this.ForContextTaskRes(TaskResult.lift2 mapCtx)

    member _.LookupTaskRes(getById: Func<'ctx, 'id, Task<Result<'entity option, Error list>>>) =
        ResourceLookup<'originalCtx, 'ctx, 'entity, 'id>
            .Create(mapCtx, (fun ctx id -> getById.Invoke(ctx, id)))

    member this.LookupTaskRes(getById: Func<'id, Task<Result<'entity option, Error list>>>) =
        this.LookupTaskRes(fun _ id -> getById.Invoke id)

    member this.LookupAsyncRes(getById: Func<'ctx, 'id, Async<Result<'entity option, Error list>>>) =
        this.LookupTaskRes(Task.liftAsyncFunc2 getById)

    member this.LookupAsyncRes(getById: Func<'id, Async<Result<'entity option, Error list>>>) =
        this.LookupTaskRes(Task.liftAsyncFunc getById)

    member this.LookupTask(getById: Func<'ctx, 'id, Task<'entity option>>) =
        this.LookupTaskRes(fun ctx id -> getById.Invoke(ctx, id) |> Task.map Ok)

    member this.LookupTask(getById: Func<'id, Task<'entity option>>) =
        this.LookupTaskRes(fun _ id -> getById.Invoke id |> Task.map Ok)

    member this.LookupAsync(getById: Func<'ctx, 'id, Async<'entity option>>) =
        this.LookupTask(Task.liftAsyncFunc2 getById)

    member this.LookupAsync(getById: Func<'id, Async<'entity option>>) =
        this.LookupTask(Task.liftAsyncFunc getById)

    member this.LookupRes(getById: Func<'ctx, 'id, Result<'entity option, Error list>>) =
        this.LookupTaskRes(Task.liftFunc2 getById)

    member this.LookupRes(getById: Func<'id, Result<'entity option, Error list>>) =
        this.LookupTaskRes(Task.liftFunc getById)

    member this.Lookup(getById: Func<'ctx, 'id, 'entity option>) =
        this.LookupTaskRes(TaskResult.liftFunc2 getById)

    member this.Lookup(getById: Func<'id, 'entity option>) =
        this.LookupTaskRes(TaskResult.liftFunc getById)

    member _.GetCollectionTaskRes(getCollection: Func<'ctx, Task<Result<'entity list, Error list>>>) =
        GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>
            .Create(
                mapCtx,
                fun _origCtx ctx _req -> getCollection.Invoke ctx |> TaskResult.map (fun xs -> Set.empty, Set.empty, xs)
            )

    member this.GetCollectionTaskRes(getCollection: Func<unit, Task<Result<'entity list, Error list>>>) =
        this.GetCollectionTaskRes(fun (_: 'ctx) -> getCollection.Invoke())

    member _.GetCollectionTaskRes
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Task<Result<RequestParser<'originalCtx, 'entity list>, Error list>>>)
        =
        GetCollectionOperation<'originalCtx, 'ctx, 'entity, 'id>
            .Create(
                mapCtx,
                fun origCtx ctx req ->
                    getRequestParser.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req))
                    |> TaskResult.bind (fun p -> p.ParseWithConsumed())
            )

    member this.GetCollectionAsyncRes(getCollection: Func<'ctx, Async<Result<'entity list, Error list>>>) =
        this.GetCollectionTaskRes(Task.liftAsyncFunc getCollection)

    member this.GetCollectionAsyncRes(getCollection: Func<unit, Async<Result<'entity list, Error list>>>) =
        this.GetCollectionTaskRes(Task.liftAsyncFunc getCollection)

    member this.GetCollectionAsyncRes
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Async<Result<RequestParser<'originalCtx, 'entity list>, Error list>>>)
        =
        this.GetCollectionTaskRes(Task.liftAsyncFunc2 getRequestParser)

    member this.GetCollectionTask(getCollection: Func<'ctx, Task<'entity list>>) =
        this.GetCollectionTaskRes(getCollection.Invoke >> Task.map Ok)

    member this.GetCollectionTask(getCollection: Func<unit, Task<'entity list>>) =
        this.GetCollectionTaskRes(getCollection.Invoke >> Task.map Ok)

    member this.GetCollectionTask
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Task<RequestParser<'originalCtx, 'entity list>>>)
        =
        this.GetCollectionTaskRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Task.map Ok)

    member this.GetCollectionAsync(getCollection: Func<'ctx, Async<'entity list>>) =
        this.GetCollectionTask(Task.liftAsyncFunc getCollection)

    member this.GetCollectionAsync(getCollection: Func<unit, Async<'entity list>>) =
        this.GetCollectionTask(Task.liftAsyncFunc getCollection)

    member this.GetCollectionAsync
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Async<RequestParser<'originalCtx, 'entity list>>>)
        =
        this.GetCollectionTask(Task.liftAsyncFunc2 getRequestParser)

    member this.GetCollectionRes(getCollection: Func<'ctx, Result<'entity list, Error list>>) =
        this.GetCollectionTaskRes(Task.liftFunc getCollection)

    member this.GetCollectionRes(getCollection: Func<unit, Result<'entity list, Error list>>) =
        this.GetCollectionTaskRes(Task.liftFunc getCollection)

    member this.GetCollectionRes
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Result<RequestParser<'originalCtx, 'entity list>, Error list>>)
        =
        this.GetCollectionTaskRes(Task.liftFunc2 getRequestParser)

    member this.GetCollection(getCollection: Func<'ctx, 'entity list>) =
        this.GetCollectionTaskRes(TaskResult.liftFunc getCollection)

    member this.GetCollection(getCollection: Func<unit, 'entity list>) =
        this.GetCollectionTaskRes(TaskResult.liftFunc getCollection)

    member this.GetCollection
        (getRequestParser: Func<'ctx, RequestParserHelper<'originalCtx>, RequestParser<'originalCtx, 'entity list>>)
        =
        this.GetCollectionTaskRes(TaskResult.liftFunc2 getRequestParser)

    member _.PostTaskRes(createEntity: Func<'ctx, Task<Result<'entity, Error list>>>) =
        PostOperation<'originalCtx, 'ctx, 'entity>
            .Create(
                (fun ctx _res -> mapCtx ctx),
                fun _origCtx ctx _res -> createEntity.Invoke ctx |> TaskResult.map (fun e -> Set.empty, Set.empty, e)
            )

    member this.PostTaskRes(createEntity: Func<unit, Task<Result<'entity, Error list>>>) =
        this.PostTaskRes(fun (ctx: 'ctx) -> createEntity.Invoke())

    member _.PostTaskRes
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Task<Result<RequestParser<'originalCtx, 'entity>, Error list>>>)
        =
        PostOperation<'originalCtx, 'ctx, 'entity>
            .Create(
                (fun ctx _ -> mapCtx ctx),
                fun origCtx ctx req ->
                    getRequestParser.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req))
                    |> TaskResult.bind (fun p -> p.ParseWithConsumed())
            )

    member this.PostAsyncRes(createEntity: Func<'ctx, Async<Result<'entity, Error list>>>) =
        this.PostTaskRes(Task.liftAsyncFunc createEntity)

    member this.PostAsyncRes(createEntity: Func<unit, Async<Result<'entity, Error list>>>) =
        this.PostTaskRes(Task.liftAsyncFunc createEntity)

    member this.PostAsyncRes
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Async<Result<RequestParser<'originalCtx, 'entity>, Error list>>>)
        =
        this.PostTaskRes(Task.liftAsyncFunc2 getRequestParser)

    member this.PostTask(createEntity: Func<'ctx, Task<'entity>>) =
        this.PostTaskRes(createEntity.Invoke >> Task.map Ok)

    member this.PostTask(createEntity: Func<unit, Task<'entity>>) =
        this.PostTaskRes(createEntity.Invoke >> Task.map Ok)

    member this.PostTask
        (getRequestParser: Func<'ctx, RequestParserHelper<'originalCtx>, Task<RequestParser<'originalCtx, 'entity>>>)
        =
        this.PostTaskRes(fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Task.map Ok)

    member this.PostAsync(createEntity: Func<'ctx, Async<'entity>>) =
        this.PostTask(Task.liftAsyncFunc createEntity)

    member this.PostAsync(createEntity: Func<unit, Async<'entity>>) =
        this.PostTask(Task.liftAsyncFunc createEntity)

    member this.PostAsync
        (getRequestParser: Func<'ctx, RequestParserHelper<'originalCtx>, Async<RequestParser<'originalCtx, 'entity>>>)
        =
        this.PostTask(Task.liftAsyncFunc2 getRequestParser)

    member this.PostRes(createEntity: Func<'ctx, Result<'entity, Error list>>) =
        this.PostTaskRes(Task.liftFunc createEntity)

    member this.PostRes(createEntity: Func<unit, Result<'entity, Error list>>) =
        this.PostTaskRes(Task.liftFunc createEntity)

    member this.PostRes
        (getRequestParser:
            Func<'ctx, RequestParserHelper<'originalCtx>, Result<RequestParser<'originalCtx, 'entity>, Error list>>)
        =
        this.PostTaskRes(Task.liftFunc2 getRequestParser)

    member this.Post(createEntity: Func<'ctx, 'entity>) =
        this.PostTaskRes(TaskResult.liftFunc createEntity)

    member this.Post(createEntity: Func<unit, 'entity>) =
        this.PostTaskRes(TaskResult.liftFunc createEntity)

    member this.Post
        (getRequestParser: Func<'ctx, RequestParserHelper<'originalCtx>, RequestParser<'originalCtx, 'entity>>)
        =
        this.PostTaskRes(TaskResult.liftFunc2 getRequestParser)

    member _.PostBackRefTaskRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<'ctx * 'backRefEntity, Task<Result<'entity, Error list>>>
        ) =
        let mapCtxWithBackRef ctx req =
            mapCtx ctx
            |> TaskResult.bind (fun mappedCtx -> backRef.Get(ctx, req, None) |> TaskResult.map (fun e -> mappedCtx, e))

        let consumedFieldNames =
            match backRef.FieldName with
            | None -> Set.empty
            | Some fn -> Set.empty.Add fn

        PostOperation<'originalCtx, 'ctx * 'backRefEntity, 'entity>
            .Create(
                mapCtxWithBackRef,
                fun _origCtx ctx _res ->
                    createEntity.Invoke ctx
                    |> TaskResult.map (fun e -> consumedFieldNames, Set.empty, e)
            )

    member this.PostBackRefTaskRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<unit, Task<Result<'entity, Error list>>>
        ) =
        this.PostBackRefTaskRes(backRef, (fun (_ctx: 'ctx, _br: 'backRefEntity) -> createEntity.Invoke()))

    member _.PostBackRefTaskRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            getRequestParser:
                Func<'ctx * 'backRefEntity, RequestParserHelper<'originalCtx>, Task<Result<RequestParser<'originalCtx, 'entity>, Error list>>>
        ) =
        let mapCtxWithBackRef ctx req =
            mapCtx ctx
            |> TaskResult.bind (fun mappedCtx -> backRef.Get(ctx, req, None) |> TaskResult.map (fun e -> mappedCtx, e))

        let addBackRefFieldName =
            match backRef.FieldName with
            | None -> id
            | Some fn -> Set.add fn

        PostOperation<'originalCtx, 'ctx * 'backRefEntity, 'entity>
            .Create(
                mapCtxWithBackRef,
                fun origCtx ctx req ->
                    getRequestParser.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req))
                    |> TaskResult.bind (fun p -> p.ParseWithConsumed())
                    |> TaskResult.map (fun (fns, qns, e) -> addBackRefFieldName fns, qns, e)
            )

    member this.PostBackRefAsyncRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<'ctx * 'backRefEntity, Async<Result<'entity, Error list>>>
        ) =
        this.PostBackRefTaskRes(backRef, Task.liftAsyncFunc createEntity)

    member this.PostBackRefAsyncRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<unit, Async<Result<'entity, Error list>>>
        ) =
        this.PostBackRefTaskRes(backRef, Task.liftAsyncFunc createEntity)

    member this.PostBackRefAsyncRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            getRequestParser:
                Func<'ctx * 'backRefEntity, RequestParserHelper<'originalCtx>, Async<Result<RequestParser<'originalCtx, 'entity>, Error list>>>
        ) =
        this.PostBackRefTaskRes(backRef, Task.liftAsyncFunc2 getRequestParser)

    member this.PostBackRefTask
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<'ctx * 'backRefEntity, Task<'entity>>
        ) =
        this.PostBackRefTaskRes(backRef, createEntity.Invoke >> Task.map Ok)

    member this.PostBackRefTask
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<unit, Task<'entity>>
        ) =
        this.PostBackRefTaskRes(backRef, createEntity.Invoke >> Task.map Ok)

    member this.PostBackRefTask
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            getRequestParser:
                Func<'ctx * 'backRefEntity, RequestParserHelper<'originalCtx>, Task<RequestParser<'originalCtx, 'entity>>>
        ) =
        this.PostBackRefTaskRes(backRef, (fun ctx parse -> getRequestParser.Invoke(ctx, parse) |> Task.map Ok))

    member this.PostBackRefAsync
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<'ctx * 'backRefEntity, Async<'entity>>
        ) =
        this.PostBackRefTask(backRef, Task.liftAsyncFunc createEntity)

    member this.PostBackRefAsync
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<unit, Async<'entity>>
        ) =
        this.PostBackRefTask(backRef, Task.liftAsyncFunc createEntity)

    member this.PostBackRefAsync
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            getRequestParser:
                Func<'ctx * 'backRefEntity, RequestParserHelper<'originalCtx>, Async<RequestParser<'originalCtx, 'entity>>>
        ) =
        this.PostBackRefTask(backRef, Task.liftAsyncFunc2 getRequestParser)

    member this.PostBackRefRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<'ctx * 'backRefEntity, Result<'entity, Error list>>
        ) =
        this.PostBackRefTaskRes(backRef, Task.liftFunc createEntity)

    member this.PostBackRefRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<unit, Result<'entity, Error list>>
        ) =
        this.PostBackRefTaskRes(backRef, Task.liftFunc createEntity)

    member this.PostBackRefRes
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            getRequestParser:
                Func<'ctx * 'backRefEntity, RequestParserHelper<'originalCtx>, Result<RequestParser<'originalCtx, 'entity>, Error list>>
        ) =
        this.PostBackRefTaskRes(backRef, Task.liftFunc2 getRequestParser)

    member this.PostBackRef
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            createEntity: Func<'ctx * 'backRefEntity, 'entity>
        ) =
        this.PostBackRefTaskRes(backRef, TaskResult.liftFunc createEntity)

    member this.PostBackRef(backRef: RequestGetter<'originalCtx, 'backRefEntity>, createEntity: Func<unit, 'entity>) =
        this.PostBackRefTaskRes(backRef, TaskResult.liftFunc createEntity)

    member this.PostBackRef
        (
            backRef: RequestGetter<'originalCtx, 'backRefEntity>,
            getRequestParser:
                Func<'ctx * 'backRefEntity, RequestParserHelper<'originalCtx>, RequestParser<'originalCtx, 'entity>>
        ) =
        this.PostBackRefTaskRes(backRef, TaskResult.liftFunc2 getRequestParser)

    member _.PostCustomTask
        (operation:
            Func<'ctx, RequestParserHelper<'originalCtx>, PostCustomHelper<'originalCtx, 'entity>, Task<Result<HttpHandler, Error list>>>)
        =
        CustomPostOperation<'originalCtx, 'ctx, 'entity>
            .Create(
                mapCtx,
                fun origCtx ctx req helper ->
                    operation.Invoke(ctx, RequestParserHelper<'originalCtx>(origCtx, req), helper)
            )

    member this.PostCustomAsync
        (operation:
            Func<'ctx, RequestParserHelper<'originalCtx>, PostCustomHelper<'originalCtx, 'entity>, Async<Result<HttpHandler, Error list>>>)
        =
        this.PostCustomTask(Task.liftAsyncFunc3 operation)
