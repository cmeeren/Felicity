namespace Felicity

open System
open System.Reflection
open System.Text.Json.Serialization
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Errors
open Microsoft.Extensions.Logging


type internal RelationshipOperations<'ctx> = {
    getRelated: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
    getSelf: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
    postSelf: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
    patchSelf: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
    deleteSelf: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
}


type internal LinkOperations<'ctx> = {
    get:
        ((RequestValidationConfig -> HttpHandler)
            -> 'ctx
            -> Request
            -> ResourceDefinition<'ctx>
            -> BoxedEntity
            -> HttpHandler) option
    post:
        ((RequestValidationConfig -> HttpHandler)
            -> 'ctx
            -> Request
            -> ResourceDefinition<'ctx>
            -> BoxedEntity
            -> HttpHandler) option
    patch:
        ((RequestValidationConfig -> HttpHandler)
            -> 'ctx
            -> Request
            -> ResourceDefinition<'ctx>
            -> BoxedEntity
            -> HttpHandler) option
    delete:
        ((RequestValidationConfig -> HttpHandler)
            -> 'ctx
            -> Request
            -> ResourceDefinition<'ctx>
            -> BoxedEntity
            -> HttpHandler) option
}


type internal ResourceOperations<'ctx> = {
    getByIdBoxedHandler:
        ('ctx -> ResourceId -> (ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) -> HttpHandler) option
    get: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
    patch: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
    delete: ('ctx -> Request -> ResourceDefinition<'ctx> -> BoxedEntity -> HttpHandler) option
    relationships: Map<RelationshipName, RelationshipOperations<'ctx>>
    links: Map<LinkName, LinkOperations<'ctx>>
}


type internal CollectionOperations<'ctx> = {
    getCollection: ('ctx -> Request -> HttpHandler) option
    postCollection: ('ctx -> Request -> HttpHandler) option
    resourceOperations: ResourceOperations<'ctx>
}



module internal RoutingOperations =

    let responseBuilder resourceModuleMap getBaseUrl : ResponseBuilder<'ctx> =

        { new ResponseBuilder<'ctx> with
            member _.Write httpCtx ctx req rDefEntity = task {
                let linkCfg = httpCtx.RequestServices.GetRequiredService<LinkConfig<'ctx>>()
                let resourceDef, e = rDefEntity
                let baseUrl = getBaseUrl httpCtx

                let! main, included =
                    ResourceBuilder.ResourceBuilder(
                        resourceModuleMap,
                        baseUrl,
                        [],
                        linkCfg,
                        httpCtx,
                        ctx,
                        req,
                        resourceDef,
                        e
                    )
                    |> ResourceBuilder.buildOne (httpCtx.GetService<ILoggerFactory>())

                return {
                    ResourceDocument.jsonapi = Skip // support later when valid use-cases arrive
                    links = Skip // support later when valid use-cases arrive; remember to check LinkConfig
                    meta =
                        httpCtx.GetService<MetaGetter<'ctx>>().GetMeta ctx
                        |> Include
                        |> Skippable.filter (fun x -> x.Count > 0)
                    data = Some main
                    included =
                        if req.Query.ContainsKey "include" then
                            Include included
                        else
                            Skip
                }
            }

            member _.WriteList httpCtx ctx req rDefsEntities = task {
                let linkCfg = httpCtx.RequestServices.GetRequiredService<LinkConfig<'ctx>>()
                let baseUrl = getBaseUrl httpCtx

                let! main, included =
                    rDefsEntities
                    |> List.map (fun (rDef, e) ->
                        ResourceBuilder.ResourceBuilder(
                            resourceModuleMap,
                            baseUrl,
                            [],
                            linkCfg,
                            httpCtx,
                            ctx,
                            req,
                            rDef,
                            e
                        ))
                    |> ResourceBuilder.build (httpCtx.GetService<ILoggerFactory>())

                return {
                    ResourceCollectionDocument.jsonapi = Skip // support later when valid use-cases arrive
                    links = Skip // support later when valid use-cases arrive; remember to check LinkConfig
                    meta =
                        httpCtx.GetService<MetaGetter<'ctx>>().GetMeta ctx
                        |> Include
                        |> Skippable.filter (fun x -> x.Count > 0)
                    data = main
                    included =
                        if req.Query.ContainsKey "include" then
                            Include included
                        else
                            Skip
                }
            }

            member _.WriteOpt httpCtx ctx req rDefEntity = task {
                let linkCfg = httpCtx.RequestServices.GetRequiredService<LinkConfig<'ctx>>()
                let baseUrl = getBaseUrl httpCtx

                let! main, included =
                    rDefEntity
                    |> Option.map (fun (rDef, e) ->
                        ResourceBuilder.ResourceBuilder(
                            resourceModuleMap,
                            baseUrl,
                            [],
                            linkCfg,
                            httpCtx,
                            ctx,
                            req,
                            rDef,
                            e
                        )
                        |> ResourceBuilder.buildOne (httpCtx.GetService<ILoggerFactory>())
                        |> Task.map (fun (res, inc) -> Some res, Include inc))
                    |> Option.defaultValue (Task.result (None, Skip))

                return {
                    ResourceDocument.jsonapi = Skip // support later when valid use-cases arrive
                    links = Skip // support later when valid use-cases arrive; remember to check LinkConfig
                    meta =
                        httpCtx.GetService<MetaGetter<'ctx>>().GetMeta ctx
                        |> Include
                        |> Skippable.filter (fun x -> x.Count > 0)
                    data = main
                    included = if req.Query.ContainsKey "include" then included else Skip
                }
            }
        }

    let boxedPatcher (resourceModule: Type) : BoxedPatcher<'ctx> =
        let constraintsField =
            resourceModule.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<ConstrainedField<'ctx>>)
            |> Array.exists (fun f -> f.HasConstraints)
            |> function
                | false -> None
                | true ->
                    Some
                        { new FieldSetter<'ctx> with
                            member _.Names = Set.singleton "constraints"
                            member _.SetOrder = 0

                            member _.Set ctx req e _ =
                                match req.Document.Value with
                                | Ok(Some {
                                              data = Some { attributes = Include attrVals }
                                          }) when attrVals.ContainsKey "constraints" ->
                                    Error [ setAttrReadOnly "constraints" "/data/attributes/constraints" ]
                                    |> Task.result
                                | _ -> Ok(e, false) |> Task.result
                        }

        let fields =
            resourceModule.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<FieldSetter<'ctx>>)
            |> Array.append (constraintsField |> Option.toArray)
            |> Array.sortBy (fun f -> f.SetOrder)

        let numSetters =
            fields
            |> Array.collect (fun f -> f.Names |> Set.toArray)
            |> Array.countBy id
            |> Map.ofArray

        fun ctx req consumedFields entity -> task {
            let mutable result = Ok(entity, Set.empty)

            for field in fields do
                if not <| Set.intersects field.Names consumedFields then
                    match result with
                    | Error _ -> ()
                    | Ok(e, fns) ->
                        match! field.Set ctx req e numSetters with
                        | Ok(e, wasSet) -> result <- Ok(e, (if wasSet then Set.union field.Names fns else fns))
                        | Error errs -> result <- Error errs

            return result
        }


    let getResource<'ctx> resourceModuleMap getBaseUrl collName (resourceModules: Type[]) =
        let opsAndResourceDefs =
            resourceModules
            |> Array.choose (fun m ->
                let op = ResourceModule.getResourceOperation<'ctx> m
                op |> Option.map (fun op -> op, ResourceModule.resourceDefinition<'ctx> m))

        if opsAndResourceDefs.Length = 0 then
            None
        else
            let opLookup =
                opsAndResourceDefs
                |> Array.map (fun (op, resDef) ->
                    let builder = responseBuilder resourceModuleMap getBaseUrl
                    resDef.TypeName, (fun ctx req entity -> op.Run resDef ctx req entity builder))
                |> dict

            Some
            <| fun ctx (req: Request) (resDef: ResourceDefinition<'ctx>) entity ->
                fun next httpCtx ->
                    match opLookup.TryGetValue resDef.TypeName with
                    | false, _ -> handleErrors [ resGetNotSupportedPolymorphic resDef.TypeName collName ] next httpCtx
                    | true, run -> run ctx req entity next httpCtx


    let patchResource<'ctx> resourceModuleMap getBaseUrl collName (resourceModules: Type[]) =
        let opsAndResourceDefs =
            resourceModules
            |> Array.choose (fun m ->
                let patchOp =
                    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
                    |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<PatchOperation<'ctx>>)
                    |> function
                        | [||] -> None
                        | [| x |] -> Some x
                        | xs ->
                            failwith
                                $"Resource module %s{collName} contains %i{xs.Length} public PATCH operations; only one is allowed"

                patchOp
                |> Option.map (fun op ->
                    op, ResourceModule.preconditions<'ctx> m, ResourceModule.resourceDefinition<'ctx> m))

        if opsAndResourceDefs.Length = 0 then
            None
        else
            let opLookup =
                opsAndResourceDefs
                |> Array.map (fun (patchOp, prec, resDef) ->
                    let builder = responseBuilder resourceModuleMap getBaseUrl
                    let prec = prec |> Option.defaultValue Preconditions.noop

                    let patch =
                        resourceModuleMap
                        |> Map.tryFind resDef.TypeName
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"Framework bug: Resource module map does not contain entry for resource type %s{resDef.TypeName}")
                        |> boxedPatcher

                    resDef.TypeName, (fun ctx req entity -> patchOp.Run resDef ctx req prec entity patch builder))
                |> dict

            Some
            <| fun ctx (req: Request) (resDef: ResourceDefinition<'ctx>) entity ->
                fun next httpCtx ->
                    match opLookup.TryGetValue resDef.TypeName with
                    | false, _ -> handleErrors [ resPatchNotSupportedPolymorphic resDef.TypeName collName ] next httpCtx
                    | true, run -> run ctx req entity next httpCtx


    let deleteResource<'ctx> resourceModuleMap getBaseUrl collName (resourceModules: Type[]) =
        let opsAndResourceDefs =
            resourceModules
            |> Array.choose (fun m ->
                let op =
                    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
                    |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<DeleteOperation<'ctx>>)
                    |> function
                        | [||] -> None
                        | [| x |] -> Some x
                        | xs ->
                            failwith
                                $"Resource module %s{collName} contains %i{xs.Length} public DELETE operations; only one is allowed"

                op
                |> Option.map (fun op ->
                    op, ResourceModule.preconditions<'ctx> m, ResourceModule.resourceDefinition<'ctx> m))

        if opsAndResourceDefs.Length = 0 then
            None
        else
            let opLookup =
                opsAndResourceDefs
                |> Array.map (fun (op, prec, resDef) ->
                    let builder = responseBuilder resourceModuleMap getBaseUrl
                    let prec = prec |> Option.defaultValue Preconditions.noop
                    resDef.TypeName, (fun ctx req entity -> op.Run resDef ctx req prec entity builder))
                |> dict

            Some
            <| fun ctx (req: Request) (resDef: ResourceDefinition<'ctx>) entity ->
                fun next httpCtx ->
                    match opLookup.TryGetValue resDef.TypeName with
                    | false, _ ->
                        handleErrors [ resDeleteNotSupportedPolymorphic resDef.TypeName collName ] next httpCtx
                    | true, run -> run ctx req entity next httpCtx


    let relationshipOperations
        (resourceModuleMap: Map<_, _>)
        getBaseUrl
        collName
        (resourceModules: Type[])
        : Map<RelationshipName, RelationshipOperations<'ctx>> =
        resourceModules
        |> Array.collect (fun m ->
            m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<RelationshipHandlers<'ctx>>)
            |> Array.map (fun op -> op, ResourceModule.preconditions<'ctx> m, ResourceModule.resourceDefinition<'ctx> m))
        |> Array.groupBy (fun (op, _, _) -> op.Name)
        |> Array.map (fun (relName, opsAndResDefs) ->
            let opsMap =
                opsAndResDefs
                |> Array.map (fun (op, preconditions, rDef) ->
                    rDef.TypeName, (op, preconditions |> Option.defaultValue Preconditions.noop))
                |> dict

            let builder = responseBuilder resourceModuleMap getBaseUrl

            let hasGetRelated =
                opsAndResDefs |> Array.exists (fun (op, _, _) -> op.GetRelated.IsSome)

            let hasGetSelf = opsAndResDefs |> Array.exists (fun (op, _, _) -> op.GetSelf.IsSome)

            let hasPostSelf =
                opsAndResDefs |> Array.exists (fun (op, _, _) -> op.PostSelf.IsSome)

            let hasPatchSelf =
                opsAndResDefs |> Array.exists (fun (op, _, _) -> op.PatchSelf.IsSome)

            let hasDeleteSelf =
                opsAndResDefs |> Array.exists (fun (op, _, _) -> op.DeleteSelf.IsSome)

            relName,
            {
                getRelated =
                    if not hasGetRelated then
                        None
                    else
                        Some
                        <| fun ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ relNotDefinedPolymorphic relName resDef.TypeName collName ]
                            | true, (op, _) ->
                                match op.GetRelated with
                                | None -> handleErrors [ getRelNotDefinedPolymorphic relName resDef.TypeName collName ]
                                | Some getRel -> getRel ctx req entity resDef builder
                getSelf =
                    if not hasGetSelf then
                        None
                    else
                        Some
                        <| fun ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ relNotDefinedPolymorphic relName resDef.TypeName collName ]
                            | true, (op, _) ->
                                match op.GetSelf with
                                | None -> handleErrors [ getRelNotDefinedPolymorphic relName resDef.TypeName collName ]
                                | Some getSelf -> getSelf ctx req entity resDef builder
                postSelf =
                    if not hasPostSelf then
                        None
                    else
                        Some
                        <| fun ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ relNotDefinedPolymorphic relName resDef.TypeName collName ]
                            | true, (op, preconditions) ->
                                match op.PostSelf with
                                | None ->
                                    handleErrors [
                                        postToManyRelSelfNotAllowedPolymorphic
                                            relName
                                            resDef.TypeName
                                            op.PatchSelf.IsSome
                                            op.DeleteSelf.IsSome
                                            collName
                                    ]
                                | Some postSelf -> postSelf ctx req preconditions entity resDef builder
                patchSelf =
                    if not hasPatchSelf then
                        None
                    else
                        Some
                        <| fun ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ relNotDefinedPolymorphic relName resDef.TypeName collName ]
                            | true, (op, preconditions) ->
                                match op.PatchSelf with
                                | None ->
                                    if op.IsToMany then
                                        if op.IsSettableButNotGettable then
                                            handleErrors [
                                                patchRelSelfSettableButNotGettablePolymorphic
                                                    relName
                                                    resDef.TypeName
                                                    collName
                                            ]
                                        elif op.PostSelf.IsNone && op.DeleteSelf.IsNone then
                                            handleErrors [ modifyRelSelfReadOnly relName resDef.TypeName ]
                                        else
                                            handleErrors [
                                                patchToManyRelSelfNotAllowedPolymorphic
                                                    relName
                                                    resDef.TypeName
                                                    op.PostSelf.IsSome
                                                    op.DeleteSelf.IsSome
                                                    collName
                                            ]
                                    else if op.IsSettableButNotGettable then
                                        handleErrors [
                                            patchRelSelfSettableButNotGettablePolymorphic
                                                relName
                                                resDef.TypeName
                                                collName
                                        ]
                                    else
                                        handleErrors [ modifyRelSelfReadOnly relName resDef.TypeName ]
                                | Some patchSelf ->
                                    patchSelf ctx req resDef.TypeName preconditions entity resDef builder
                deleteSelf =
                    if not hasDeleteSelf then
                        None
                    else
                        Some
                        <| fun ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ relNotDefinedPolymorphic relName resDef.TypeName collName ]
                            | true, (op, preconditions) ->
                                match op.DeleteSelf with
                                | None ->
                                    handleErrors [
                                        deleteToManyRelSelfNotAllowedPolymorphic
                                            relName
                                            resDef.TypeName
                                            op.PatchSelf.IsSome
                                            op.PostSelf.IsSome
                                            collName
                                    ]
                                | Some deleteSelf -> deleteSelf ctx req preconditions entity resDef builder
            })
        |> Map.ofArray


    let linkOperations
        (resourceModuleMap: Map<_, _>)
        getBaseUrl
        collName
        (resourceModules: Type[])
        : Map<LinkName, LinkOperations<'ctx>> =
        resourceModules
        |> Array.collect (fun m ->
            m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
            |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<CustomOperation<'ctx>>)
            |> Array.map (fun op -> op, ResourceModule.preconditions<'ctx> m, ResourceModule.resourceDefinition<'ctx> m))
        |> Array.groupBy (fun (op, _, _) -> op.Name)
        |> Array.map (fun (opName, opsAndResDefs) ->

            let opsMap =
                opsAndResDefs
                |> Array.map (fun (op, prec, rDef) ->
                    let prec = prec |> Option.defaultValue Preconditions.noop
                    rDef.TypeName, (op, prec))
                |> dict

            let getResponder ctx req =
                Responder(responseBuilder resourceModuleMap getBaseUrl, ctx, req)

            let hasGet = opsAndResDefs |> Array.exists (fun (op, _, _) -> op.Get.IsSome)
            let hasPost = opsAndResDefs |> Array.exists (fun (op, _, _) -> op.Post.IsSome)
            let hasPatch = opsAndResDefs |> Array.exists (fun (op, _, _) -> op.Patch.IsSome)
            let hasDelete = opsAndResDefs |> Array.exists (fun (op, _, _) -> op.Delete.IsSome)

            let getAllowHeader (op: CustomOperation<'ctx>) =
                [
                    if op.Get.IsSome then
                        "GET"
                        "HEAD"
                    if op.Post.IsSome then
                        "POST"
                    if op.Patch.IsSome then
                        "PATCH"
                    if op.Delete.IsSome then
                        "DELETE"
                ]
                |> String.concat ", "

            opName,
            {
                get =
                    if not hasGet then
                        None
                    else
                        Some
                        <| fun getValidationHandler ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ customOpNotDefinedPolymorphic opName resDef.TypeName collName ]
                            | true, (op, _) ->
                                match op.Get with
                                | None ->
                                    handleErrors [
                                        customOpVerbNotDefinedPolymorphic
                                            opName
                                            resDef.TypeName
                                            "GET"
                                            (getAllowHeader op)
                                            collName
                                    ]
                                | Some get -> get getValidationHandler ctx req (getResponder ctx req) entity
                post =
                    if not hasPost then
                        None
                    else
                        Some
                        <| fun getValidationHandler ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ customOpNotDefinedPolymorphic opName resDef.TypeName collName ]
                            | true, (op, prec) ->
                                match op.Post with
                                | None ->
                                    handleErrors [
                                        customOpVerbNotDefinedPolymorphic
                                            opName
                                            resDef.TypeName
                                            "POST"
                                            (getAllowHeader op)
                                            collName
                                    ]
                                | Some post -> post getValidationHandler ctx req (getResponder ctx req) prec entity
                patch =
                    if not hasPatch then
                        None
                    else
                        Some
                        <| fun getValidationHandler ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ customOpNotDefinedPolymorphic opName resDef.TypeName collName ]
                            | true, (op, prec) ->
                                match op.Patch with
                                | None ->
                                    handleErrors [
                                        customOpVerbNotDefinedPolymorphic
                                            opName
                                            resDef.TypeName
                                            "PATCH"
                                            (getAllowHeader op)
                                            collName
                                    ]
                                | Some patch -> patch getValidationHandler ctx req (getResponder ctx req) prec entity
                delete =
                    if not hasDelete then
                        None
                    else
                        Some
                        <| fun getValidationHandler ctx req resDef entity ->
                            match opsMap.TryGetValue resDef.TypeName with
                            | false, _ -> handleErrors [ customOpNotDefinedPolymorphic opName resDef.TypeName collName ]
                            | true, (op, prec) ->
                                match op.Delete with
                                | None ->
                                    handleErrors [
                                        customOpVerbNotDefinedPolymorphic
                                            opName
                                            resDef.TypeName
                                            "DELETE"
                                            (getAllowHeader op)
                                            collName
                                    ]
                                | Some delete -> delete getValidationHandler ctx req (getResponder ctx req) prec entity
            })
        |> Map.ofArray



    let resourceOperations resourceModuleMap getBaseUrl collName (resourceModules: Type[]) : ResourceOperations<'ctx> = {
        getByIdBoxedHandler =
            ResourceModule.resourceLookup<'ctx> collName resourceModules
            |> Option.map (fun op ->
                fun ctx rawId handler ->
                    fun next httpCtx -> task {
                        match! op.GetByIdBoxed ctx rawId with
                        | Error errs -> return! handleErrors errs next httpCtx
                        | Ok None -> return! handleErrors [ resourceNotFound collName rawId ] next httpCtx
                        | Ok(Some(resDef, entity)) -> return! handler resDef entity next httpCtx
                    })
        get = getResource resourceModuleMap getBaseUrl collName resourceModules
        patch = patchResource resourceModuleMap getBaseUrl collName resourceModules
        delete = deleteResource resourceModuleMap getBaseUrl collName resourceModules
        relationships = relationshipOperations resourceModuleMap getBaseUrl collName resourceModules
        links = linkOperations resourceModuleMap getBaseUrl collName resourceModules
    }


    let getCollection<'ctx> resourceModuleMap getBaseUrl collName (resourceModules: Type[]) =
        let nonPolymorphicOperations =
            resourceModules
            |> Array.choose (fun m ->
                m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
                |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<GetCollectionOperation<'ctx>>)
                |> Array.tryHead
                |> Option.map (fun op ->
                    let rDef = ResourceModule.resourceDefinition<'ctx> m
                    let builder = responseBuilder resourceModuleMap getBaseUrl
                    fun ctx req -> op.Run rDef ctx req builder))

        let polymorphicOperations =
            resourceModules
            |> Array.choose (fun m ->
                m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
                |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<PolymorphicGetCollectionOperation<'ctx>>)
                |> Array.tryHead
                |> Option.map (fun op ->
                    let thisCollName = (ResourceModule.resourceDefinition<'ctx> m).CollectionName

                    let collectionResTypes =
                        resourceModules
                        |> Array.map ResourceModule.resourceDefinition<'ctx>
                        |> Array.filter (fun resDef -> resDef.CollectionName = thisCollName)
                        |> Array.map (fun resDef -> resDef.TypeName)
                        |> Array.toList

                    let builder = responseBuilder resourceModuleMap getBaseUrl
                    fun ctx req -> op.Run collectionResTypes ctx req builder))

        Array.concat [ nonPolymorphicOperations; polymorphicOperations ]
        |> function
            | [||] -> None
            | [| x |] -> Some x
            | xs ->
                failwith
                    $"%i{xs.Length} public GET resource operations specified for collection name %s{collName}; only one is allowed"


    let postCollection<'ctx> resourceModuleMap getBaseUrl collName (resourceModules: Type[]) =
        let opsAndResourceDefs =
            resourceModules
            |> Array.choose (fun m ->
                ResourceModule.postOperation<'ctx> m
                |> Option.map (fun op -> op, ResourceModule.resourceDefinition<'ctx> m))

        if opsAndResourceDefs.Length = 0 then
            None
        else
            let opLookup =
                opsAndResourceDefs
                |> Array.map (fun (op, resDef) ->
                    let builder = responseBuilder resourceModuleMap getBaseUrl

                    let patch =
                        resourceModuleMap
                        |> Map.tryFind resDef.TypeName
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"Framework bug: Resource module map does not contain entry for resource type %s{resDef.TypeName}")
                        |> boxedPatcher

                    resDef.TypeName, (fun ctx req -> op.Run collName resDef ctx req patch builder))
                |> dict

            let allowedTypes =
                opsAndResourceDefs
                |> Array.map (fun (_, resDef) -> resDef.TypeName)
                |> List.ofArray
                |> List.sort

            Some
            <| fun ctx (req: Request) ->
                fun next httpCtx ->
                    match req.Document.Value with
                    | Error errs -> handleErrors errs next httpCtx
                    | Ok None -> handleErrors [ collPostMissingResourceObject "" ] next httpCtx
                    | Ok(Some { data = None }) -> handleErrors [ collPostMissingResourceObject "/data" ] next httpCtx
                    | Ok(Some { data = Some { ``type`` = t } }) ->
                        match opLookup.TryGetValue t with
                        | false, _ ->
                            handleErrors [ collPostTypeNotAllowed collName t allowedTypes "/data/type" ] next httpCtx
                        | true, run -> run ctx req next httpCtx


    let collectionOperations<'ctx>
        resourceModuleMap
        getBaseUrl
        collName
        (resourceModules: Type[])
        : CollectionOperations<'ctx> =
        {
            getCollection = getCollection resourceModuleMap getBaseUrl collName resourceModules
            postCollection = postCollection resourceModuleMap getBaseUrl collName resourceModules
            resourceOperations = resourceOperations resourceModuleMap getBaseUrl collName resourceModules
        }
