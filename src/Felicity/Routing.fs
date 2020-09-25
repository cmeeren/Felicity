module internal Felicity.Routing

open Microsoft.AspNetCore.Http
open Hopac
open FSharp.Control.Tasks.V2.ContextInsensitive
open Giraffe
open Errors


let jsonApiHandler (getCtx: HttpContext -> Job<Result<'ctx, Error list>>) collections : HttpHandler =

  let getCtx (handler: 'ctx -> Request -> HttpHandler) : HttpHandler =
    fun next (httpCtx: HttpContext) ->
      job {
        let serializer = httpCtx.GetService<Serializer<'ctx>> ()
        match! getCtx httpCtx with
        | Error errs -> return! handleErrors errs next httpCtx
        | Ok ctx ->
            let query = httpCtx.Request.Query |> Seq.map (fun kvp -> kvp.Key, kvp.Value.ToString()) |> Map.ofSeq
            let! json = httpCtx.ReadBodyFromRequestAsync ()
            let req = {
              Document = lazy (serializer.DeserializeResourceDocument json)
              IdentifierDocument = lazy (serializer.DeserializeResourceIdentifierDocument json)
              IdentifierCollectionDocument = lazy (serializer.DeserializeResourceIdentifierCollectionDocument json)
              Headers = httpCtx.Request.Headers |> Seq.map (fun kvp -> kvp.Key, kvp.Value.ToString()) |> Map.ofSeq
              Query = query
              Fieldsets = query |> Map.filter (fun k _ -> k.StartsWith "fields[" && k.EndsWith "]") |> Map.toArray |> Array.map (fun (k, v) -> k.Substring(7, k.Length - 8), v.Split ',' |> Set.ofArray) |> Map.ofArray
              Includes = query |> Map.tryFind "include" |> Option.map (fun paths -> paths.Split ',' |> Array.filter ((<>) "") |> Array.map (fun path -> path.Split '.' |> Array.filter ((<>) "")) |> Array.map (Array.toList) |> Array.toList) |> Option.defaultValue []
            }
            return! handler ctx req next httpCtx
      }
      |> Job.startAsTask


  let validateRequest : HttpHandler =
    fun next httpCtx ->

      let errs =
        [
          if not <| RequestValidation.acceptsJsonApi httpCtx then invalidAccept
          if RequestValidation.hasNonJsonApiContent httpCtx then invalidContentType
          if RequestValidation.allJsonApiAcceptsHaveParams httpCtx then invalidAcceptParams
          if RequestValidation.jsonApiContentTypeHasParams httpCtx then invalidContentTypeParams

          match RequestValidation.getIllegalQueryStringParams httpCtx with
          | [] -> ()
          | names -> yield! names |> List.map illegalQueryParamName
        ]
      match errs with
      | [] -> next httpCtx
      | errs -> handleErrors errs next httpCtx


  let lockResourceForModification (ctx: 'ctx) req collName resId : HttpHandler =
    fun next httpCtx ->
      task {
        match ResourceModule.lockSpec<'ctx> collName with
        | None -> return! next httpCtx
        | Some lockSpec ->
            match! lockSpec.GetId ctx req resId |> Job.startAsTask with
            | None -> return! next httpCtx
            | Some resIdToLock ->
                let! locker =
                  match lockSpec.CustomLock with
                  | None ->
                      let queueFactory = httpCtx.GetService<SemaphoreQueueFactory<'ctx>>()
                      let queue = queueFactory.GetFor(lockSpec.CollName, resIdToLock)
                      queue.Lock lockSpec.Timeout
                  | Some getLock ->
                      getLock ctx resIdToLock |> Job.startAsTask
                match locker with
                | Some locker ->
                    use _ = locker
                    return! next httpCtx
                | None ->
                    return! handleErrors [lockTimeout] next httpCtx
      }


  choose [
    for collName, ops in collections |> Map.toSeq do

      subRoute ("/" + collName) (getCtx (fun ctx req -> validateRequest >=> choose [


        // Collection operations

        routex "/?" >=> choose [
          GET_HEAD >=>
            match ops.getCollection with
            | None -> handleErrors [collGetNotAllowed collName]
            | Some getColl -> getColl ctx req

          POST >=> 
            match ops.postCollection with
            | None -> handleErrors [collPostNotAllowed collName]
            | Some postColl -> lockResourceForModification ctx req collName None >=> postColl ctx req

          let allowHeader =
            [ if ops.getCollection.IsSome then "GET"; "HEAD"
              if ops.postCollection.IsSome then "POST" ]
            |> String.concat ", "

          fun next (httpCtx: HttpContext) ->
            let method = httpCtx.Request.Method
            handleErrors [methodNotAllowed method allowHeader] next httpCtx
        ]


        // Specific resource operations

        subRoutef "/%s" (fun resourceId ->

          match ops.resourceOperations.getByIdBoxedHandler with
          | None -> handleErrors [collLookupNotSupported collName]
          | Some getById ->
              let lookup f = getById ctx resourceId f

              choose [

                routex "/?" >=> choose [

                  GET_HEAD >=>
                    match ops.resourceOperations.get with
                    | None -> handleErrors [resGetNotSupportedForAnyResource collName]
                    | Some get -> lookup (get ctx req)

                  PATCH >=>
                    match ops.resourceOperations.patch with
                    | None -> handleErrors [resPatchNotSupportedForAnyResource collName]
                    | Some patch -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (patch ctx req)

                  DELETE >=>
                    match ops.resourceOperations.delete with
                    | None -> handleErrors [resDeleteNotSupportedForAnyResource collName]
                    | Some delete -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (delete ctx req)

                  let allowedMethods =
                    [ if ops.resourceOperations.get.IsSome then "GET"; "HEAD"
                      if ops.resourceOperations.patch.IsSome then "PATCH"
                      if ops.resourceOperations.delete.IsSome then "DELETE" ]
                    |> String.concat ", "

                  fun next (httpCtx: HttpContext) ->
                    let method = httpCtx.Request.Method
                    handleErrors [methodNotAllowed method allowedMethods] next httpCtx
                ]


                // Resource relationship operations

                for relName, rel in ops.resourceOperations.relationships |> Map.toList do

                  // Related
                  routex ("/" + relName + "/?")
                  >=> choose [

                    GET_HEAD >=>
                      match rel.getRelated with
                      | None -> handleErrors [getRelNotDefinedForAnyResource relName collName]
                      | Some get -> lookup (get ctx req)

                    let allowHeader =
                      [ if rel.getRelated.IsSome then "GET"; "HEAD" ]
                      |> String.concat ", "

                    fun next (httpCtx: HttpContext) ->
                      let method = httpCtx.Request.Method
                      handleErrors [methodNotAllowed method allowHeader] next httpCtx

                  ]

                  // Self
                  routex ("/relationships/" + relName + "/?")
                  >=> choose [

                    GET_HEAD >=>
                      match rel.getSelf with
                      | None -> handleErrors [getRelNotDefinedForAnyResource relName collName]
                      | Some get -> lookup (get ctx req)

                    PATCH >=>
                      match rel.patchSelf with
                      | None -> handleErrors [patchRelSelfNotAllowedForAnyResource relName collName rel.postSelf.IsSome rel.deleteSelf.IsSome]
                      | Some patch -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (patch ctx req)

                    POST >=>
                      match rel.postSelf with
                      | None -> handleErrors [postToManyRelSelfNotAllowedForAnyResource relName collName rel.patchSelf.IsSome rel.deleteSelf.IsSome]
                      | Some post -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (post ctx req)

                    DELETE >=>
                      match rel.deleteSelf with
                      | None -> handleErrors [deleteToManyRelSelfNotAllowedForAnyResource relName collName rel.patchSelf.IsSome rel.postSelf.IsSome]
                      | Some delete -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (delete ctx req)

                    let allowHeader =
                      [ if rel.getSelf.IsSome then "GET"; "HEAD"
                        if rel.patchSelf.IsSome then "PATCH"
                        if rel.postSelf.IsSome then "POST"
                        if rel.deleteSelf.IsSome then "DELETE" ]
                      |> String.concat ", "

                    fun next (httpCtx: HttpContext) ->
                      let method = httpCtx.Request.Method
                      handleErrors [methodNotAllowed method allowHeader] next httpCtx

                  ]



                // Resource link operations

                for linkName, link in ops.resourceOperations.links |> Map.toList do

                  routex ("/" + linkName + "/?")
                  >=> choose [

                    let allowHeader =
                      [ if link.get.IsSome then "GET"; "HEAD"
                        if link.post.IsSome then "POST"
                        if link.patch.IsSome then "PATCH"
                        if link.delete.IsSome then "DELETE" ]
                      |> String.concat ", "

                    GET_HEAD >=>
                      match link.get with
                      | None -> handleErrors [customOpVerbNotDefinedForAnyResource linkName "GET" collName allowHeader]
                      | Some get -> lookup (get ctx req)

                    POST >=>
                      match link.post with
                      | None -> handleErrors [customOpVerbNotDefinedForAnyResource linkName "POST" collName allowHeader]
                      | Some post -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (post ctx req)

                    PATCH >=>
                      match link.patch with
                      | None -> handleErrors [customOpVerbNotDefinedForAnyResource linkName "PATCH" collName allowHeader]
                      | Some patch -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (patch ctx req)

                    DELETE >=>
                      match link.delete with
                      | None -> handleErrors [customOpVerbNotDefinedForAnyResource linkName "DELETE" collName allowHeader]
                      | Some delete -> lockResourceForModification ctx req collName (Some resourceId) >=> lookup (delete ctx req)

                    fun next (httpCtx: HttpContext) ->
                      let method = httpCtx.Request.Method
                      handleErrors [methodNotAllowed method allowHeader] next httpCtx

                  ]


                // Fallback

                subRoute "/relationships" (choose [
                  routex "/?" >=> handleErrors [invalidPath "relationships" collName]

                  subRoutef "/%s" (fun relName -> choose [
                    routex "/?" >=> handleErrors [linkOrRelationshipDoesNotExistForAnyResource relName collName]
                    subRoutef "/%s" (fun path2 ->
                      handleErrors [invalidPath (sprintf "relationships/%s/%s" relName path2) collName]
                    )
                  ])
                ])

                subRoutef "/%s" (fun linkOrRelName ->
                  choose [
                    routex "/?" >=> handleErrors [linkOrRelationshipDoesNotExistForAnyResource linkOrRelName collName]
                    subRoutef "/%s" (fun path2 ->
                      handleErrors [invalidPath (sprintf "%s/%s" linkOrRelName path2) collName]
                    )
                  ]
                )

              ]
        )


      ]))
  ]
