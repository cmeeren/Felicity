module internal Felicity.Routing

open Microsoft.AspNetCore.Http
open Hopac
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


  validateRequest
  >=> 
    choose [
      for collName, ops in collections |> Map.toSeq do

        subRoute ("/" + collName) (getCtx (fun ctx req -> choose [


          // Collection operations

          routex "/?" >=> choose [
            GET_HEAD >=>
              match ops.getCollection with
              | None -> handleErrors [collGetNotAllowed collName]
              | Some getColl -> getColl ctx req

            POST >=> 
              match ops.postCollection with
              | None -> handleErrors [collPostNotAllowed collName]
              | Some postColl -> postColl ctx req

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
              getById ctx resourceId (fun resDef entity ->

                choose [

                  routex "/?" >=> choose [

                    GET_HEAD >=>
                      match ops.resourceOperations.get with
                      | None -> handleErrors [resGetNotSupportedForAnyResource collName]
                      | Some get -> get ctx req resDef entity

                    PATCH >=>
                      match ops.resourceOperations.patch with
                      | None -> handleErrors [resPatchNotSupportedForAnyResource collName]
                      | Some patch -> patch ctx req resDef entity

                    DELETE >=>
                      match ops.resourceOperations.delete with
                      | None -> handleErrors [resDeleteNotSupportedForAnyResource collName]
                      | Some delete -> delete ctx req resDef entity

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
                        | Some get -> get ctx req resDef entity

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
                        | Some get -> get ctx req resDef entity

                      PATCH >=>
                        match rel.patchSelf with
                        | None -> handleErrors [patchRelSelfNotAllowedForAnyResource relName collName rel.postSelf.IsSome rel.deleteSelf.IsSome]
                        | Some patch -> patch ctx req resDef entity

                      POST >=>
                        match rel.postSelf with
                        | None -> handleErrors [postToManyRelSelfNotAllowedForAnyResource relName collName rel.patchSelf.IsSome rel.deleteSelf.IsSome]
                        | Some post -> post ctx req resDef entity

                      DELETE >=>
                        match rel.deleteSelf with
                        | None -> handleErrors [deleteToManyRelSelfNotAllowedForAnyResource relName collName rel.patchSelf.IsSome rel.postSelf.IsSome]
                        | Some delete -> delete ctx req resDef entity

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
                        | Some delete -> delete ctx req resDef entity

                      POST >=>
                        match link.post with
                        | None -> handleErrors [customOpVerbNotDefinedForAnyResource linkName "POST" collName allowHeader]
                        | Some delete -> delete ctx req resDef entity

                      PATCH >=>
                        match link.patch with
                        | None -> handleErrors [customOpVerbNotDefinedForAnyResource linkName "PATCH" collName allowHeader]
                        | Some delete -> delete ctx req resDef entity

                      DELETE >=>
                        match link.delete with
                        | None -> handleErrors [customOpVerbNotDefinedForAnyResource linkName "DELETE" collName allowHeader]
                        | Some delete -> delete ctx req resDef entity

                      fun next (httpCtx: HttpContext) ->
                        let method = httpCtx.Request.Method
                        handleErrors [methodNotAllowed method allowHeader] next httpCtx

                    ]


                  // Fallback
                  subRoutef "/%s" (fun path ->
                    handleErrors [linkOrRelationshipDoesNotExistForAnyResource path collName]
                  )

                ]

              )
          )


        ]))
    ]
