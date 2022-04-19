module Felicity.Routing

open System
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.DependencyInjection
open Hopac
open Giraffe
open Giraffe.EndpointRouting
open Errors


type internal JsonApiEndpoints<'ctx> = JsonApiEndpoints of Endpoint list


let verifyPathCase expectedPath : HttpHandler =
  fun next ctx ->
    task {
      let actualPath = ctx.Request.Path.Value
      let actualPath = if actualPath = "/" then "/" else actualPath.TrimEnd('/')
      let expectedPath = if expectedPath = "/" then "/" else expectedPath.TrimEnd('/')
      if actualPath = expectedPath then
        return! next ctx
      else
        return! handleErrors [incorrectPathCase actualPath expectedPath] next ctx
    }


let internal route1 (path: string) (paramName: string) (handler: string -> HttpHandler) : Endpoint =
  route path (
    fun next ctx ->
      task {
        let p = ctx.GetRouteValue(paramName) :?> string
        return! handler p next ctx
      }
  )


let internal route2 (path: string) (paramName1: string) (paramName2: string) (handler: string -> string -> HttpHandler) : Endpoint =
  route path (
    fun next ctx ->
      task {
        let p1 = ctx.GetRouteValue(paramName1) :?> string
        let p2 = ctx.GetRouteValue(paramName2) :?> string
        return! handler p1 p2 next ctx
      }
  )


let internal route3 (path: string) (paramName1: string) (paramName2: string) (paramName3: string) (handler: string -> string -> string -> HttpHandler) : Endpoint =
  route path (
    fun next ctx ->
      task {
        let p1 = ctx.GetRouteValue(paramName1) :?> string
        let p2 = ctx.GetRouteValue(paramName2) :?> string
        let p3 = ctx.GetRouteValue(paramName3) :?> string
        return! handler p1 p2 p3 next ctx
      }
  )


let internal jsonApiEndpoints relativeRootWithLeadingSlash (getCtx: HttpContext -> Job<Result<'ctx, Error list>>) collections : Endpoint list =

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
              Fieldsets =
                query
                |> Map.filter (fun k _ -> k.StartsWith("fields[", StringComparison.Ordinal) && k.EndsWith(']'))
                |> Map.toArray
                |> Array.map (fun (k, v) -> k.Substring(7, k.Length - 8), v.Split ',' |> Set.ofArray)
                |> Map.ofArray
              Includes =
                query
                |> Map.tryFind "include"
                |> Option.map (fun paths ->
                    paths.Split ','
                    |> Array.filter ((<>) "")
                    |> Array.map (fun path -> path.Split '.' |> Array.filter ((<>) ""))
                    |> Array.map Array.toList
                    |> Array.toList)
                |> Option.defaultValue []
            }
            return! handler ctx req next httpCtx
      }
      |> Job.startAsTask


  let validateRequest : HttpHandler =
    fun next httpCtx ->

      let errs =
        [
          if not <| RequestValidation.acceptsJsonApi httpCtx then invalidAccept ()
          if RequestValidation.hasNonJsonApiContent httpCtx then invalidContentType ()
          if RequestValidation.allJsonApiAcceptsHaveParams httpCtx then invalidAcceptParams ()
          if RequestValidation.jsonApiContentTypeHasParams httpCtx then invalidContentTypeParams ()

          match RequestValidation.getIllegalQueryStringParams httpCtx with
          | [] -> ()
          | names -> yield! names |> List.map illegalQueryParamName

          let linkCfg = httpCtx.RequestServices.GetRequiredService<LinkConfig<'ctx>>()
          yield! linkCfg.GetIllegalValueErrors(httpCtx)

        ]
      match errs with
      | [] -> next httpCtx
      | errs -> handleErrors errs next httpCtx


  let lockResourceForModification (ctx: 'ctx) req collName resId : HttpHandler =
    fun next httpCtx ->
      task {
        match ResourceModule.lockSpec<'ctx> collName with
        | None -> return! next httpCtx
        | Some (lockSpecs, totalTimeout) ->
            match! LockSpecification.lockAll httpCtx ctx req totalTimeout resId lockSpecs with
            | Error errs -> return! handleErrors errs next httpCtx
            | Ok locker ->
                use _ = locker
                return! next httpCtx
      }


  let verifyPartialPathCase (expectedPathPrefix: string) : HttpHandler =
    fun next ctx ->
      task {
        let actualPath = ctx.Request.Path.Value.TrimEnd('/')
        if actualPath.StartsWith(expectedPathPrefix, StringComparison.Ordinal) then
          return! next ctx
        else
          return! handleErrors [incorrectPartialPathCase expectedPathPrefix] next ctx
      }


  [
    for collName, ops in collections |> Map.toSeq do

      subRoute relativeRootWithLeadingSlash [
        subRoute ("/" + collName) [
          let expectedCollPath = relativeRootWithLeadingSlash + "/" + collName

          // Collection operations

          GET_HEAD [
            route "/" (
              match ops.getCollection with
              | None -> handleErrors [collGetNotAllowed collName]
              | Some getColl -> verifyPathCase expectedCollPath >=> validateRequest >=> getCtx getColl
          )]

          POST [
            route "/" (
              match ops.postCollection with
              | None -> handleErrors [collPostNotAllowed collName]
              | Some postColl ->
                  verifyPathCase expectedCollPath
                  >=> validateRequest
                  >=> getCtx (fun ctx req ->
                        lockResourceForModification ctx req collName None
                        >=> postColl ctx req
                  )
          )]

          route "/" (
            let allowHeader =
              [ if ops.getCollection.IsSome then "GET"; "HEAD"
                if ops.postCollection.IsSome then "POST" ]
              |> String.concat ", "

            verifyPathCase expectedCollPath
            >=> validateRequest
            >=> fun next (httpCtx: HttpContext) ->
                  handleErrors [methodNotAllowed httpCtx.Request.Method allowHeader] next httpCtx
          )


          // Specific resource operations

          match ops.resourceOperations.getByIdBoxedHandler with

          | None ->
              route "/{id}/{*restPath}" (
                verifyPartialPathCase expectedCollPath
                >=> validateRequest
                >=> handleErrors [collLookupNotSupported collName]
              )

          | Some getById ->

              GET_HEAD [route1 "/{id}" "id" (fun resId ->
                let expectedPath = expectedCollPath + "/" + resId
                match ops.resourceOperations.get with
                | None -> handleErrors [resGetNotSupportedForAnyResource collName]
                | Some get ->
                    verifyPathCase expectedPath
                    >=> validateRequest
                    >=> getCtx (fun ctx req -> getById ctx resId (get ctx req))
              )]

              PATCH [route1 "/{id}" "id" (fun resId ->
                let expectedPath = expectedCollPath + "/" + resId
                match ops.resourceOperations.patch with
                | None -> handleErrors [resPatchNotSupportedForAnyResource collName]
                | Some patch ->
                    verifyPathCase expectedPath
                    >=> validateRequest
                    >=> getCtx (fun ctx req ->
                          lockResourceForModification ctx req collName (Some resId)
                          >=> getById ctx resId (patch ctx req)
                    )
              )]

              DELETE [route1 "/{id}" "id" (fun resId ->
                let expectedPath = expectedCollPath + "/" + resId
                match ops.resourceOperations.delete with
                | None -> handleErrors [resDeleteNotSupportedForAnyResource collName]
                | Some delete ->
                    verifyPathCase expectedPath
                    >=> validateRequest
                    >=> getCtx (fun ctx req ->
                          lockResourceForModification ctx req collName (Some resId)
                          >=> getById ctx resId (delete ctx req)
                    )
              )]

              route1 "/{id}" "id" (fun resId ->
                let expectedPath = expectedCollPath + "/" + resId
                let allowHeader =
                  [ if ops.resourceOperations.get.IsSome then "GET"; "HEAD"
                    if ops.resourceOperations.patch.IsSome then "PATCH"
                    if ops.resourceOperations.delete.IsSome then "DELETE" ]
                  |> String.concat ", "

                verifyPathCase expectedPath
                >=> validateRequest
                >=> fun next (httpCtx: HttpContext) ->
                      handleErrors [methodNotAllowed httpCtx.Request.Method allowHeader] next httpCtx
              )


              // Resource relationship 'related' operations


              for relName, rel in ops.resourceOperations.relationships |> Map.toList do

                GET_HEAD [route1 ("/{id}/" + relName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/" + relName

                  match rel.getRelated with
                  | None -> handleErrors [getRelNotDefinedForAnyResource relName collName]
                  | Some get ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req -> getById ctx resId (get ctx req))
                )]

                route1 ("/{id}/" + relName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/" + relName
                  verifyPathCase expectedPath
                  >=> validateRequest
                  >=> fun next ctx ->
                        let allowHeader =
                          [ if rel.getRelated.IsSome then "GET"; "HEAD" ]
                          |> String.concat ", "

                        handleErrors [methodNotAllowed ctx.Request.Method allowHeader] next ctx
                )


              // Resource relationship 'self' operations


              for relName, rel in ops.resourceOperations.relationships |> Map.toList do

                GET_HEAD [route1 ("/{id}/relationships/" + relName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/relationships/" + relName

                  match rel.getSelf with
                  | None -> handleErrors [getRelNotDefinedForAnyResource relName collName]
                  | Some get ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req -> getById ctx resId (get ctx req))
                )]

                PATCH [route1 ("/{id}/relationships/" + relName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/relationships/" + relName

                  match rel.patchSelf with
                  | None -> handleErrors [patchRelSelfNotAllowedForAnyResource relName collName rel.postSelf.IsSome rel.deleteSelf.IsSome]
                  | Some patch ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req ->
                            lockResourceForModification ctx req collName (Some resId)
                            >=> getById ctx resId (patch ctx req)
                      )
                )]

                POST [route1 ("/{id}/relationships/" + relName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/relationships/" + relName

                  match rel.postSelf with
                  | None -> handleErrors [postToManyRelSelfNotAllowedForAnyResource relName collName rel.patchSelf.IsSome rel.deleteSelf.IsSome]
                  | Some post ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req ->
                            lockResourceForModification ctx req collName (Some resId)
                            >=> getById ctx resId (post ctx req)
                      )
                )]

                DELETE [route1 ("/{id}/relationships/" + relName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/relationships/" + relName

                  match rel.deleteSelf with
                  | None -> handleErrors [deleteToManyRelSelfNotAllowedForAnyResource relName collName rel.patchSelf.IsSome rel.postSelf.IsSome]
                  | Some delete ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req ->
                            lockResourceForModification ctx req collName (Some resId)
                            >=> getById ctx resId (delete ctx req)
                      )
                )]

                route1 ("/{id}/relationships/" + relName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/relationships/" + relName
                  verifyPathCase expectedPath
                  >=> validateRequest
                  >=> fun next ctx ->
                        let allowHeader =
                          [ if rel.getSelf.IsSome then "GET"; "HEAD"
                            if rel.patchSelf.IsSome then "PATCH"
                            if rel.postSelf.IsSome then "POST"
                            if rel.deleteSelf.IsSome then "DELETE" ]
                          |> String.concat ", "

                        handleErrors [methodNotAllowed ctx.Request.Method allowHeader] next ctx
                )

          
          
          
              // Resource link operations


              let getLinkAllowHeader (link: LinkOperations<'ctx>) =
                [ if link.get.IsSome then "GET"; "HEAD"
                  if link.post.IsSome then "POST"
                  if link.patch.IsSome then "PATCH"
                  if link.delete.IsSome then "DELETE" ]
                |> String.concat ", "

              for linkName, link in ops.resourceOperations.links |> Map.toList do

                GET_HEAD [route1 ("/{id}/" + linkName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/" + linkName

                  match link.get with
                  | None ->
                      fun next ctx ->
                        handleErrors [customOpVerbNotDefinedForAnyResource linkName ctx.Request.Method collName (getLinkAllowHeader link)] next ctx
                  | Some get ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req -> getById ctx resId (get ctx req))
                )]

                POST [route1 ("/{id}/" + linkName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/" + linkName

                  match link.post with
                  | None ->
                      fun next ctx ->
                        handleErrors [customOpVerbNotDefinedForAnyResource linkName ctx.Request.Method collName (getLinkAllowHeader link)] next ctx
                  | Some post ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req ->
                            lockResourceForModification ctx req collName (Some resId)
                            >=> getById ctx resId (post ctx req)
                      )
                )]

                PATCH [route1 ("/{id}/" + linkName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/" + linkName

                  match link.patch with
                  | None ->
                      fun next ctx ->
                        handleErrors [customOpVerbNotDefinedForAnyResource linkName ctx.Request.Method collName (getLinkAllowHeader link)] next ctx
                  | Some patch ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req ->
                            lockResourceForModification ctx req collName (Some resId)
                            >=> getById ctx resId (patch ctx req)
                      )
                )]

                DELETE [route1 ("/{id}/" + linkName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/" + linkName

                  match link.delete with
                  | None ->
                      fun next ctx ->
                        handleErrors [customOpVerbNotDefinedForAnyResource linkName ctx.Request.Method collName (getLinkAllowHeader link)] next ctx
                  | Some delete ->
                      verifyPathCase expectedPath
                      >=> validateRequest
                      >=> getCtx (fun ctx req ->
                            lockResourceForModification ctx req collName (Some resId)
                            >=> getById ctx resId (delete ctx req)
                      )
                )]

                route1 ("/{id}/" + linkName) "id" (fun resId ->
                  let expectedPath = expectedCollPath + "/" + resId + "/" + linkName
                  verifyPathCase expectedPath
                  >=> validateRequest
                  >=> fun next ctx ->
                        handleErrors [customOpVerbNotDefinedForAnyResource linkName ctx.Request.Method collName (getLinkAllowHeader link)] next ctx
                )


              // Fallbacks

              route2 "/{id}/relationships/{relName}" "id" "relName" (fun _ relName ->
                verifyPartialPathCase expectedCollPath
                >=> validateRequest
                >=> handleErrors [relationshipDoesNotExistForAnyResource relName collName]
              )

              route3 "/{id}/relationships/{relName}/{path}/{*restPath}" "id" "relName" "path" (fun _ linkOrRelName path ->
                let path = $"relationships/%s{linkOrRelName}/%s{path}"
                verifyPartialPathCase expectedCollPath
                >=> validateRequest
                >=> handleErrors [invalidPath path collName]
              )

              route1 "/{id}/relationships" "id" (fun _ ->
                verifyPartialPathCase expectedCollPath
                >=> validateRequest
                >=> handleErrors [invalidPath "relationships" collName]
              )

              route2 "/{id}/{linkOrRelName}" "id" "linkOrRelName" (fun _ linkOrRelName ->
                verifyPartialPathCase expectedCollPath
                >=> validateRequest
                >=> handleErrors [linkOrRelationshipDoesNotExistForAnyResource linkOrRelName collName]
              )

              route3 "/{id}/{linkOrRelName}/{path}/{*restPath}" "id" "linkOrRelName" "path" (fun _ linkOrRelName path ->
                let path = $"%s{linkOrRelName}/%s{path}"
                verifyPartialPathCase expectedCollPath
                >=> validateRequest
                >=> handleErrors [invalidPath path collName]
              )

        ]
      ]

  ]
