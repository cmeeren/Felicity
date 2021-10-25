module ``GET resource``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = {
  Id: string
  A: bool
}

let a = {
  Id = "1"
  A = true
}

type MappedCtx = {
  GetById: string -> Result<A option, Error list>
  ModifyResponse: A -> HttpHandler
}

type Ctx = {
  ParseId: string -> string option
  GetById: string -> Result<A option, Error list>
  ModifyResponse: A -> HttpHandler
  MapCtx: Ctx -> Result<MappedCtx, Error list>
} with
  static member Default = {
    ParseId = Some
    GetById = (fun _ -> Ok (Some a))
    ModifyResponse = fun _ -> fun next ctx -> next ctx
    MapCtx = fun ctx -> Ok { GetById = ctx.GetById; ModifyResponse = ctx.ModifyResponse }
  }


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.ParsedOpt(id, (fun ctx s -> ctx.ParseId s), fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .LookupRes(fun ctx id -> ctx.GetById id)
  let get =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .GetResource()
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyResponse)



type Ctx2 = Ctx2

module A2 =

  let define = Define<Ctx2, A, string>()
  let resId = define.Id.Simple(fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> None)



type Ctx3 = Ctx3

module A3 =

  let define = Define<Ctx3, A, string>()
  let resId = define.Id.Simple(fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")


[<Tests>]
let tests =
  testList "GET resource" [

    testJob "Returns 200 if successful" {
      let! response = Request.get Ctx.Default "/as/1" |> getResponse
      response |> testStatusCode 200
    }

    testJob "Insensitive to trailing slashes" {
      let! response = Request.get Ctx.Default "/as/1/" |> getResponse
      response |> testStatusCode 200
    }

    testJob "Correctly handles ETag and If-None-Match" {
      let! response = Request.get Ctx.Default "/as/1" |> getResponse
      response |> testStatusCode 200
      let eTag = response.headers.[ETag]

      let! response =
        Request.get Ctx.Default "/as/1"
        |> Request.setHeader (IfNoneMatch eTag)
        |> getResponse
      response |> testStatusCode 304
    }

    testJob "Modifies response if successful" {
      let ctx = {
        Ctx.Default with
          ModifyResponse = fun _ -> setHttpHeader "Foo" "Bar"
      }
      let! response = Request.get ctx "/as/1" |> getResponse
      response |> testSuccessStatusCode
      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>
    }

    testJob "Returns 404 if not found" {
      let ctx = { Ctx.Default with GetById = fun _ -> Ok None }
      let! response = Request.get ctx "/as/1" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'as' does not contain a resource with ID '1'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors returned by GetById" {
      let ctx = { Ctx.Default with GetById = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/as/1" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors returned by mapCtx" {
      let ctx = { Ctx.Default with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/as/1" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 if ID parsing fails" {
      let ctx = { Ctx.Default with ParseId = fun _ -> None }
      let! response = Request.get ctx "/as/1" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'as' does not contain a resource with ID '1'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported" {
      let! response = Request.get Ctx2 "/as/nonExistentId" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].detail" = "GET is not supported for any resource in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if missing lookup" {
      let! response = Request.get Ctx3 "/as/nonExistentId" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'as' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns error if collection case does not match" {
      let! response = Request.get Ctx.Default "/As/1" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path '/As/1' does not exist, but differs only by case from the existing path '/as/1'. Paths are case sensitive." @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
