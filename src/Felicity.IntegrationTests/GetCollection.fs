﻿module ``GET collection``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = {
  Id: string
  A: bool
}

let a1 = {
  Id = "1"
  A = true
}

let a2 = {
  Id = "2"
  A = false
}

type MappedCtx = {
  ModifyResponse: A list -> HttpHandler
  GetColl: unit -> Result<A list, Error list>
}

type Ctx = {
  ModifyResponse: A list -> HttpHandler
  GetColl: unit -> Result<A list, Error list>
  MapCtx: Ctx -> Result<MappedCtx, Error list>
} with
  static member Default = {
    ModifyResponse = fun _ -> fun next ctx -> next ctx
    GetColl = fun ()  -> Ok [a1; a2]
    MapCtx = fun ctx -> Ok { ModifyResponse = ctx.ModifyResponse; GetColl = ctx.GetColl }
  }


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let a = define.Attribute.SimpleBool().Get(fun a -> a.A)
  let getColl =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .GetCollectionRes(fun (ctx: MappedCtx) -> ctx.GetColl ())
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyResponse)



type Ctx2 = Ctx2


module A2 =

  let define = Define<Ctx2, A, string>()
  let resId = define.Id.Simple(fun _ -> failwith "Not used")
  let resDef = define.Resource("a", resId).CollectionName("as")


type Ctx3 = Ctx3


module A3 =

  let define = Define<Ctx3, A, string>()
  let resId = define.Id.Simple(fun _ -> "ignored")
  let resDef = define.Resource("a", resId).CollectionName("as")
  let getColl =
    define.Operation
      .GetCollection(fun ctx parser ->
        parser.For(id, Sort.Enum([A.a.Name, "a"])).Map(fun _ -> [a1])
      )


[<Tests>]
let tests =
  testList "GET collection" [

    testJob "Returns 200 if successful" {
      let! response = Request.get Ctx.Default "/as" |> getResponse
      response |> testStatusCode 200
    }

    testJob "Correctly handles ETag and If-None-Match" {
      let! response = Request.get Ctx.Default "/as" |> getResponse
      response |> testStatusCode 200
      let eTag = response.headers.[ETag]

      let! response =
        Request.get Ctx.Default "/as"
        |> Request.setHeader (IfNoneMatch eTag)
        |> getResponse
      response |> testStatusCode 304
    }

    testJob "Returns correct data" {
      let! response = Request.get Ctx.Default "/as" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "a" @>
      test <@ json |> getPath "data[0].id" = "1" @>
      test <@ json |> getPath "data[0].attributes.a" = true @>
      test <@ json |> getPath "data[1].type" = "a" @>
      test <@ json |> getPath "data[1].id" = "2" @>
      test <@ json |> getPath "data[1].attributes.a" = false @>
    }

    testJob "Modifies response if successful" {
      let ctx = {
        Ctx.Default with
          ModifyResponse = fun _ -> setHttpHeader "Foo" "Bar"
      }
      let! response = Request.get ctx "/as" |> getResponse
      response |> testSuccessStatusCode
      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>
    }

    testJob "Returns errors returned by GetColl" {
      let ctx = { Ctx.Default with GetColl = fun () -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/as" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors returned by mapCtx" {
      let ctx = { Ctx.Default with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/as" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported" {
      let! response = Request.get Ctx2 "/as" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'as' does not support fetching resources" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 if sort not supported" {
      let! response = Request.get Ctx.Default "/as?sort=a" |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "This operation does not support sorting" @>
      test <@ json |> getPath "errors[0].source.parameter" = "sort" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 200 if sort supported" {
      let! response = Request.get Ctx3 "/as?sort=a" |> getResponse
      response |> testStatusCode 200
    }

  ]
