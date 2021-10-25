module ``GET resource polymorphic``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = {
  Id: string
  A: bool
}

type B = {
  Id: string
  B: int
}

type ABC = A of A | B of B | C of A

let a = A { Id = "a1"; A = true }
let b = B { Id = "b2"; B = 2 }
let c = C { Id = "c0"; A = false }


type MappedCtx = {
  GetById: string -> Result<ABC option, Error list>
  ModifyAResponse: A -> HttpHandler
  ModifyBResponse: B -> HttpHandler
}

type Ctx = {
  ParseId: string -> string option
  GetById: string -> Result<ABC option, Error list>
  ModifyAResponse: A -> HttpHandler
  ModifyBResponse: B -> HttpHandler
  MapCtx: Ctx -> Result<MappedCtx, Error list>
} with
  static member Default = {
    ParseId = Some
    GetById = (fun _ -> Ok (Some a))
    ModifyAResponse = fun _ -> fun next ctx -> next ctx
    ModifyBResponse = fun _ -> fun next ctx -> next ctx
    MapCtx = fun ctx -> Ok { GetById = ctx.GetById; ModifyAResponse = ctx.ModifyAResponse; ModifyBResponse = ctx.ModifyBResponse }
  }


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.ParsedOpt(id, (fun ctx s -> ctx.ParseId s), fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("abs")
  let a = define.Attribute.SimpleBool().Get(fun a -> a.A)
  let get =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .GetResource()
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyAResponse)


module B =

  let define = Define<Ctx, B, string>()
  let resId = define.Id.ParsedOpt(id, (fun ctx s -> ctx.ParseId s), fun (b: B) -> b.Id)
  let resDef = define.Resource("b", resId).CollectionName("abs")
  let b = define.Attribute.SimpleInt().Get(fun b -> b.B)
  let get =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .GetResource()
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyBResponse)


module C =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.ParsedOpt(id, (fun ctx s -> ctx.ParseId s), fun (a: A) -> a.Id)
  let resDef = define.Resource("c", resId).CollectionName("abs")


module AB =

  let define = Define<Ctx, ABC, string>()

  let resId = define.Id.ParsedOpt(id, (fun ctx s -> ctx.ParseId s), function A a -> a.Id | B b -> b.Id | C c -> c.Id)

  let resDef =
    define.PolymorphicResource(resId)
      .CollectionName("abs")

  let lookup =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .Polymorphic
      .LookupRes(
        (fun ctx id -> ctx.GetById id),
        function
          | A a -> A.resDef.PolymorphicFor a
          | B b -> B.resDef.PolymorphicFor b
          | C c -> C.resDef.PolymorphicFor c
      )


type Ctx2 = Ctx2

module AB2 =

  let define = Define<Ctx2, ABC, string>()
  let resId = define.Id.Simple(function A a -> a.Id | B b -> b.Id | C c -> c.Id)
  let resDef =
    define.PolymorphicResource(resId)
      .CollectionName("abs")
  let lookup = define.Operation.Lookup(fun _ -> None)



type Ctx3 = Ctx3

module AB3 =

  let define = Define<Ctx3, ABC, string>()
  let resId = define.Id.Simple(function A a -> a.Id | B b -> b.Id | C c -> c.Id)
  let resDef =
    define.PolymorphicResource(resId)
      .CollectionName("abs")


[<Tests>]
let tests =
  testList "GET resource polymorphic" [

    testJob "Returns 200 if successful" {
      let! response = Request.get Ctx.Default "/abs/a1" |> getResponse
      response |> testStatusCode 200
    }

    testJob "Insensitive to trailing slashes" {
      let! response = Request.get Ctx.Default "/abs/a1/" |> getResponse
      response |> testStatusCode 200
    }

    testJob "Correctly handles ETag and If-None-Match" {
      let! response = Request.get Ctx.Default "/abs/a1" |> getResponse
      response |> testStatusCode 200
      let eTag = response.headers.[ETag]

      let! response =
        Request.get Ctx.Default "/abs/a1"
        |> Request.setHeader (IfNoneMatch eTag)
        |> getResponse
      response |> testStatusCode 304
    }

    testJob "Returns correct data for A" {
      let! response = Request.get Ctx.Default "/abs/a1" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "a" @>
      test <@ json |> getPath "data.id" = "a1" @>
      test <@ json |> getPath "data.attributes.a" = true @>
      test <@ json |> getPath "data.links.self" = "http://example.com/abs/a1" @>
    }

    testJob "Returns correct data for B" {
      let ctx = { Ctx.Default with GetById = (fun _ -> Ok (Some b)) }
      let! response = Request.get ctx "/abs/b2" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "b" @>
      test <@ json |> getPath "data.id" = "b2" @>
      test <@ json |> getPath "data.attributes.b" = 2 @>
      test <@ json |> getPath "data.links.self" = "http://example.com/abs/b2" @>
    }

    testJob "Modifies response if successful for A" {
      let ctx = { Ctx.Default with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response = Request.get ctx "/abs/a1" |> getResponse
      response |> testSuccessStatusCode
      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>
    }

    testJob "Modifies response if successful for B" {
      let ctx = { Ctx.Default with GetById = (fun _ -> Ok (Some b)); ModifyBResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response = Request.get ctx "/abs/b2" |> getResponse
      response |> testSuccessStatusCode
      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>
    }

    testJob "Returns 404 if not found" {
      let ctx = { Ctx.Default with GetById = fun _ -> Ok None }
      let! response = Request.get ctx "/abs/a1" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not contain a resource with ID 'a1'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors returned by GetById" {
      let ctx = { Ctx.Default with GetById = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/abs/a1" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors returned by mapCtx" {
      let ctx = { Ctx.Default with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/abs/a1" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 if ID parsing fails" {
      let ctx = { Ctx.Default with ParseId = fun _ -> None }
      let! response = Request.get ctx "/abs/a1" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not contain a resource with ID 'a1'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported for resource" {
      let ctx = { Ctx.Default with GetById = fun _ -> Some c |> Ok }
      let! response = Request.get ctx "/abs/c0" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].detail" = "GET is not supported for resource type 'c' (it may be supported for other resource types in collection 'abs')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported at all" {
      let! response = Request.get Ctx2 "/abs/a1" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].detail" = "GET is not supported for any resource in collection 'abs'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if missing lookup" {
      let! response = Request.get Ctx3 "/abs/a1" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns error if collection case does not match" {
      let! response = Request.get Ctx.Default "/Abs/a1" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path '/Abs/a1' does not exist, but differs only by case from the existing path '/abs/a1'. Paths are case sensitive." @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
