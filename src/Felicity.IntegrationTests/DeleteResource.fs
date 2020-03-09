module ``DELETE resource``

open System
open Microsoft.Net.Http.Headers
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = {
  Id: string
}

type B = {
  Id: string
}

type ABC = A of A | B of B | C of A


type Db () =
  let mutable ABs : Map<string, ABC> =
    Map.empty
    |> Map.add "a1" (A { Id = "a1" })
    |> Map.add "b2" (B { Id = "b2" })
    |> Map.add "c0" (C { Id = "c0" })

  member _.Remove id =
    ABs <- ABs.Remove id

  member _.TryGet id =
    ABs.TryFind id


type MappedCtx = {
  ModifyAResponse: HttpHandler
  ModifyBResponse: HttpHandler
  BeforeDeleteA: A -> Result<A, Error list>
  DeleteA: A -> Result<unit, Error list>
  DeleteB: B -> unit
  Db: Db
}


type Ctx = {
  ModifyAResponse: HttpHandler
  ModifyBResponse: HttpHandler
  Db: Db
  BeforeDeleteA: A -> Result<A, Error list>
  DeleteA: A -> Result<unit, Error list>
  DeleteB: B -> unit
  MapCtx: Ctx -> Result<MappedCtx, Error list>
} with
  static member WithDb db = {
    ModifyAResponse = fun next ctx -> next ctx
    ModifyBResponse = fun next ctx -> next ctx
    Db = db
    BeforeDeleteA = fun a -> Ok a
    DeleteA = fun a -> db.Remove a.Id |> Ok
    DeleteB = fun b -> db.Remove b.Id
    MapCtx = fun ctx -> Ok {
      ModifyAResponse = ctx.ModifyAResponse
      ModifyBResponse = ctx.ModifyBResponse
      Db = ctx.Db
      BeforeDeleteA = ctx.BeforeDeleteA
      DeleteA = ctx.DeleteA
      DeleteB = ctx.DeleteB
    }
  }


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("abs")

  let get = define.Operation.GetResource()

  let delete =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .DeleteRes(fun (ctx: MappedCtx) a -> ctx.DeleteA a)
      .BeforeDeleteRes(fun (ctx: MappedCtx) a -> ctx.BeforeDeleteA a)
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyAResponse)



module B =

  let define = Define<Ctx, B, string>()
  let resId = define.Id.Simple(fun (b: B) -> b.Id)
  let resDef = define.Resource("b", resId).CollectionName("abs")

  let get = define.Operation.GetResource()

  let patch =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .Delete(fun (ctx: MappedCtx) b -> ctx.DeleteB b)
      .Return202Accepted()
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyBResponse)


module C =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("c", resId).CollectionName("abs")



module AB =

  let define = Define<Ctx, ABC, string>()

  let resId = define.Id.Simple(function A a -> a.Id | B b -> b.Id | C c -> c.Id)

  let resDef =
    define.PolymorphicResource(resId)
      .CollectionName("abs")

  let lookup =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .Polymorphic
      .Lookup(
        (fun (ctx: MappedCtx) id -> ctx.Db.TryGet id),
        function
          | A a -> A.resDef.PolymorphicFor a
          | B b -> B.resDef.PolymorphicFor b
          | C c -> C.resDef.PolymorphicFor c
      )


type Ctx2 = Ctx2


module A2 =

  let define = Define<Ctx2, A, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("a", resId).CollectionName("abs")
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "a1" })


type Ctx3 = Ctx3


module A3 =

  let define = Define<Ctx3, A, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("a", resId).CollectionName("abs")


type Ctx4 = Ctx4

module A4 =

  let define = Define<Ctx4, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "a1" })
  let get = define.Operation.GetResource()
  let delete = define.Operation.Delete(ignore)
  let preconditions = define.Preconditions.ETag(fun _ -> EntityTagHeaderValue.FromString false "valid-etag")


type Ctx5 = Ctx5

module A5 =

  let define = Define<Ctx5, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "a1" })
  let get = define.Operation.GetResource()
  let delete = define.Operation.Delete(ignore)
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))


type Ctx6 = Ctx6

module A6 =

  let define = Define<Ctx6, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "a1" })
  let get = define.Operation.GetResource()
  let delete = define.Operation.Delete(ignore)
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)).Optional



[<Tests>]
let tests =
  testList "DELETE resource" [

    testJob "Create A: Returns 204 and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = setHttpHeader "Foo" "Bar" }
      let! response = Request.delete ctx "/abs/a1" |> getResponse
      response |> testStatusCode 204
      let! json = response |> Response.readBodyAsString
      test <@ json = "" @>

      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>

      test <@ db.TryGet "a1" |> Option.isNone @>
    }

    testJob "Create B: Returns 202, and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyBResponse = setHttpHeader "Foo" "Bar" }
      let! response = Request.delete ctx "/abs/b2" |> getResponse
      response |> testStatusCode 202
      let! json = response |> Response.readBodyAsString
      test <@ json = "" @>

      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>

      test <@ db.TryGet "b2" |> Option.isNone @>
    }

    testJob "Correctly handles precondition validation using ETag" {
      let! response = Request.delete Ctx4 "/as/a1" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.delete Ctx4 "/as/a1"
        |> Request.setHeader (IfMatch "\"invalid-etag\"")
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey ETag = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Match header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.delete Ctx4 "/as/a1"
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> getResponse
      test <@ response.headers.ContainsKey ETag = false @>
      response |> testStatusCode 204
    }

    testJob "Correctly handles precondition validation using If-Unmodified-Since" {
      let! response = Request.delete Ctx5 "/as/a1" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.delete Ctx5 "/as/a1"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.delete Ctx5 "/as/a1"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 204
    }

    testJob "Correctly handles optional precondition validation" {
      let! response = Request.delete Ctx6 "/as/a1" |> getResponse
      response |> testStatusCode 204

      let! response =
        Request.delete Ctx6 "/as/a1"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.delete Ctx6 "/as/a1"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 204
    }

    testJob "Calls delete with entity returned by BeforeDelete" {
      let db = Db ()
      let mutable calledWith = None
      let ctx = {
        Ctx.WithDb db with
          BeforeDeleteA = fun a -> Ok { a with Id = "foobar" }
          DeleteA = fun a -> calledWith <- Some a; Ok ()
      }
      let! response = Request.delete ctx "/abs/a1" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some { Id = "foobar" } @>
    }

    testJob "Returns errors and does not call delete if BeforeDelete returns an error" {
      let db = Db ()
      let ctx = {
        Ctx.WithDb db with
          BeforeDeleteA = fun _ -> Error [Error.create 422 |> Error.setCode "custom"]
          DeleteA = fun _ -> failwith "Should not be called"
      }
      let! response = Request.delete ctx "/abs/a1" |> getResponse

      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      test <@ db.TryGet "a1" |> Option.isSome @>
    }

    testJob "Returns errors returned by delete" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with DeleteA = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.delete ctx "/abs/a1" |> getResponse

      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 when resource is not found" {
      let db = Db ()
      let! response = Request.delete (Ctx.WithDb db) "/abs/a2" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not contain a resource with ID 'a2'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors returned by mapCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.delete ctx "/abs/a1" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported by resource" {
      let db = Db()
      let! response = Request.delete (Ctx.WithDb db) "/abs/c0" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "DELETE is not supported for resource type 'c' (it may be supported for other resource types in collection 'abs')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported at all" {
      let! response = Request.delete Ctx2 "/abs/a2" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "DELETE is not supported for any resource in collection 'abs'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if missing lookup" {
      let! response = Request.delete Ctx3 "/abs/a1" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
