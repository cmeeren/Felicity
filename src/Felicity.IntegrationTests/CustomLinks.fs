module ``Custom links``

open System
open Microsoft.Net.Http.Headers
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = { Id: string }
type B = { Id: string }
type C = { Id: string }
type D = { Id: string }
type Entity = A of A | B of B | C of C | D of D


type Db () =
  let mutable entities : Map<string, Entity> =
    Map.empty
    |> Map.add "a1" (A { Id = "a1" })
    |> Map.add "b1" (B { Id = "b1" })
    |> Map.add "c1" (C { Id = "c1" })
    |> Map.add "d1" (D { Id = "d1" })

  member _.TryGet id =
    entities.TryFind id


type MappedCtx = {
  Db: Db
  DisableMeta: bool
  Condition: Result<unit, Error list>
  GetOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
  PostOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
  PatchOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
  DeleteOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
}


and Ctx = {
  Db: Db
  DisableMeta: bool
  Condition: Result<unit, Error list>
  GetOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
  PostOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
  PatchOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
  DeleteOperation: Responder<Ctx> -> Result<HttpHandler, Error list>
  MapCtx: Ctx -> Result<MappedCtx, Error list>
} with
  static member WithDb db = {
    Db = db
    DisableMeta = false
    Condition = Ok ()
    GetOperation = fun _ -> failwith "Operation must specified"
    PostOperation = fun _ -> failwith "Operation must specified"
    PatchOperation = fun _ -> failwith "Operation must specified"
    DeleteOperation = fun _ -> failwith "Operation must specified"
    MapCtx = fun ctx -> Ok {
      Db = ctx.Db
      DisableMeta = ctx.DisableMeta
      Condition = ctx.Condition
      GetOperation = ctx.GetOperation
      PostOperation = ctx.PostOperation
      PatchOperation = ctx.PatchOperation
      DeleteOperation = ctx.DeleteOperation
    }
  }


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("entities")

  let get = define.Operation.GetResource()

  let someRel =
    define.Relationship
      .ToOne(resDef)
      .Get(fun _ -> { A.Id = "test" })

  let customOp =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .CustomLink()
      .ConditionRes(fun ctx _ -> ctx.Condition)
      .Get(fun ctx parser responder _ -> ctx.GetOperation responder |> async.Return)
      .Post(fun ctx parser responder _ -> ctx.PostOperation responder |> async.Return)
      .Patch(fun ctx parser responder _ -> ctx.PatchOperation responder |> async.Return)
      .Delete(fun ctx parser responder _ -> ctx.DeleteOperation responder |> async.Return)
      .AddMeta("someValue", (fun _ _ -> 2), fun ctx _ -> not ctx.DisableMeta)



module B =

  let define = Define<Ctx, B, string>()
  let resId = define.Id.Simple(fun (b: B) -> b.Id)
  let resDef = define.Resource("b", resId).CollectionName("entities")
  let get = define.Operation.GetResource()

  let x = define.Attribute.Simple().Get(fun _ -> 2)

  let customOp =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .CustomLink()
      .Get(fun _ _ _ _ -> failwith "not used")
      .Post(fun _ _ _ _ -> failwith "not used")



module C =

  let define = Define<Ctx, C, string>()
  let resId = define.Id.Simple(fun (c: C) -> c.Id)
  let resDef = define.Resource("c", resId).CollectionName("entities")
  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .CustomLink()
      .Patch(fun _ _ _ _ -> failwith "not used")
      .Delete(fun _ _ _ _ -> failwith "not used")


module D =

  let define = Define<Ctx, D, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("d", resId).CollectionName("entities")



module Entity =

  let define = Define<Ctx, Entity, string>()

  let resId = define.Id.Simple(function A a -> a.Id | B b -> b.Id | C c -> c.Id | D d -> d.Id)

  let resDef =
    define.PolymorphicResource(resId)
      .CollectionName("entities")

  let lookup =
    define.Operation
      .Polymorphic
      .Lookup(
        (fun (ctx: Ctx) id -> ctx.Db.TryGet id),
        function
          | A a -> A.resDef.PolymorphicFor a
          | B b -> B.resDef.PolymorphicFor b
          | C c -> C.resDef.PolymorphicFor c
          | D d -> D.resDef.PolymorphicFor d
      )


type Ctx2 = Ctx2


module A2 =

  let define = Define<Ctx2, A, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("a", resId).CollectionName("entities")
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "foo" })
  let get = define.Operation.GetResource()

  let customOpWithGetAndPost =
    define.Operation
      .CustomLink()
      .Get(fun _ _ _ _ -> failwith "not used")
      .Post(fun _ _ _ _ -> failwith "not used")

  let customOpWithPatchAndDelete =
    define.Operation
      .CustomLink()
      .Patch(fun _ _ _ _ -> failwith "not used")
      .Delete(fun _ _ _ _ -> failwith "not used")


type Ctx3 = Ctx3


module A3 =

  let define = Define<Ctx3, A, string>()
  let resId = define.Id.Simple(fun x -> x.Id)
  let resDef = define.Resource("a", resId).CollectionName("entities")
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "a1" })


type Ctx4 = Ctx4


module A4 =

  let define = Define<Ctx4, A, string>()
  let resId = define.Id.Simple(fun x -> x.Id)
  let resDef = define.Resource("a", resId).CollectionName("entities")


type Ctx5 = Ctx5


module A5 =

  let define = Define<Ctx5, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("entities")
  let preconditions = define.Preconditions.ETag(fun _ -> EntityTagHeaderValue.FromString false "valid-etag")
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" } )

  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .CustomLink()
      .Post(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
      .Patch(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
      .Delete(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx6 = Ctx6

module A6 =

  let define = Define<Ctx6, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("entities")
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" } )

  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .CustomLink()
      .Post(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
      .Patch(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
      .Delete(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx7 = Ctx7

module A7 =

  let define = Define<Ctx7, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("entities")
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)).Optional
  let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" } )

  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .CustomLink()
      .Post(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
      .Patch(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
      .Delete(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


[<Tests>]
let tests =
  testList "Custom links" [

    testJob "Link has correct name and URL" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/entities/a1" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "a" @>
      test <@ json |> getPath "data.id" = "a1" @>
      test <@ json |> getPath "data.links.customOp.href" = "http://example.com/entities/a1/customOp" @>
      test <@ json |> getPath "data.links.customOp.meta.someValue" = 2 @>
    }

    testJob "No link if condition is false and no meta" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with Condition = Error []; DisableMeta = true }
      let! response = Request.get ctx "/entities/a1" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> hasNoPath "data.links.customOp" @>
    }

    testJob "No link if mapCtx fails" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [] }
      let! response = Request.get ctx "/entities/a1" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> hasNoPath "data.links.customOp" @>
    }

    testJob "Has link with null href if condition is false and has meta and mapCtx succeeds" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with Condition = Error [] }
      let! response = Request.get ctx "/entities/a1" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.links.customOp.href" = null @>
      test <@ json |> getPath "data.links.customOp.meta.someValue" = 2 @>
    }

    testJob "GET returns correct response" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with GetOperation = fun respond -> Ok (setStatusCode 201 >=> respond.WithEntity(A.resDef, { Id = "foo" }) ) }
      let! response = Request.get ctx "/entities/a1/customOp?include=someRel" |> getResponse
      response |> testStatusCode 201
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "a" @>
      test <@ json |> getPath "data.id" = "foo" @>
      test <@ json |> getPath "data.relationships.someRel.data.type" = "a" @>
      test <@ json |> getPath "data.relationships.someRel.data.id" = "test" @>
      test <@ json |> getPath "data.links.customOp.href" = "http://example.com/entities/foo/customOp" @>
      test <@ json |> getPath "data.links.customOp.meta.someValue" = 2 @>
      test <@ json |> getPath "included[0].type" = "a" @>
      test <@ json |> getPath "included[0].id" = "test" @>
      test <@ json |> getPath "included[0].links.self" = "http://example.com/entities/test" @>
      test <@ json |> getPath "included[0].links.customOp.href" = "http://example.com/entities/test/customOp" @>
      test <@ json |> getPath "included[0].links.customOp.meta.someValue" = 2 @>
    }

    testJob "POST returns correct response 1" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with PostOperation = fun respond -> Ok (setStatusCode 201 >=> respond.WithOptEntity(A.resDef, Some { Id = "foo" }) ) }
      let! response = Request.post ctx "/entities/a1/customOp?include=someRel" |> getResponse
      response |> testStatusCode 201
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "a" @>
      test <@ json |> getPath "data.id" = "foo" @>
      test <@ json |> getPath "data.relationships.someRel.data.type" = "a" @>
      test <@ json |> getPath "data.relationships.someRel.data.id" = "test" @>
      test <@ json |> getPath "data.links.customOp.href" = "http://example.com/entities/foo/customOp" @>
      test <@ json |> getPath "data.links.customOp.meta.someValue" = 2 @>
      test <@ json |> getPath "included[0].type" = "a" @>
      test <@ json |> getPath "included[0].id" = "test" @>
      test <@ json |> getPath "included[0].links.self" = "http://example.com/entities/test" @>
      test <@ json |> getPath "included[0].links.customOp.href" = "http://example.com/entities/test/customOp" @>
      test <@ json |> getPath "included[0].links.customOp.meta.someValue" = 2 @>
    }

    testJob "POST returns correct response 2" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with PostOperation = fun respond -> Ok (setStatusCode 201 >=> respond.WithOptEntity(A.resDef, None) ) }
      let! response = Request.post ctx "/entities/a1/customOp?include=someRel" |> getResponse
      response |> testStatusCode 201
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data" = null @>
    }

    testJob "PATCH returns correct response" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with PatchOperation = fun respond -> Ok (setStatusCode 201 >=> respond.WithEntities(B.resDef, [{ Id = "foo" }; { Id = "bar" }]) ) }
      let! response = Request.patch ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 201
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "b" @>
      test <@ json |> getPath "data[0].id" = "foo" @>
      test <@ json |> getPath "data[0].attributes.x" = 2 @>
      test <@ json |> getPath "data[1].type" = "b" @>
      test <@ json |> getPath "data[1].id" = "bar" @>
      test <@ json |> getPath "data[1].attributes.x" = 2 @>
    }

    testJob "DELETE returns correct response" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with DeleteOperation = fun respond -> Ok (setStatusCode 201 >=> respond.WithPolymorphicEntities([A.resDef.PolymorphicFor { Id = "foo" }; B.resDef.PolymorphicFor { Id = "bar" }]) ) }
      let! response = Request.delete ctx "/entities/a1/customOp?include=someRel" |> getResponse
      response |> testStatusCode 201
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "a" @>
      test <@ json |> getPath "data[0].id" = "foo" @>
      test <@ json |> getPath "data[0].relationships.someRel.data.type" = "a" @>
      test <@ json |> getPath "data[0].relationships.someRel.data.id" = "test" @>
      test <@ json |> getPath "data[0].links.customOp.href" = "http://example.com/entities/foo/customOp" @>
      test <@ json |> getPath "data[0].links.customOp.meta.someValue" = 2 @>
      test <@ json |> getPath "data[1].type" = "b" @>
      test <@ json |> getPath "data[1].id" = "bar" @>
      test <@ json |> getPath "data[1].attributes.x" = 2 @>
      test <@ json |> getPath "included[0].type" = "a" @>
      test <@ json |> getPath "included[0].id" = "test" @>
      test <@ json |> getPath "included[0].links.self" = "http://example.com/entities/test" @>
      test <@ json |> getPath "included[0].links.customOp.href" = "http://example.com/entities/test/customOp" @>
      test <@ json |> getPath "included[0].links.customOp.meta.someValue" = 2 @>
    }

    testJob "Simple link when no meta" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with DisableMeta = true; GetOperation = fun respond -> Ok (setStatusCode 201 >=> respond.WithEntity(A.resDef, { Id = "foo" }) ) }
      let! response = Request.get ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 201
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.links.customOp" = "http://example.com/entities/foo/customOp" @>
    }

    testJob "POST correctly handles precondition validation using ETag" {
      let! response = Request.post Ctx5 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx5 "/entities/a1/customOp"
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
        Request.post Ctx5 "/entities/a1/customOp"
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> getResponse
      test <@ response.headers.ContainsKey ETag = false @>
      response |> testStatusCode 200
    }

    testJob "POST correctly handles precondition validation using If-Unmodified-Since" {
      let! response = Request.post Ctx6 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx6 "/entities/a1/customOp"
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
        Request.post Ctx6 "/entities/a1/customOp"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "POST correctly handles optional precondition validation" {
      let! response = Request.post Ctx7 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 200

      let! response =
        Request.post Ctx7 "/entities/a1/customOp"
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
        Request.post Ctx7 "/entities/a1/customOp"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "PATCH correctly handles precondition validation using ETag" {
      let! response = Request.patch Ctx5 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx5 "/entities/a1/customOp"
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
        Request.patch Ctx5 "/entities/a1/customOp"
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> getResponse
      test <@ response.headers.ContainsKey ETag = false @>
      response |> testStatusCode 200
    }

    testJob "PATCH correctly handles precondition validation using If-Unmodified-Since" {
      let! response = Request.patch Ctx6 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx6 "/entities/a1/customOp"
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
        Request.patch Ctx6 "/entities/a1/customOp"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "PATCH correctly handles optional precondition validation" {
      let! response = Request.patch Ctx7 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 200

      let! response =
        Request.patch Ctx7 "/entities/a1/customOp"
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
        Request.patch Ctx7 "/entities/a1/customOp"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "DELETE correctly handles precondition validation using ETag" {
      let! response = Request.delete Ctx5 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.delete Ctx5 "/entities/a1/customOp"
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
        Request.delete Ctx5 "/entities/a1/customOp"
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> getResponse
      test <@ response.headers.ContainsKey ETag = false @>
      response |> testStatusCode 200
    }

    testJob "DELETE correctly handles precondition validation using If-Unmodified-Since" {
      let! response = Request.delete Ctx6 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.delete Ctx6 "/entities/a1/customOp"
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
        Request.delete Ctx6 "/entities/a1/customOp"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "DELETE correctly handles optional precondition validation" {
      let! response = Request.delete Ctx7 "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 200

      let! response =
        Request.delete Ctx7 "/entities/a1/customOp"
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
        Request.delete Ctx7 "/entities/a1/customOp"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "GET returns 404 when resource is not found" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns 404 when resource is not found" {
      let db = Db ()
      let! response = Request.post (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns 404 when resource is not found" {
      let db = Db ()
      let! response = Request.patch (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns 404 when resource is not found" {
      let db = Db ()
      let! response = Request.delete (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns errors returned by mapCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns errors returned by mapCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.post ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns errors returned by mapCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.patch ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns errors returned by mapCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.delete ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns errors returned by condition" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with Condition = Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns errors returned by condition" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with Condition = Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.post ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns errors returned by condition" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with Condition = Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.patch ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns errors returned by condition" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with Condition = Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.delete ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns errors returned by operation" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with GetOperation = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.get ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns errors returned by operation" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with PostOperation = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.post ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns errors returned by operation" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with PatchOperation = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.patch ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns errors returned by operation" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with DeleteOperation = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response = Request.delete ctx "/entities/a1/customOp" |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns 405 if verb not supported for resource" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/entities/c1/customOp" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'c' does not support GET (other resource types in collection 'entities' may have an operation 'customOp' supporting GET)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "PATCH, DELETE" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns 405 if verb not supported for resource" {
      let db = Db ()
      let! response = Request.post (Ctx.WithDb db) "/entities/c1/customOp" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'c' does not support POST (other resource types in collection 'entities' may have an operation 'customOp' supporting POST)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "PATCH, DELETE" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns 405 if verb not supported for resource" {
      let db = Db ()
      let! response = Request.patch (Ctx.WithDb db) "/entities/b1/customOp" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'b' does not support PATCH (other resource types in collection 'entities' may have an operation 'customOp' supporting PATCH)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "GET, HEAD, POST" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns 405 if verb not supported for resource" {
      let db = Db ()
      let! response = Request.delete (Ctx.WithDb db) "/entities/b1/customOp" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'b' does not support DELETE (other resource types in collection 'entities' may have an operation 'customOp' supporting DELETE)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "GET, HEAD, POST" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns 405 if verb not supported for any resource" {
      let! response = Request.get Ctx2 "/entities/idIgnored/customOpWithPatchAndDelete" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOpWithPatchAndDelete' does not support GET for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "PATCH, DELETE" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns 405 if verb not supported for any resource" {
      let! response = Request.post Ctx2 "/entities/idIgnored/customOpWithPatchAndDelete" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOpWithPatchAndDelete' does not support POST for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "PATCH, DELETE" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns 405 if verb not supported for any resource" {
      let! response = Request.patch Ctx2 "/entities/idIgnored/customOpWithGetAndPost" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOpWithGetAndPost' does not support PATCH for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "GET, HEAD, POST" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns 405 if verb not supported for any resource" {
      let! response = Request.delete Ctx2 "/entities/idIgnored/customOpWithGetAndPost" |> getResponse
      response |> testStatusCode 405
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "405" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOpWithGetAndPost' does not support DELETE for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ response.headers.[Allow] = "GET, HEAD, POST" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns 404 if link not defined for resource" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns 404 if link not defined for resource" {
      let db = Db ()
      let! response = Request.post (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns 404 if link not defined for resource" {
      let db = Db ()
      let! response = Request.patch (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns 404 if link not defined for resource" {
      let db = Db ()
      let! response = Request.delete (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns 404 if link not defined for any resource" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/entities/d1/invalidOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Link or relationship 'invalidOp' does not exist for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns 404 if link not defined for any resource" {
      let db = Db ()
      let! response = Request.post (Ctx.WithDb db) "/entities/d1/invalidOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Link or relationship 'invalidOp' does not exist for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns 404 if link not defined for any resource" {
      let db = Db ()
      let! response = Request.patch (Ctx.WithDb db) "/entities/d1/invalidOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Link or relationship 'invalidOp' does not exist for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns 404 if link not defined for any resource" {
      let db = Db ()
      let! response = Request.delete (Ctx.WithDb db) "/entities/d1/invalidOp" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Link or relationship 'invalidOp' does not exist for any resource in collection 'entities'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "GET returns 403 if missing lookup" {
      let! response = Request.get Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST returns 403 if missing lookup" {
      let! response = Request.post Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "PATCH returns 403 if missing lookup" {
      let! response = Request.patch Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "DELETE returns 403 if missing lookup" {
      let! response = Request.delete Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
