﻿module ``Custom links``

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
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

type Entity =
    | A of A
    | B of B
    | C of C
    | D of D


type Db() =
    let mutable entities: Map<string, Entity> =
        Map.empty
        |> Map.add "a1" (A { Id = "a1" })
        |> Map.add "b1" (B { Id = "b1" })
        |> Map.add "c1" (C { Id = "c1" })
        |> Map.add "d1" (D { Id = "d1" })

    member _.TryGet id = entities.TryFind id


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
    MapCtxWithEntity: Ctx -> B -> Result<MappedCtx, Error list>
} with

    static member WithDb db = {
        Db = db
        DisableMeta = false
        Condition = Ok()
        GetOperation = fun _ -> failwith "Operation must specified"
        PostOperation = fun _ -> failwith "Operation must specified"
        PatchOperation = fun _ -> failwith "Operation must specified"
        DeleteOperation = fun _ -> failwith "Operation must specified"
        MapCtx =
            fun ctx ->
                Ok {
                    Db = ctx.Db
                    DisableMeta = ctx.DisableMeta
                    Condition = ctx.Condition
                    GetOperation = ctx.GetOperation
                    PostOperation = ctx.PostOperation
                    PatchOperation = ctx.PatchOperation
                    DeleteOperation = ctx.DeleteOperation
                }
        MapCtxWithEntity =
            fun ctx _ ->
                Ok {
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

    let someRel = define.Relationship.ToOne(resDef).Get(fun _ -> { A.Id = "test" })

    let customOp =
        define.Operation
            .ForContextRes(fun ctx -> ctx.MapCtx ctx)
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .ConditionRes(fun ctx _ -> ctx.Condition)
            .GetAsync(fun ctx parser responder _ -> ctx.GetOperation responder |> async.Return)
            .PostAsync(fun ctx parser responder _ -> ctx.PostOperation responder |> async.Return)
            .PatchAsync(fun ctx parser responder _ -> ctx.PatchOperation responder |> async.Return)
            .DeleteAsync(fun ctx parser responder _ -> ctx.DeleteOperation responder |> async.Return)
            .AddMeta("someValue", (fun _ _ -> 2), (fun ctx _ -> not ctx.DisableMeta))



module B =

    let define = Define<Ctx, B, string>()
    let resId = define.Id.Simple(fun (b: B) -> b.Id)
    let resDef = define.Resource("b", resId).CollectionName("entities")
    let get = define.Operation.GetResource()

    let x = define.Attribute.SimpleInt().Get(fun _ -> 2)

    let customOp =
        define.Operation
            .ForContextRes(fun ctx e -> ctx.MapCtxWithEntity ctx e)
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .GetAsync(fun _ _ _ _ -> failwith "not used")
            .PostAsync(fun _ _ _ _ -> failwith "not used")



module C =

    let define = Define<Ctx, C, string>()
    let resId = define.Id.Simple(fun (c: C) -> c.Id)
    let resDef = define.Resource("c", resId).CollectionName("entities")
    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .ForContextRes(fun ctx -> ctx.MapCtx ctx)
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PatchAsync(fun _ _ _ _ -> failwith "not used")
            .DeleteAsync(fun _ _ _ _ -> failwith "not used")


module D =

    let define = Define<Ctx, D, string>()
    let resId = define.Id.Simple(fun _ -> failwith "not used")
    let resDef = define.Resource("d", resId).CollectionName("entities")



module Entity =

    let define = Define<Ctx, Entity, string>()

    let resId =
        define.Id.Simple(
            function
            | A a -> a.Id
            | B b -> b.Id
            | C c -> c.Id
            | D d -> d.Id
        )

    let resDef = define.PolymorphicResource(resId).CollectionName("entities")

    let lookup =
        define.Operation.Polymorphic.Lookup(
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
            .ValidateStrictModeQueryParams()
            .GetAsync(fun _ _ _ _ -> failwith "not used")
            .PostAsync(fun _ _ _ _ -> failwith "not used")

    let customOpWithPatchAndDelete =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PatchAsync(fun _ _ _ _ -> failwith "not used")
            .DeleteAsync(fun _ _ _ _ -> failwith "not used")


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

    let preconditions =
        define.Preconditions.ETag(fun _ -> EntityTagHeaderValue.FromString false "valid-etag")

    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PatchAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .DeleteAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx6 = Ctx6

module A6 =

    let define = Define<Ctx6, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("entities")

    let preconditions =
        define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))

    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PatchAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .DeleteAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx7 = Ctx7

module A7 =

    let define = Define<Ctx7, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("entities")

    let preconditions =
        define.Preconditions
            .LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))
            .Optional

    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PatchAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .DeleteAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx8 = Ctx8

module A8 =

    let define = Define<Ctx8, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("entities")

    let preconditions =
        define.Preconditions
            .LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))
            .Optional

    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .SkipStandardAcceptValidation()
            .GetAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PostAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PatchAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .DeleteAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx9 = Ctx9

module A9 =

    let define = Define<Ctx9, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("entities")

    let preconditions =
        define.Preconditions
            .LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))
            .Optional

    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .SkipStandardContentTypeValidation()
            .GetAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PostAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PatchAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .DeleteAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx10 = Ctx10

module A10 =

    let define = Define<Ctx10, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("entities")

    let preconditions =
        define.Preconditions
            .LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))
            .Optional

    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams("invalid")
            .SkipStandardQueryParamNameValidation()
            .GetAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PostAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .PatchAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)
            .DeleteAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type Ctx11 = Ctx11

module A11 =

    let define = Define<Ctx11, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("entities")
    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .SkipLink()
            .GetAsync(fun ctx parser responder _ -> setStatusCode 200 |> Ok |> async.Return)


type MetaCtx = {
    StatusCode: int option
    mutable Meta: Map<string, obj>
}


let getClientForMeta ctx =
    let server =
        new TestServer(
            WebHostBuilder()
                .ConfigureServices(fun services ->
                    services
                        .AddGiraffe()
                        .AddRouting()
                        .AddJsonApi()
                        .GetCtx(fun _ -> ctx)
                        .GetMeta(fun ctx -> ctx.Meta)
                        .EnableUnknownFieldStrictMode()
                        .EnableUnknownQueryParamStrictMode()
                        .Add()
                    |> ignore
                )
                .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<MetaCtx>() |> ignore)
        )

    server.CreateClient()

module A12 =

    let define = Define<MetaCtx, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("as")
    let lookup = define.Operation.Lookup(fun _ -> Some { A.Id = "someId" })

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .SkipLink()
            .GetAsync(fun (ctx: MetaCtx) parser respond _ ->
                async {
                    let handler =
                        match ctx.StatusCode with
                        | None -> respond.WithNoEntity()
                        | Some statusCode -> setStatusCode statusCode >=> respond.WithNoEntity()

                    return Ok handler
                }
            )


type HttpCtx = HttpCtx of HttpContext


module A13 =

    let define = Define<HttpCtx, unit, string>()
    let resId = define.Id.Simple(fun () -> "someId")
    let resDef = define.Resource("a", resId).CollectionName("as")

    let lookup = define.Operation.Lookup(fun _ -> Some())

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .SkipLink()
            .PostAsync(fun (HttpCtx ctx) parser respond _ ->
                async {
                    let! content = ctx.ReadBodyFromRequestAsync() |> Async.AwaitTask

                    if content = "foobar" then
                        return Ok(respond.WithEntity(resDef, ()))
                    else
                        return Ok(setStatusCode 400)
                }
            )


[<Tests>]
let tests =
    testList "Custom links" [

        testJob "Link has correct name and URL" {
            let db = Db()
            let! response = Request.get (Ctx.WithDb db) "/entities/a1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "a" @>
            test <@ json |> getPath "data.id" = "a1" @>
            test <@ json |> getPath "data.links.customOp.href" = "http://example.com/entities/a1/customOp" @>
            test <@ json |> getPath "data.links.customOp.meta.someValue" = 2 @>
        }

        testJob "No link if condition is false and no meta" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    Condition = Error []
                    DisableMeta = true
            }

            let! response = Request.get ctx "/entities/a1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasNoPath "data.links.customOp" @>
        }

        testJob "No link if mapCtx fails" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    MapCtx = fun _ -> Error []
            }

            let! response = Request.get ctx "/entities/a1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasNoPath "data.links.customOp" @>
        }

        testJob "No link if mapCtx with entity fails" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    MapCtxWithEntity = fun _ _ -> Error []
            }

            let! response = Request.get ctx "/entities/b1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasNoPath "data.links.customOp" @>
        }

        testJob "Has link with null href if condition is false and has meta and mapCtx succeeds" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    Condition = Error []
            }

            let! response = Request.get ctx "/entities/a1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.links.customOp.href" = null @>
            test <@ json |> getPath "data.links.customOp.meta.someValue" = 2 @>
        }

        testJob "GET returns correct response" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    GetOperation = fun respond -> Ok(setStatusCode 201 >=> respond.WithEntity(A.resDef, { Id = "foo" }))
            }

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

        testJob "GET insensitive to trailing slashes" {
            let ctx = {
                Ctx.WithDb(Db()) with
                    GetOperation = fun _ -> Ok(setStatusCode 201)
            }

            let! response = Request.get ctx "/entities/a1/customOp/" |> getResponse
            response |> testStatusCode 201
        }

        testJob "POST returns correct response 1" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PostOperation =
                        fun respond -> Ok(setStatusCode 201 >=> respond.WithOptEntity(A.resDef, Some { Id = "foo" }))
            }

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
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PostOperation = fun respond -> Ok(setStatusCode 201 >=> respond.WithOptEntity(A.resDef, None))
            }

            let! response = Request.post ctx "/entities/a1/customOp?include=someRel" |> getResponse
            response |> testStatusCode 201
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data" = null @>
        }

        testJob "POST insensitive to trailing slashes" {
            let ctx = {
                Ctx.WithDb(Db()) with
                    PostOperation = fun _ -> Ok(setStatusCode 201)
            }

            let! response = Request.post ctx "/entities/a1/customOp/" |> getResponse
            response |> testStatusCode 201
        }

        testJob "PATCH returns correct response" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PatchOperation =
                        fun respond ->
                            Ok(
                                setStatusCode 201
                                >=> respond.WithEntities(B.resDef, [ { Id = "foo" }; { Id = "bar" } ])
                            )
            }

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

        testJob "PATCH insensitive to trailing slashes" {
            let ctx = {
                Ctx.WithDb(Db()) with
                    PatchOperation = fun _ -> Ok(setStatusCode 201)
            }

            let! response = Request.patch ctx "/entities/a1/customOp/" |> getResponse
            response |> testStatusCode 201
        }

        testJob "DELETE returns correct response" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    DeleteOperation =
                        fun respond ->
                            Ok(
                                setStatusCode 201
                                >=> respond.WithPolymorphicEntities(
                                    [
                                        A.resDef.PolymorphicFor { Id = "foo" }
                                        B.resDef.PolymorphicFor { Id = "bar" }
                                    ]
                                )
                            )
            }

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

        testJob "DELETE insensitive to trailing slashes" {
            let ctx = {
                Ctx.WithDb(Db()) with
                    DeleteOperation = fun _ -> Ok(setStatusCode 201)
            }

            let! response = Request.delete ctx "/entities/a1/customOp/" |> getResponse
            response |> testStatusCode 201
        }

        testJob "Simple link when no meta" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    DisableMeta = true
                    GetOperation = fun respond -> Ok(setStatusCode 201 >=> respond.WithEntity(A.resDef, { Id = "foo" }))
            }

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

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header"
                @>

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

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx6 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx6 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "POST correctly handles optional precondition validation" {
            let! response = Request.post Ctx7 "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 200

            let! response =
                Request.post Ctx7 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx7 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "PATCH correctly handles precondition validation using ETag" {
            let! response = Request.patch Ctx5 "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 428
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "428" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header"
                @>

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

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.patch Ctx6 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.patch Ctx6 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "PATCH correctly handles optional precondition validation" {
            let! response = Request.patch Ctx7 "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 200

            let! response =
                Request.patch Ctx7 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.patch Ctx7 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "DELETE correctly handles precondition validation using ETag" {
            let! response = Request.delete Ctx5 "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 428
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "428" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header"
                @>

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

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.delete Ctx6 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.delete Ctx6 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "DELETE correctly handles optional precondition validation" {
            let! response = Request.delete Ctx7 "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 200

            let! response =
                Request.delete Ctx7 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.delete Ctx7 "/entities/a1/customOp"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "POST can read body" {
            let! response =
                Request.postWith HttpCtx "/as/ignoredId/customOp"
                |> Request.bodyString "foobar"
                |> getResponse

            response |> testSuccessStatusCode
        }

        testJob "GET returns 404 when resource is not found" {
            let db = Db()
            let! response = Request.get (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns 404 when resource is not found" {
            let db = Db()
            let! response = Request.post (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns 404 when resource is not found" {
            let db = Db()
            let! response = Request.patch (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns 404 when resource is not found" {
            let db = Db()
            let! response = Request.delete (Ctx.WithDb db) "/entities/nonExistentId/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not contain a resource with ID 'nonExistentId'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns errors returned by mapCtx" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.get ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns errors returned by mapCtx" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.post ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns errors returned by mapCtx" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.patch ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns errors returned by mapCtx" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.delete ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns errors returned by mapCtx with entity" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    MapCtxWithEntity = fun _ _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.get ctx "/entities/b1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "mapCtx with entity gets passed the entity" {
            let mutable calledWith = ValueNone
            let db = Db()
            let expected = db.TryGet "b1" |> Option.get

            let ctx = {
                Ctx.WithDb db with
                    MapCtxWithEntity =
                        fun _ctx e ->
                            calledWith <- ValueSome(B e)
                            Error [ Error.create 422 ]
            }

            let! _response = Request.get ctx "/entities/b1/customOp" |> getResponse
            Expect.equal calledWith (ValueSome expected) ""
        }

        testJob "GET returns errors returned by condition" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    Condition = Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.get ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns errors returned by condition" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    Condition = Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.post ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns errors returned by condition" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    Condition = Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.patch ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns errors returned by condition" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    Condition = Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.delete ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns errors returned by operation" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    GetOperation = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.get ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns errors returned by operation" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PostOperation = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.post ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns errors returned by operation" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PatchOperation = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.patch ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns errors returned by operation" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    DeleteOperation = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response = Request.delete ctx "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns 405 if verb not supported for resource" {
            let db = Db()
            let! response = Request.get (Ctx.WithDb db) "/entities/c1/customOp" |> getResponse
            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'c' does not support GET (other resource types in collection 'entities' may have an operation 'customOp' supporting GET)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "PATCH, DELETE" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns 405 if verb not supported for resource" {
            let db = Db()
            let! response = Request.post (Ctx.WithDb db) "/entities/c1/customOp" |> getResponse
            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'c' does not support POST (other resource types in collection 'entities' may have an operation 'customOp' supporting POST)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "PATCH, DELETE" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns 405 if verb not supported for resource" {
            let db = Db()
            let! response = Request.patch (Ctx.WithDb db) "/entities/b1/customOp" |> getResponse
            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'b' does not support PATCH (other resource types in collection 'entities' may have an operation 'customOp' supporting PATCH)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "GET, HEAD, POST" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns 405 if verb not supported for resource" {
            let db = Db()
            let! response = Request.delete (Ctx.WithDb db) "/entities/b1/customOp" |> getResponse
            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' on type 'b' does not support DELETE (other resource types in collection 'entities' may have an operation 'customOp' supporting DELETE)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "GET, HEAD, POST" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns 405 if verb not supported for any resource" {
            let! response = Request.get Ctx2 "/entities/idIgnored/customOpWithPatchAndDelete" |> getResponse
            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOpWithPatchAndDelete' does not support GET for any resource in collection 'entities'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "PATCH, DELETE" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns 405 if verb not supported for any resource" {
            let! response =
                Request.post Ctx2 "/entities/idIgnored/customOpWithPatchAndDelete"
                |> getResponse

            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOpWithPatchAndDelete' does not support POST for any resource in collection 'entities'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "PATCH, DELETE" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns 405 if verb not supported for any resource" {
            let! response = Request.patch Ctx2 "/entities/idIgnored/customOpWithGetAndPost" |> getResponse
            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOpWithGetAndPost' does not support PATCH for any resource in collection 'entities'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "GET, HEAD, POST" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns 405 if verb not supported for any resource" {
            let! response = Request.delete Ctx2 "/entities/idIgnored/customOpWithGetAndPost" |> getResponse
            response |> testStatusCode 405
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "405" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOpWithGetAndPost' does not support DELETE for any resource in collection 'entities'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ response.headers[Allow] = "GET, HEAD, POST" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns 404 if link not defined for resource" {
            let db = Db()
            let! response = Request.get (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns 404 if link not defined for resource" {
            let db = Db()
            let! response = Request.post (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns 404 if link not defined for resource" {
            let db = Db()
            let! response = Request.patch (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns 404 if link not defined for resource" {
            let db = Db()
            let! response = Request.delete (Ctx.WithDb db) "/entities/d1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Operation 'customOp' is not defined for resource type 'd' (it may exist for other resource types in collection 'entities')"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns 403 if missing lookup" {
            let! response = Request.get Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns 403 if missing lookup" {
            let! response = Request.post Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns 403 if missing lookup" {
            let! response = Request.patch Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns 403 if missing lookup" {
            let! response = Request.delete Ctx4 "/entities/ignoredId/ignoredOp" |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'entities' does not support any resource-specific operations"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns error if collection case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.get ctx "/Entities/a1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Entities/a1/customOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns error if link case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.get ctx "/entities/a1/CustomOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/entities/a1/CustomOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns error if collection case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.post ctx "/Entities/a1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Entities/a1/customOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST returns error if link case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.post ctx "/entities/a1/CustomOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/entities/a1/CustomOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns error if collection case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.patch ctx "/Entities/a1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Entities/a1/customOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH returns error if link case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.patch ctx "/entities/a1/CustomOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/entities/a1/CustomOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns error if collection case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.delete ctx "/Entities/a1/customOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Entities/a1/customOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE returns error if link case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.delete ctx "/entities/a1/CustomOp" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/entities/a1/CustomOp' does not exist, but differs only by case from the existing path '/entities/a1/customOp'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET returns 406 if Accept is not */* or application/vnd.api+json" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    GetOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response =
                Request.get ctx "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 406
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "406" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The client must accept the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET does not check Accept header if configured to skip this validation" {
            let! response =
                Request.get Ctx8 "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "POST returns 406 if Accept is not */* or application/vnd.api+json" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PostOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response =
                Request.post ctx "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 406
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "406" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The client must accept the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST does not check Accept header if configured to skip this validation" {
            let! response =
                Request.post Ctx8 "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "PATCH returns 406 if Accept is not */* or application/vnd.api+json" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PatchOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response =
                Request.patch ctx "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 406
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "406" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The client must accept the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH does not check Accept header if configured to skip this validation" {
            let! response =
                Request.patch Ctx8 "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "DELETE returns 406 if Accept is not */* or application/vnd.api+json" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    DeleteOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response =
                Request.delete ctx "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 406
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "406" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The client must accept the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE does not check Accept header if configured to skip this validation" {
            let! response =
                Request.delete Ctx8 "/entities/a1/customOp"
                |> Request.setHeader (Accept "application/json")
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "POST returns 415 if Content-Type is not application/vnd.api+json" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PostOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response =
                Request.post ctx "/entities/a1/customOp"
                |> Request.setHeader (ContentType (ContentType.parse "application/json").Value)
                |> Request.bodyString "foo"
                |> getResponse

            response |> testStatusCode 415
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "415" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Request content must be sent with Content-Type set to the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST does not check Content-Type header if configured to skip this validation" {
            let! response =
                Request.post Ctx9 "/entities/a1/customOp"
                |> Request.setHeader (ContentType (ContentType.parse "application/json").Value)
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "PATCH returns 415 if Content-Type is not application/vnd.api+json" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PatchOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response =
                Request.patch ctx "/entities/a1/customOp"
                |> Request.setHeader (ContentType (ContentType.parse "application/json").Value)
                |> Request.bodyString "foo"
                |> getResponse

            response |> testStatusCode 415
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "415" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Request content must be sent with Content-Type set to the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH does not check Content-Type header if configured to skip this validation" {
            let! response =
                Request.patch Ctx9 "/entities/a1/customOp"
                |> Request.setHeader (ContentType (ContentType.parse "application/json").Value)
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "DELETE returns 415 if Content-Type is not application/vnd.api+json" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    DeleteOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response =
                Request.delete ctx "/entities/a1/customOp"
                |> Request.setHeader (ContentType (ContentType.parse "application/json").Value)
                |> Request.bodyString "foo"
                |> getResponse

            response |> testStatusCode 415
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "415" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Request content must be sent with Content-Type set to the JSON:API media type (application/vnd.api+json)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE does not check Content-Type header if configured to skip this validation" {
            let! response =
                Request.delete Ctx9 "/entities/a1/customOp"
                |> Request.setHeader (ContentType (ContentType.parse "application/json").Value)
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "GET returns 400 if a query parameter name is invalid" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    GetOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response = Request.get ctx "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].source.parameter" = "invalid" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "'invalid' is not an allowed query parameter name according to the JSON:API specification"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "GET does not check query parameter names if configured to skip this validation" {
            let! response = Request.get Ctx10 "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 200
        }

        testJob "POST returns 400 if a query parameter name is invalid" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PostOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response = Request.post ctx "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].source.parameter" = "invalid" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "'invalid' is not an allowed query parameter name according to the JSON:API specification"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST does not check query parameter names if configured to skip this validation" {
            let! response = Request.post Ctx10 "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 200
        }

        testJob "PATCH returns 400 if a query parameter name is invalid" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    PatchOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response = Request.patch ctx "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].source.parameter" = "invalid" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "'invalid' is not an allowed query parameter name according to the JSON:API specification"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "PATCH does not check query parameter names if configured to skip this validation" {
            let! response = Request.patch Ctx10 "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 200
        }

        testJob "DELETE returns 400 if a query parameter name is invalid" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    DeleteOperation = fun _ -> Ok(setStatusCode 200)
            }

            let! response = Request.delete ctx "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].source.parameter" = "invalid" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "'invalid' is not an allowed query parameter name according to the JSON:API specification"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "DELETE does not check query parameter names if configured to skip this validation" {
            let! response = Request.delete Ctx10 "/entities/a1/customOp?invalid" |> getResponse
            response |> testStatusCode 200
        }

        testJob "Link is not added to resource if using SkipLink" {
            let! resourceResponse = Request.get Ctx11 "/entities/a1" |> getResponse
            resourceResponse |> testStatusCode 200
            let! json = resourceResponse |> Response.readBodyAsString
            test <@ json |> hasPath "data.links.self" @>
            test <@ json |> hasNoPath "data.links.customOp" @>

            // Verify that operation still works
            let! response = Request.get Ctx11 "/entities/a1/customOp" |> getResponse
            response |> testStatusCode 200
        }

        testJob "WithNoEntity returns status code 204 and empty body when there is no content" {
            let ctx = { StatusCode = None; Meta = Map.empty }
            let client = getClientForMeta ctx

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testStatusCode 204
            let! json = response |> Response.readBodyAsString
            test <@ json = "" @>
        }

        testJob
            "WithNoEntity returns status code 204 and empty body when there is no content, even if a status code was set" {
            let ctx = {
                StatusCode = Some 200
                Meta = Map.empty
            }

            let client = getClientForMeta ctx

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testStatusCode 204
            let! json = response |> Response.readBodyAsString
            test <@ json = "" @>
        }

        testJob "WithNoEntity returns status code 200 by default when there is meta" {
            let ctx = {
                StatusCode = None
                Meta = Map.empty.Add("someValue", 1)
            }

            let client = getClientForMeta ctx

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testStatusCode 200
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "meta.someValue" = 1 @>
        }

        testJob "WithNoEntity returns the specified status code when there is meta" {
            let ctx = {
                StatusCode = Some 201
                Meta = Map.empty.Add("someValue", 1)
            }

            let client = getClientForMeta ctx

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testStatusCode 201
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "meta.someValue" = 1 @>
        }

    ]
