module Meta

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Expecto
open HttpFs.Client
open Felicity


type Ctx1 = { mutable Meta: Map<string, obj> }


type A = A


module A =

    let define = Define<Ctx1, A, string>()
    let resId = define.Id.Simple(fun _ -> "1")
    let resDef = define.Resource("a", resId).CollectionName("as")
    let lookup = define.Operation.Lookup(fun _ -> Some A)

    let getColl =
        define.Operation.GetCollection(fun (ctx: Ctx1) ->
            ctx.Meta <- Map.empty.Add("foo", box {| test1 = 123; test2 = "some value" |})
            [ A ])

    let post =
        define
            .Operation
            .Post(fun (ctx: Ctx1) ->
                ctx.Meta <-
                    Map.empty.Add(
                        "bar",
                        box
                            {|
                                test3 = 456
                                test4 = "another value"
                            |}
                    )

                A)
            .AfterCreate(ignore)

    let get =
        define
            .Operation
            .GetResource()
            .ModifyResponse(fun (ctx: Ctx1) _a ->
                ctx.Meta <- Map.empty
                fun next ctx -> next ctx)

    let patch =
        define
            .Operation
            .Patch()
            .ModifyResponse(fun (ctx: Ctx1) _a ->
                ctx.Meta <- Map.empty
                fun next ctx -> next ctx)
            .AfterUpdate(ignore)

    let delete =
        define.Operation.DeleteRes(fun (ctx: Ctx1) a ->
            ctx.Meta <- Map.empty.Add("foo", box "bar")
            Error [ (Error.create 400) ])


let getClient () =
    let server =
        new TestServer(
            WebHostBuilder()
                .ConfigureServices(fun services ->
                    services
                        .AddGiraffe()
                        .AddRouting()
                        .AddJsonApi()
                        .GetCtx(fun _ -> { Ctx1.Meta = Map.empty })
                        .GetMeta(fun ctx -> ctx.Meta)
                        .EnableUnknownFieldStrictMode()
                        .EnableUnknownQueryParamStrictMode()
                        .Add()
                    |> ignore)
                .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<Ctx1>() |> ignore)
        )

    server.CreateClient()


[<Tests>]
let tests =
    testList "Meta" [

        testJob "Has expected meta 1" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.equal 123 (json |> getPath "meta.foo.test1") ""
            Expect.equal "some value" (json |> getPath "meta.foo.test2") ""
        }

        testJob "Has expected meta 2" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Post (Uri("http://example.com/as"))
                |> Request.jsonApiHeaders
                |> Request.bodySerialized {| data = {| ``type`` = "a" |} |}
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.equal 456 (json |> getPath "meta.bar.test3") ""
            Expect.equal "another value" (json |> getPath "meta.bar.test4") ""
        }

        testJob "No meta if empty map 1" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "meta") json
        }

        testJob "No meta if empty map 2" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Patch (Uri("http://example.com/as/1"))
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "a"; id = "1" |}
                    |}
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "meta") json
        }

        testJob "No meta if error" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Delete (Uri("http://example.com/as/1"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "meta") json
        }

    ]
