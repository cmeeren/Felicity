module TopLevelLinks

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Expecto
open HttpFs.Client
open Felicity


type Ctx1 = {
    mutable Links: Map<string, string * Map<string, obj>>
}


type A = A


module A =

    let define = Define<Ctx1, A, string>()
    let resId = define.Id.Simple(fun _ -> "1")
    let resDef = define.Resource("a", resId).CollectionName("as")
    let lookup = define.Operation.Lookup(fun _ -> Some A)

    let getColl =
        define.Operation.GetCollection(fun (ctx: Ctx1) ->
            ctx.Links <-
                Map.empty
                |> Map.add
                    "self"
                    ("http://foo.bar", Map.empty.Add("someMeta", box {| test1 = 123; test2 = "some value" |}))
                |> Map.add
                    "related"
                    ("a", Map.empty)
                |> Map.add
                    "describedby"
                    ("b", Map.empty)
                |> Map.add
                    "first"
                    ("c", Map.empty)
                |> Map.add
                    "last"
                    ("d", Map.empty)
                |> Map.add
                    "prev"
                    ("e", Map.empty)
                |> Map.add
                    "next"
                    ("f", Map.empty)
                |> Map.add
                    "custom"
                    ("http://foo2.bar", Map.empty.Add("otherMeta", box {|
                            test3 = 456
                            test4 = "another value"
                        |}))
            [ A ]
        )

    let post =
        define.Operation
            .Post(fun (ctx: Ctx1) ->
                ctx.Links <-
                    Map.empty.Add(
                        "bar",
                        ("http://foo3.bar", Map.empty)
                    )

                A
            )
            .AfterCreate(ignore)

    let get =
        define.Operation
            .GetResource()
            .ModifyResponse(fun (ctx: Ctx1) _a ->
                ctx.Links <- Map.empty
                fun next ctx -> next ctx
            )

    let patch =
        define.Operation
            .Patch()
            .ModifyResponse(fun (ctx: Ctx1) _a ->
                ctx.Links <- Map.empty
                fun next ctx -> next ctx
            )
            .AfterUpdate(ignore)

    let delete =
        define.Operation.DeleteRes(fun (ctx: Ctx1) a ->
            ctx.Links <- Map.empty.Add("foo", ("http://foo4.bar", Map.empty))
            Error [ (Error.create 400) ]
        )


let getClient () =
    let server =
        new TestServer(
            WebHostBuilder()
                .ConfigureServices(fun services ->
                    services
                        .AddGiraffe()
                        .AddRouting()
                        .AddJsonApi()
                        .GetCtx(fun _ -> { Ctx1.Links = Map.empty })
                        .GetTopLevelLinksWithMeta(_.Links)
                        .SkipStandardLinksQueryParamName("skipStandardLinks")
                        .SkipCustomLinksQueryParamName("skipCustomLinks")
                        .EnableUnknownFieldStrictMode()
                        .EnableUnknownQueryParamStrictMode()
                        .Add()
                    |> ignore
                )
                .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<Ctx1>() |> ignore)
        )

    server.CreateClient()


[<Tests>]
let tests =
    testList "Links" [

        testJob "Has expected links 1" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.equal "http://foo.bar" (json |> getPath "links.self.href") ""
            Expect.equal 123 (json |> getPath "links.self.meta.someMeta.test1") ""
            Expect.equal "some value" (json |> getPath "links.self.meta.someMeta.test2") ""
            Expect.equal "a" (json |> getPath "links.related") ""
            Expect.equal "b" (json |> getPath "links.describedby") ""
            Expect.equal "c" (json |> getPath "links.first") ""
            Expect.equal "d" (json |> getPath "links.last") ""
            Expect.equal "e" (json |> getPath "links.prev") ""
            Expect.equal "f" (json |> getPath "links.next") ""
            Expect.equal "http://foo2.bar" (json |> getPath "links.custom.href") ""
            Expect.equal 456 (json |> getPath "links.custom.meta.otherMeta.test3") ""
            Expect.equal "another value" (json |> getPath "links.custom.meta.otherMeta.test4") ""
        }

        testJob "Has expected links 2" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Post (Uri("http://example.com/as"))
                |> Request.jsonApiHeaders
                |> Request.bodySerialized {| data = {| ``type`` = "a" |} |}
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.equal "http://foo3.bar" (json |> getPath "links.bar") ""
        }

        testJob "Omits standard links when skipped" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as?skipStandardLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "links.self") json
            Expect.isTrue (json |> hasNoPath "links.related") json
            Expect.isTrue (json |> hasNoPath "links.describedby") json
            Expect.isTrue (json |> hasNoPath "links.first") json
            Expect.isTrue (json |> hasNoPath "links.last") json
            Expect.isTrue (json |> hasNoPath "links.prev") json
            Expect.isTrue (json |> hasNoPath "links.next") json
            Expect.equal "http://foo2.bar" (json |> getPath "links.custom.href") ""
            Expect.equal 456 (json |> getPath "links.custom.meta.otherMeta.test3") ""
            Expect.equal "another value" (json |> getPath "links.custom.meta.otherMeta.test4") ""
        }

        testJob "Omits custom links when skipped" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as?skipCustomLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.equal "http://foo.bar" (json |> getPath "links.self.href") ""
            Expect.equal 123 (json |> getPath "links.self.meta.someMeta.test1") ""
            Expect.equal "some value" (json |> getPath "links.self.meta.someMeta.test2") ""
            Expect.equal "a" (json |> getPath "links.related") ""
            Expect.equal "b" (json |> getPath "links.describedby") ""
            Expect.equal "c" (json |> getPath "links.first") ""
            Expect.equal "d" (json |> getPath "links.last") ""
            Expect.equal "e" (json |> getPath "links.prev") ""
            Expect.equal "f" (json |> getPath "links.next") ""
            Expect.isTrue (json |> hasNoPath "links.custom") json
        }

        testJob "Omits all links when skipped" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as?skipStandardLinks&skipCustomLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "links") json
        }

        testJob "No links if empty map 1" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "links") json
        }

        testJob "No links if empty map 2" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Patch (Uri("http://example.com/as/1"))
                |> Request.bodySerialized {|
                    data = {| ``type`` = "a"; id = "1" |}
                |}
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "links") json
        }

        testJob "No links if error" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Delete (Uri("http://example.com/as/1"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasNoPath "links") json
        }

    ]
