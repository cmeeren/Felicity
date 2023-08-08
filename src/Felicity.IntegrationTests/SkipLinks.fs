module ``Skip links``

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Expecto
open HttpFs.Client
open Felicity


type Ctx = Ctx


type A = A


module A =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun _ -> "1")
    let resDef = define.Resource("a", resId).CollectionName("as")
    let lookup = define.Operation.Lookup(fun _ -> Some A)

    let toOne = define.Relationship.ToOne(resDef).Get(fun _ -> A)

    let toOneNullable = define.Relationship.ToOneNullable(resDef).Get(fun _ -> None)

    let toMany = define.Relationship.ToMany(resDef).Get(fun _ -> [])

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun ctx parser respond a -> async { return Ok(respond.WithEntity(resDef, a)) })


let getClient () =
    let server =
        new TestServer(
            WebHostBuilder()
                .ConfigureServices(fun services ->
                    services
                        .AddGiraffe()
                        .AddRouting()
                        .AddJsonApi()
                        .GetCtx(fun _ -> Ctx)
                        .SkipStandardLinksQueryParamName("skipStandardLinks", "skipAllLinks")
                        .SkipCustomLinksQueryParamName("skipCustomLinks", "skipAllLinks")
                        .EnableUnknownFieldStrictMode()
                        .EnableUnknownQueryParamStrictMode()
                        .Add()
                    |> ignore
                )
                .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<Ctx>() |> ignore)
        )

    server.CreateClient()


[<Tests>]
let tests =
    testList "Skip links" [

        testJob "Has all links if no query parameter is specified" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasPath "data.links.self") ""
            Expect.isTrue (json |> hasPath "data.links.customOp") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOne.links.self") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOne.links.related") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOneNullable.links.self") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOneNullable.links.related") ""
            Expect.isTrue (json |> hasPath "data.relationships.toMany.links.self") ""
            Expect.isTrue (json |> hasPath "data.relationships.toMany.links.related") ""
        }

        testJob "Has only custom links if standard links are skipped" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1?skipStandardLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isFalse (json |> hasPath "data.links.self") ""
            Expect.isTrue (json |> hasPath "data.links.customOp") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOne") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOneNullable") ""
            Expect.isFalse (json |> hasPath "data.relationships.toMany") ""
        }

        testJob "Has only standard links if custom links are skipped" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1?skipCustomLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasPath "data.links.self") ""
            Expect.isFalse (json |> hasPath "data.links.customOp") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOne.links.self") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOne.links.related") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOneNullable.links.self") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOneNullable.links.related") ""
            Expect.isTrue (json |> hasPath "data.relationships.toMany.links.self") ""
            Expect.isTrue (json |> hasPath "data.relationships.toMany.links.related") ""
        }

        testJob "Has no links if all links are skipped using both parameters" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1?skipStandardLinks&skipCustomLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isFalse (json |> hasPath "data.links") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOne") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOneNullable") ""
            Expect.isFalse (json |> hasPath "data.relationships.toMany") ""
        }

        testJob "Has no links if all links are skipped using a common parameter" {
            let client = getClient ()

            let! response =
                Request.createWithClient client Get (Uri("http://example.com/as/1?skipAllLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isFalse (json |> hasPath "data.links") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOne") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOneNullable") ""
            Expect.isFalse (json |> hasPath "data.relationships.toMany") ""
        }

        testJob "Correctly relationship data but not links if relationships are included and links are skipped" {
            let client = getClient ()

            let! response =
                Request.createWithClient
                    client
                    Get
                    (Uri("http://example.com/as/1?include=toOne,toOneNullable,toMany&skipStandardLinks"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            Expect.isTrue (json |> hasPath "data.relationships.toOne.data") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOne.links") ""
            Expect.isTrue (json |> hasPath "data.relationships.toOneNullable.data") ""
            Expect.isFalse (json |> hasPath "data.relationships.toOneNullable.links") ""
            Expect.isTrue (json |> hasPath "data.relationships.toMany.data") ""
            Expect.isFalse (json |> hasPath "data.relationships.toMany.links") ""
        }

        testJob "Query parameter values cause errors" {
            let client = getClient ()

            let! response =
                Request.createWithClient
                    client
                    Get
                    (Uri("http://example.com/as/1?skipStandardLinks=false&skipCustomLinks=0&skipStandardLinks=foo"))
                |> Request.jsonApiHeaders
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            Expect.equal (json |> getPath "errors[0].status") "400" ""

            Expect.equal
                (json |> getPath "errors[0].detail")
                "Query parameter 'skipStandardLinks' must be specified without a value, but got 'false'"
                ""

            Expect.equal (json |> getPath "errors[0].source.parameter") "skipStandardLinks" ""
            Expect.equal (json |> getPath "errors[1].status") "400" ""

            Expect.equal
                (json |> getPath "errors[1].detail")
                "Query parameter 'skipCustomLinks' must be specified without a value, but got '0'"
                ""

            Expect.equal (json |> getPath "errors[1].source.parameter") "skipCustomLinks" ""
            Expect.isTrue (json |> hasNoPath "errors[2]") ""
        }

    ]
