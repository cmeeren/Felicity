module Routing

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type Ctx = Ctx

type A = A

module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun _-> "1")
  let resDef = define.Resource("a", resId).CollectionName("as")
  let knownRel = define.Relationship.ToOne(resDef).Get(fun _ -> A)
  let lookup = define.Operation.Lookup(fun _ -> Some A)
  let post = define.Operation.Post(fun () -> A).AfterCreate(ignore)
  let get = define.Operation.GetResource()

[<Tests>]
let tests =
  testList "Routing" [

    testJob "Unknown/invalid collections fall through" {
      let! response =
        Request.get Ctx "/unknown"
        |> Request.setHeader (Accept "text/plain")
        |> getResponse
      response |> testStatusCode 404
      let! content = response |> Response.readBodyAsString
      test <@ content = "" @>
    }

    testJob "Unknown link or relationship related URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/unknownRel"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The link or relationship 'unknownRel' does not exist for any resource in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown relationship self URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/relationships/unknownRel"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The relationship 'unknownRel' does not exist for any resource in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after unknown relationship related URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/unknownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'unknownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after known relationship related URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/knownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'knownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after unknown relationship self URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/relationships/unknownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'relationships/unknownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after known relationship self URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/relationships/knownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'relationships/knownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "If no base URL or relative root path is specified, uses the base URL from the request" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx)
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response1 =
        Request.createWithClient client Get (Uri("http://example1.com/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response1 |> testSuccessStatusCode
      let! json1 = response1 |> Response.readBodyAsString
      test <@ json1 |> getPath "data.links.self" = "http://example1.com/as/1" @>

      let! response2 =
        Request.createWithClient client Get (Uri("http://example2.com/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response2 |> testSuccessStatusCode
      let! json2 = response2 |> Response.readBodyAsString
      test <@ json2 |> getPath "data.links.self" = "http://example2.com/as/1" @>
    }


    testJob "If relative root path is specified, uses the base URL from the request with the root path inserted" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx)
                  .RelativeJsonApiRoot("foo/bar")
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response1 =
        Request.createWithClient client Get (Uri("http://example1.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response1 |> testSuccessStatusCode
      let! json1 = response1 |> Response.readBodyAsString
      test <@ json1 |> getPath "data.links.self" = "http://example1.com/foo/bar/as/1" @>

      let! response2 =
        Request.createWithClient client Get (Uri("http://example2.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response2 |> testSuccessStatusCode
      let! json2 = response2 |> Response.readBodyAsString
      test <@ json2 |> getPath "data.links.self" = "http://example2.com/foo/bar/as/1" @>
    }


    testJob "Relative root path works with leading slash" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx)
                  .RelativeJsonApiRoot("/foo/bar")
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response1 =
        Request.createWithClient client Get (Uri("http://example.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response1 |> testSuccessStatusCode
      let! json1 = response1 |> Response.readBodyAsString
      test <@ json1 |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>
    }


    testJob "Relative root path works with trailing slash" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx)
                  .RelativeJsonApiRoot("foo/bar/")
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response =
        Request.createWithClient client Get (Uri("http://example.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>
    }


    testJob "Relative root path is case sensitive" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx)
                  .RelativeJsonApiRoot("foo/bar")
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response =
        Request.createWithClient client Get (Uri("http://example.com/Foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path '/Foo/bar/as/1' does not exist, but differs only by case from the existing path '/foo/bar/as/1'. Paths are case sensitive." @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }


    testJob "If base URL is specified, uses the specified base URL in links regardless of actual request URL" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx)
                  .BaseUrl("http://example.com/foo/bar")
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response1 =
        Request.createWithClient client Get (Uri("http://example.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response1 |> testSuccessStatusCode
      let! json1 = response1 |> Response.readBodyAsString
      test <@ json1 |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>

      let! response2 =
        Request.createWithClient client Get (Uri("http://something-else.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response2 |> testSuccessStatusCode
      let! json2 = response2 |> Response.readBodyAsString
      test <@ json2 |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>
    }


    testJob "Base URL works with trailing slash" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx)
                  .BaseUrl("http://example.com/foo/bar/")
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response1 =
        Request.createWithClient client Get (Uri("http://example.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response1 |> testSuccessStatusCode
      let! json1 = response1 |> Response.readBodyAsString
      test <@ json1 |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>

      let! response2 =
        Request.createWithClient client Get (Uri("http://something-else.com/foo/bar/as/1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response2 |> testSuccessStatusCode
      let! json2 = response2 |> Response.readBodyAsString
      test <@ json2 |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>
    }


    testJob "If both base URL and relative root path is specified, uses the specified base URL in links regardless of actual request URL and root path, and makes the endpoints available at the root path" {

      for path in ["baz/qux"; "/"; ""] do
        let server =
          new TestServer(
            WebHostBuilder()
              .ConfigureServices(fun services ->
                services
                  .AddGiraffe()
                  .AddRouting()
                  .AddJsonApi()
                    .GetCtx(fun _ -> Ctx)
                    .BaseUrl("http://example.com/foo/bar")
                    .RelativeJsonApiRoot(path)
                    .Add()
                  |> ignore)
              .Configure(fun app ->
                app
                  .UseRouting()
                  .UseJsonApiEndpoints<Ctx>()
                |> ignore
              )
          )
        let client = server.CreateClient ()

        let urlBasePath = if path.Trim('/') = "" then "" else "/" + path.Trim('/')

        let! response1 =
          Request.createWithClient client Get (Uri($"http://example.com{urlBasePath}/as/1"))
          |> Request.jsonApiHeaders
          |> getResponse
        response1 |> testSuccessStatusCode
        let! json1 = response1 |> Response.readBodyAsString
        test <@ json1 |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>

        let! response2 =
          Request.createWithClient client Get (Uri($"http://something-else.com{urlBasePath}/as/1"))
          |> Request.jsonApiHeaders
          |> getResponse
        response2 |> testSuccessStatusCode
        let! json2 = response2 |> Response.readBodyAsString
        test <@ json2 |> getPath "data.links.self" = "http://example.com/foo/bar/as/1" @>
    }

]
