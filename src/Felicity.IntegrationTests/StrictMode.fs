module ``Strict mode``

open System
open Expecto
open Swensen.Unquote
open HttpFs.Client
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Felicity


type Ctx = Ctx


type A = A of string


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun (A id) -> id)
  let resDef = define.Resource("a", resId).CollectionName("as")

  let lookup = define.Operation.Lookup(A >> Some)

  let getColl =
    define.Operation.GetCollection(fun ctx parser ->
      parser
        .For((fun _ -> []), Query.String("customQueryParam"))
        .IgnoreStrictMode(Query.String("ignoredQueryParam"))
    )

  let post =
    define.Operation.Post(fun ctx parser ->
      parser
        .For((fun _ -> A "1"), Query.String("customQueryParam"))
        .IgnoreStrictMode(Query.String("ignoredQueryParam"))
    ).AfterCreate(ignore)

  let get = define.Operation.GetResource()

  let toOne =
    define.Relationship
      .ToOne(resDef)
      .Get(id)
      .Set(fun _ a -> a)
      .AfterModifySelf(ignore)

  let toOneNullable =
    define.Relationship
      .ToOneNullable(resDef)
      .Get(Some)
      .Set(fun _ a -> a)
      .AfterModifySelf(ignore)

  let toMany =
    define.Relationship
      .ToMany(resDef)
      .Get(fun _ -> [])
      .SetAll(fun _ a -> a)
      .Add(fun _ a -> a)
      .Remove(fun _ a -> a)
      .AfterModifySelf(ignore)

  let patch =
    define.Operation
      .Patch()
      .AddCustomSetter(fun ctx a parser ->
        parser
          .For((fun _ -> a), Query.String("customQueryParam"))
          .IgnoreStrictMode(Query.String("ignoredQueryParam")))
      .AfterUpdate(ignore)

  let delete = define.Operation.Delete(fun _ -> ())



let createServerAndGetClient warnOnly =
  let server =
    new TestServer(
      WebHostBuilder()
        .ConfigureServices(fun services ->
          services
            .AddGiraffe()
            .AddRouting()
            .AddJsonApi()
              .GetCtx(fun _ -> Ctx)
              .EnableUnknownFieldStrictMode(warnOnly)
              .EnableUnknownQueryParamStrictMode(warnOnly)
              .Add()
            |> ignore)
        .Configure(fun app ->
          app
            .UseRouting()
            .UseJsonApiEndpoints<Ctx>()
          |> ignore
        )
    )
  server.CreateClient ()


[<Tests>]
let tests =
  testList "Strict mode" [


    testJob "GET collection: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as?customQueryParam=foo&a1=bar&a2=baz&ignoredQueryParam=ignored&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET collection: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as?customQueryParam=foo&a1=bar&a2=baz&ignoredQueryParam=ignored&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "POST collection: Returns errors for each unknown field" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Post (Uri("http://example.com/as"))
        |> Request.bodySerialized {| data = {| ``type`` = "a"; attributes = {| attr = null |}; relationships = {| rel = {| |} |} |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Type 'a' has no attribute 'attr'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/attr" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Type 'a' has no relationship 'rel'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/rel" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "POST collection: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Post (Uri("http://example.com/as?customQueryParam=foo&a1=bar&a2=baz&ignoredQueryParam=ignored&fields[a]=&include="))
        |> Request.bodySerialized {| data = {| ``type`` = "a" |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "POST collection: Does not return errors for unknown fields or query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Post (Uri("http://example.com/as?customQueryParam=foo&a1=bar&a2=baz&ignoredQueryParam=ignored&fields[a]=&include="))
        |> Request.bodySerialized {| data = {| ``type`` = "a"; attributes = {| attr = null |}; relationships = {| rel = {| |} |} |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "GET resource: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET resource: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "PATCH resource: Returns errors for each unknown field" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1"))
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "1"; attributes = {| attr = null |}; relationships = {| rel = {| |} |} |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Type 'a' has no attribute 'attr'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/attr" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Type 'a' has no relationship 'rel'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/rel" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "PATCH resource: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1?customQueryParam=foo&a1=bar&a2=baz&ignoredQueryParam=ignored&fields[a]=&include="))
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "1" |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "PATCH resource: Does not return errors for unknown fields or query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1?customQueryParam=foo&a1=bar&a2=baz&ignoredQueryParam=ignored&fields[a]=&include="))
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "1"; attributes = {| attr = null |}; relationships = {| rel = {| |} |} |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "DELETE resource: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Delete (Uri("http://example.com/as/1?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "DELETE resource: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Delete (Uri("http://example.com/as/1?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "GET to-one relationship related: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/toOne?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET to-one relationship related: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/toOne?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "GET to-one relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/relationships/toOne?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET to-one relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/relationships/toOne?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "PATCH to-one relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1/relationships/toOne?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "1" |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "PATCH to-one relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1/relationships/toOne?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "1" |} |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "GET to-one nullable relationship related: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/toOneNullable?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET to-one nullable relationship related: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/toOneNullable?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "GET to-one nullable relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/relationships/toOneNullable?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET to-one nullable relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/relationships/toOneNullable?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "PATCH to-one nullable relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1/relationships/toOneNullable?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = null |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "PATCH to-one nullable relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1/relationships/toOneNullable?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = null |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "GET to-many relationship related: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET to-many relationship related: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "GET to-many relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "GET to-many relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "POST to-many relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Post (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = [||] |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "POST to-many relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Post (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = [||] |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "PATCH to-many relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = [||] |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "PATCH to-many relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Patch (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = [||] |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "DELETE to-many relationship self: Returns errors for each unknown query parameter" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Delete (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = [||] |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'a1' is not recognized for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "a1" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'a2' is not recognized for this operation" @>
      test <@ json |> getPath "errors[1].source.parameter" = "a2" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }


    testJob "DELETE to-many relationship self: Does not return errors for unknown query parameters in warn-only mode" {
      let client = createServerAndGetClient true
      let! response =
        Request.createWithClient client Delete (Uri("http://example.com/as/1/relationships/toMany?a1=bar&a2=baz&fields[a]=&include="))
        |> Request.bodySerialized {| data = [||] |}
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


    testJob "Does not return errors if an allowed query parameter is used multiple times" {
      let client = createServerAndGetClient false
      let! response =
        Request.createWithClient client Get (Uri("http://example.com/as?customQueryParam=foo&customQueryParam=bar"))
        |> Request.jsonApiHeaders
        |> getResponse
      response |> testSuccessStatusCode
    }


  ]
