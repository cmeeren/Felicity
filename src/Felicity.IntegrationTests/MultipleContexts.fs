module ``Multiple contexts``

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Expecto
open HttpFs.Client
open Felicity


type Ctx1 = Ctx1

type Ctx2 = Ctx2

type A = A

module A1 =

  let define = Define<Ctx1, A, string>()
  let resId = define.Id.Simple(fun _-> "1")
  let resDef = define.Resource("a", resId).CollectionName("ctx1")
  let getColl = define.Operation.GetCollection(fun () -> [A])

module A2 =

  let define = Define<Ctx2, A, string>()
  let resId = define.Id.Simple(fun _-> "1")
  let resDef = define.Resource("a", resId).CollectionName("ctx2")
  let getColl = define.Operation.GetCollection(fun () -> [A])


[<Tests>]
let tests =
  testList "Multiple contexts" [

    testJob "Can reach second context if first fails" {

      let server =
        new TestServer(
          WebHostBuilder()
            .ConfigureServices(fun services ->
              services
                .AddGiraffe()
                .AddRouting()
                .AddJsonApi()
                  .GetCtxRes(fun _ -> (Error [Error.create 422]: Result<Ctx1, _>))
                  .EnableUnknownFieldStrictMode()
                  .EnableUnknownQueryParamStrictMode()
                  .Add()
                .AddJsonApi()
                  .GetCtx(fun _ -> Ctx2)
                  .EnableUnknownFieldStrictMode()
                  .EnableUnknownQueryParamStrictMode()
                  .Add()
                |> ignore)
            .Configure(fun app ->
              app
                .UseRouting()
                .UseJsonApiEndpoints<Ctx1>()
                .UseJsonApiEndpoints<Ctx2>()
              |> ignore
            )
        )
      let client = server.CreateClient ()

      let! response1 =
        Request.createWithClient client Get (Uri("http://example.com/ctx1"))
        |> Request.jsonApiHeaders
        |> getResponse
      response1 |> testStatusCode 422

      let! response2 =
        Request.createWithClient client Get (Uri("http://example.com/ctx2"))
        |> Request.jsonApiHeaders
        |> getResponse
      response2 |> testSuccessStatusCode
    }

]
