[<AutoOpen>]
module Utils

open System
open Microsoft.AspNetCore.TestHost
open Microsoft.AspNetCore.Hosting
open Expecto
open Giraffe
open Hopac
open HttpFs.Client
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Felicity


/// Gets the value at the specified JSON path.
let getPath<'a> (path: string) (json: string) =
  let o = JObject.Parse(json)
  o.SelectToken(path, true).ToObject<'a>()

/// Gets the value at the specified JSON path.
let hasNoPath (path: string) (json: string) =
  let o = JObject.Parse(json)
  o.SelectToken(path, false) |> isNull

let hasPath (path: string) (json: string) =
  let o = JObject.Parse(json)
  o.SelectToken(path, false) |> isNull |> not


type SecondCtx = SecondCtx


let startTestServer (ctx: 'ctx) =
  let server =
    new TestServer(
      WebHostBuilder()
        .ConfigureServices(fun services ->
          services
            .AddGiraffe()
            .AddJsonApi()
              .GetCtx(fun _ -> ctx)
              .Add()
            .AddJsonApi()
              .GetCtx(fun _ -> SecondCtx)
              .Add()
          |> ignore)
        .Configure(fun app ->
          app
            .UseGiraffeErrorHandler(fun _ _ -> returnUnknownError)
            .UseGiraffe jsonApi<'ctx>
        )
    )
  server.CreateClient ()



module Request =

  let jsonApiHeaders req =
    req
    |> Request.setHeader (Accept "application/vnd.api+json")
    |> Request.setHeader (ContentType <| ContentType.create("application", "vnd.api+json"))

  let bodySerialized (body: 'a) req =
    req |> Request.bodyString (JsonConvert.SerializeObject body)

  let get ctx path =
    let testClient = startTestServer ctx
    Request.createWithClient testClient Get (Uri("http://example.com" + path))
    |> jsonApiHeaders

  let post ctx path =
    let testClient = startTestServer ctx
    Request.createWithClient testClient Post (Uri("http://example.com" + path))
    |> jsonApiHeaders

  let patch ctx path =
    let testClient = startTestServer ctx
    Request.createWithClient testClient Patch (Uri("http://example.com" + path))
    |> jsonApiHeaders

  let delete ctx path =
    let testClient = startTestServer ctx
    Request.createWithClient testClient Delete (Uri("http://example.com" + path))
    |> jsonApiHeaders


/// Fails the test if the response is not 2XX. If failed, prints the response and the
/// response body.
let testSuccessStatusCode (resp: Response) =
  let statusOk = resp.statusCode >= 200 && resp.statusCode < 300
  if not statusOk then
    let body =
      try resp |> Response.readBodyAsString |> run
      with ex -> sprintf "[Failed to get response body with exception: %s: %s]" (ex.GetType().Name) ex.Message
    failtestf "Expected success response code, but got %i. Message body:\n\n%s\n\nComplete response:\n\n%A" resp.statusCode body resp


/// Fails the test if the response is 2XX. If failed, prints the response and the
/// response body.
let testErrorStatusCode (resp: Response) =
  let statusOk = resp.statusCode >= 200 && resp.statusCode < 300
  if statusOk then
    let body =
      try resp |> Response.readBodyAsString |> run
      with ex -> sprintf "[Failed to get response body with exception: %s: %s]" (ex.GetType().Name) ex.Message
    failtestf "Expected error response code, but got %i. Message body:\n\n%s\n\nComplete response:\n\n%A" resp.statusCode body resp


/// Fails the test if the response is the specified code. If failed, prints the response and the
/// response body.
let testStatusCode code (resp: Response) =
  if resp.statusCode <> code then
    let body =
      try resp |> Response.readBodyAsString |> run
      with ex -> sprintf "[Failed to get response body with exception: %s: %s]" (ex.GetType().Name) ex.Message
    failtestf "Expected response code %i, but got %i. Message body:\n\n%s\n\nComplete response:\n\n%A" code resp.statusCode body resp
