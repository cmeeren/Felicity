[<AutoOpen>]
module Utils

open System
open MELT
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.TestHost
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Expecto
open Giraffe
open Hopac
open HttpFs.Client
open Microsoft.Extensions.Logging
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


type SpyLogger () =
  interface ILogger with
    member this.BeginScope(state) = { new IDisposable with member _.Dispose() = () }
    member this.IsEnabled(logLevel) = true
    member this.Log(logLevel, eventId, state, ``exception``, formatter) = failwith "todo"


let private startTestServer' useStrictMode (ctx: 'ctx) =
  let testLoggerFactory = TestLoggerFactory.Create()
  let server =
    new TestServer(
      WebHostBuilder()
        .ConfigureServices(fun services ->
          services
            .AddSingleton<ILoggerFactory>(fun _ -> testLoggerFactory :> ILoggerFactory)
            .AddGiraffe()
            .AddRouting()
            .AddJsonApi()
              .GetCtx(fun _ -> ctx)
              |> fun x -> if useStrictMode then x.EnableUnknownFieldStrictMode().EnableUnknownQueryParamStrictMode() else x
              |> fun x -> x.Add()
            |> fun x -> x.AddJsonApi().GetCtx(fun _ -> SecondCtx).Add()
          |> ignore)
        .Configure(fun app ->
          app
            .UseGiraffeErrorHandler(fun _ _ -> returnUnknownError)
            .UseRouting()
            .UseJsonApiEndpoints<'ctx>()
          |> ignore
        )
    )
  server.CreateClient (),
  testLoggerFactory.Sink


let startTestServer ctx = startTestServer' true ctx |> fst

let startTestServerWithLogSink ctx = startTestServer' true ctx



module Request =

  let jsonApiHeaders req =
    req
    |> Request.setHeader (Accept "application/vnd.api+json")
    |> Request.setHeader (ContentType <| ContentType.create("application", "vnd.api+json"))

  let bodySerialized (body: 'a) req =
    req |> Request.bodyString (JsonConvert.SerializeObject body)

  let private get' useStrictMode ctx path =
    let testClient = startTestServer' useStrictMode ctx |> fst
    Request.createWithClient testClient Get (Uri("http://example.com" + path))
    |> jsonApiHeaders

  let private post' useStrictMode ctx path =
    let testClient = startTestServer' useStrictMode  ctx |> fst
    Request.createWithClient testClient Post (Uri("http://example.com" + path))
    |> jsonApiHeaders

  let private patch' useStrictMode ctx path =
    let testClient = startTestServer' useStrictMode  ctx |> fst
    Request.createWithClient testClient Patch (Uri("http://example.com" + path))
    |> jsonApiHeaders

  let private delete' useStrictMode ctx path =
    let testClient = startTestServer' useStrictMode  ctx |> fst
    Request.createWithClient testClient Delete (Uri("http://example.com" + path))
    |> jsonApiHeaders

  let get ctx path = get' true ctx path
  let post ctx path = post' true ctx path
  let patch ctx path = patch' true ctx path
  let delete ctx path = delete' true ctx path

  let getWithoutStrictMode ctx path = get' false ctx path
  let postWithoutStrictMode ctx path = post' false ctx path
  let patchWithoutStrictMode ctx path = patch' false ctx path
  let deleteWithoutStrictMode ctx path = delete' false ctx path


/// Fails the test if the response is not 2XX. If failed, prints the response and the
/// response body.
let testSuccessStatusCode (resp: Response) =
  let statusOk = resp.statusCode >= 200 && resp.statusCode < 300
  if not statusOk then
    let body =
      try resp |> Response.readBodyAsString |> run
      with ex -> $"[Failed to get response body with exception: %s{ex.GetType().Name}: %s{ex.Message}]"
    failtest $"Expected success response code, but got %i{resp.statusCode}. Message body:\n\n%s{body}\n\nComplete response:\n\n%A{resp}"


/// Fails the test if the response is 2XX. If failed, prints the response and the
/// response body.
let testErrorStatusCode (resp: Response) =
  let statusOk = resp.statusCode >= 200 && resp.statusCode < 300
  if statusOk then
    let body =
      try resp |> Response.readBodyAsString |> run
      with ex -> $"[Failed to get response body with exception: %s{ex.GetType().Name}: %s{ex.Message}]"
    failtest $"Expected error response code, but got %i{resp.statusCode}. Message body:\n\n%s{body}\n\nComplete response:\n\n%A{resp}"


/// Fails the test if the response is the specified code. If failed, prints the response and the
/// response body.
let testStatusCode code (resp: Response) =
  if resp.statusCode <> code then
    let body =
      try resp |> Response.readBodyAsString |> run
      with ex -> $"[Failed to get response body with exception: %s{ex.GetType().Name}: %s{ex.Message}]"
    failtest $"Expected response code %i{code}, but got %i{resp.statusCode}. Message body:\n\n%s{body}\n\nComplete response:\n\n%A{resp}"
