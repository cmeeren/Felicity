namespace Felicity

open System
open System.Security.Cryptography
open System.Text
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers
open Giraffe
open FSharp.Control.Tasks
open Errors


[<AutoOpen>]
module HttpHandlers =


  let internal jsonApiWithETag<'ctx> (x: obj) : HttpHandler =
    fun (_next : HttpFunc) (ctx : HttpContext) ->
      task {
        let serializer = ctx.GetService<Serializer<'ctx>> ()
        let bytes =
          serializer.Serialize x
          |> Encoding.UTF8.GetBytes
        let eTag =
          bytes
          |> SHA1.HashData
          |> Convert.ToBase64String
          |> fun s -> s.TrimEnd('=')
          |> EntityTagHeaderValue.FromString false

        let returnNotModified =
          // Check status codes first so we don't set ETag for non-success responses
          ctx.Response.StatusCode >= 200
          && ctx.Response.StatusCode < 300
          // Then validate preconditions, which sets ETag
          && ctx.ValidatePreconditions(Some eTag, None) = ResourceNotModified
          // Method is checked last since ETag should be set regardless of method
          && (ctx.Request.Method = "GET" || ctx.Request.Method = "HEAD")

        if returnNotModified then return ctx.NotModifiedResponse ()
        else
          ctx.Response.Headers[HeaderNames.ContentType] <- Constants.jsonApiMediaType |> StringValues
          ctx.Response.Headers[HeaderNames.ContentLength] <- bytes.Length |> string |> StringValues
          if ctx.Request.Method <> HttpMethods.Head then
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
          return Some ctx
      }


  let internal handleErrors (errs: Error list) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
      let errs = errs |> List.distinctBy (fun e -> { e with id = Skip })

      let statusCode =
        errs
        |> List.choose (fun e -> e.status |> Skippable.toOption)
        |> List.choose (fun s -> match Int32.TryParse s with true, i -> Some i | _ -> None)
        |> function
            | [] -> None
            | xs ->
                xs
                |> List.countBy id
                |> List.maxBy snd
                |> fst
                |> Some

      let headers =
        errs
        |> List.collect (fun e -> e.headers)

      for (name, values) in headers |> List.groupBy fst do
        ctx.Response.Headers.Add(name, StringValues (values |> List.map snd |> List.toArray))

      let logger = ctx.GetLogger("Felicity.ErrorHandler")

      for i, err in Seq.indexed errs do
        logger.LogInformation(
          "[Request error {ErrNum}/{NumErrs}, ID {ErrorId}] {Status} {Code}: {Title}: {Detail}",
          i + 1,
          errs.Length,
          err.id |> Skippable.defaultValue "<no id>",
          err.status |> Skippable.defaultValue "<no status>",
          err.code |> Skippable.defaultValue "<no code>",
          err.title |> Skippable.defaultValue "<no title>",
          err.detail |> Skippable.defaultValue "<no detail>")

      let doc =  {
        jsonapi = Skip
        errors = errs
        links = Skip
        meta = Skip
      }

      let statusCode =
        match statusCode with
        | Some code -> code
        | None ->
            logger.LogWarning("None of the errors contained a valid integer status code; arbitrarily choosing status code 400")
            400

      let handler =
        setStatusCode statusCode
        >=> jsonApiWithETag<ErrorSerializerCtx> doc

      handler next ctx


  let returnErrorDocument (errors: Error list) : HttpHandler =
    handleErrors errors


  let returnUnknownError : HttpHandler =
    returnErrorDocument [unknownError ()]
