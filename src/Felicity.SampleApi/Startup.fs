namespace Felicity.SampleApp

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Serilog
open Serilog.Events
open Giraffe
open Felicity
open JsonApi
open Routes


[<AutoOpen>]
module ApplicationBuilderExtensions =

  open Microsoft.AspNetCore.StaticFiles

  type IApplicationBuilder with
    /// Enables static file serving using the specified mappings between subpath
    /// suffixes and content types.
    member this.UseStaticFiles(pathSuffixContentTypes: #seq<string * string>) =
      this.UseStaticFiles(
        StaticFileOptions(
          ContentTypeProvider =
            { new IContentTypeProvider with
                member __.TryGetContentType(subpath, contentType) =
                  let mimeType =
                    pathSuffixContentTypes
                    |> Seq.tryPick (fun (suffix, mimeType) ->
                        if subpath.EndsWith suffix then Some mimeType else None
                    )
                  match mimeType with
                  | Some mt ->
                      contentType <- mt
                      true
                  | None -> false
            }
        )
      )


module Setup =

  let setupLogger hostingContext (loggerConfiguration: LoggerConfiguration) =
    loggerConfiguration
      .MinimumLevel.Override("Microsoft", LogEventLevel.Information)
      .MinimumLevel.Override("Giraffe", LogEventLevel.Information)
      .Enrich.FromLogContext()
      .WriteTo.Console()
      |> ignore


type Startup() =

  member _.ConfigureServices(services: IServiceCollection) : unit =
    services
      .AddGiraffe()
      .AddJsonApi()
        .BaseUrl("http://localhost:5000")
        .GetCtxAsyncRes(Context.getCtx)
        .Add()
    |> ignore

  member _.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) : unit =
    app
      .UseGiraffeErrorHandler(fun ex _ ->
        Log.Error(ex, "Unhandled exception while executing request")
        // returnUnknownError will return a simple, generic 500 error saying that an
        // unknown error occurred. You can also use returnErrorDocument which accepts a
        // list of custom errors to return
        returnUnknownError
      )
      .UseDefaultFiles()
      .UseStaticFiles([
        (".html", "text/html")
        (".png", "image/png")
        (".yaml", "application/x-yaml")
      ])
      .UseGiraffe(mainHandler)
