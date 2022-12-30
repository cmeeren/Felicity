namespace Felicity.SampleApp

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Serilog
open Serilog.Events
open Giraffe
open Giraffe.EndpointRouting
open Felicity
open JsonApi


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
                            member _.TryGetContentType(subpath, contentType) =
                                let mimeType =
                                    pathSuffixContentTypes
                                    |> Seq.tryPick (fun (suffix, mimeType) ->
                                        if subpath.EndsWith suffix then Some mimeType else None)

                                match mimeType with
                                | Some mt ->
                                    contentType <- mt
                                    true
                                | None -> false
                        }
                )
            )


module Setup =

    let setupLogger _hostingContext (loggerConfiguration: LoggerConfiguration) =
        loggerConfiguration
            .MinimumLevel
            .Override("Microsoft", LogEventLevel.Information)
            .MinimumLevel.Override("Giraffe", LogEventLevel.Information)
            .Enrich.FromLogContext()
            .WriteTo.Console()
        |> ignore


module Startup =

    let configureServices (services: IServiceCollection) : unit =
        services
            .AddGiraffe()
            .AddRouting()
            .AddJsonApi()
            .GetCtxAsyncRes(Context.getCtx)
            .Add()
        |> ignore

    let configure (app: IApplicationBuilder) : unit =
        app
            .UseGiraffeErrorHandler(fun ex _ ->
                Log.Error(ex, "Unhandled exception while executing request")
                // returnUnknownError will return a simple, generic 500 error saying that an
                // unknown error occurred. You can also use returnErrorDocument which accepts a
                // list of custom errors to return
                returnUnknownError)
            .UseDefaultFiles()
            .UseStaticFiles(
                [
                    (".html", "text/html")
                    (".png", "image/png")
                    (".yaml", "application/x-yaml")
                ]
            )
            .UseRouting()
            .UseJsonApiEndpoints<Context>(List.map (applyBefore (setHttpHeader "Cache-Control" "no-cache, private")))
            .UseEndpoints(fun e -> e.MapGiraffeEndpoints Endpoints.endpoints)
        |> ignore
