namespace Felicity.SampleApp

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Serilog

module Program =

    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)
        builder.Host.UseSerilog(Setup.setupLogger) |> ignore
        Startup.configureServices builder.Services
        let app = builder.Build()
        Startup.configure app
        app.Run()
        0
