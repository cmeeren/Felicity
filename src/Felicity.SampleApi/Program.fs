namespace Felicity.SampleApp

open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Serilog

module Program =

  [<EntryPoint>]
  let main args =
    Host
      .CreateDefaultBuilder(args)
      .ConfigureWebHostDefaults(fun builder ->
        builder
          .CaptureStartupErrors(true)
          .UseSerilog(Setup.setupLogger)
          .UseStartup<Startup>()
        |> ignore
      )
      .Build()
      .Run()
    0
