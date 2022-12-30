namespace Felicity.Benchmarks

open System
open System.Net.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.TestHost
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Felicity
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Configs


type Resource = { Id: string; Related: Resource list }


type Context = { Resources: Resource list }


module Resource =

    let define = Define<Context, Resource, string>()

    let resId = define.Id.Simple(fun r -> r.Id)

    let resDef = define.Resource("resource", resId).CollectionName("resources")

    let a = define.Attribute.SimpleString().Get(fun _ -> "a")
    let b = define.Attribute.SimpleBool().Get(fun _ -> true)
    let c = define.Attribute.SimpleInt().Get(fun _ -> 3)
    let d = define.Attribute.SimpleDateTimeOffset().Get(fun _ -> DateTimeOffset.Now)
    let e = define.Attribute.Nullable.SimpleInt().Get(fun _ -> (None: int option))

    let r1 = define.Relationship.ToOneNullable(resDef).Get(fun _ _ -> None)
    let r2 = define.Relationship.ToOneNullable(resDef).Get(fun _ _ -> None)
    let related = define.Relationship.ToMany(resDef).Get(fun _ r -> r.Related)

    let getColl = define.Operation.GetCollection(fun ctx -> ctx.Resources)

    let lookup =
        define.Operation.Lookup(fun ctx resId -> ctx.Resources |> List.tryFind (fun r -> r.Id = resId))

    let get = define.Operation.GetResource()


[<MemoryDiagnoser>]
type Benchmark() =

    [<DefaultValue; Params(1000)>]
    val mutable NRes: int

    [<DefaultValue; Params(5)>]
    val mutable NInc: int

    [<DefaultValue; Params(true)>]
    val mutable ShareInc: bool

    let mutable context: Context = Unchecked.defaultof<Context>

    let server =
        new TestServer(
            WebHostBuilder()
                .ConfigureServices(fun services ->
                    services.AddGiraffe().AddRouting().AddJsonApi().GetCtx(fun _ -> context).Add()
                    |> ignore)
                .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<Context>() |> ignore)
        )

    let client = server.CreateClient()


    [<GlobalSetup>]
    member this.Setup() =
        let getRelated () =
            List.init this.NInc (fun _ -> {
                Id = Guid.NewGuid().ToString()
                Related = []
            })

        let commonRelated = getRelated ()

        context <- {
            Resources =
                List.init this.NRes (fun _ -> {
                    Id = Guid.NewGuid().ToString()
                    Related = if this.ShareInc then commonRelated else getRelated ()
                })
        }

    [<Benchmark>]
    member _.WithIncluded() =
        let msg =
            new HttpRequestMessage(HttpMethod.Get, "http://example.com/resources?include=related")

        msg.Headers.Accept.ParseAdd "application/vnd.api+json"
        client.SendAsync(msg)

//  [<Benchmark>]
//  member _.WithoutIncluded() =
//    let msg = new HttpRequestMessage(HttpMethod.Get, "http://example.com/resources")
//    msg.Headers.Accept.ParseAdd "application/vnd.api+json"
//    client.SendAsync(msg)


module Program =

    [<EntryPoint>]
    let main _argv =

        // Uncomment for manual run, e.g. profiling
        let manualProfile = false

        if manualProfile then
            let b = Benchmark()
            b.NRes <- 10000
            b.NInc <- 5
            b.ShareInc <- true
            b.Setup()

            while true do
                Console.WriteLine("Press Enter to run")
                Console.ReadLine() |> ignore
                Console.WriteLine("Running...")
                b.WithIncluded().Result |> ignore
                Console.WriteLine("Done")
        else
            BenchmarkRunner.Run<Benchmark>(DefaultConfig.Instance.AddJob(Job.Default.WithGcServer(true)))
            |> ignore

        0
