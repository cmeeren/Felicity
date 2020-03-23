namespace Felicity.Benchmarks

open System
open System.Net.Http
open Microsoft.AspNetCore.TestHost
open Microsoft.AspNetCore.Hosting
open Giraffe
open Felicity
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Configs


type Resource = {
  Id: string
}


type Context = {
  Resources: Resource list
  Related: Resource list
}


module Resource =

  let define = Define<Context, Resource, string>()

  let resId = define.Id.Simple(fun r -> r.Id)

  let resDef = define.Resource("resource", resId).CollectionName("resources")

  let a = define.Attribute.Simple().Get(fun _ -> "a")
  let b = define.Attribute.Simple().Get(fun _ -> true)
  let c = define.Attribute.Simple().Get(fun _ -> 3)
  let d = define.Attribute.Simple().Get(fun _ -> DateTimeOffset.Now)
  let e = define.Attribute.Nullable.Simple().Get(fun _ -> (None: int option))

  let r1 = define.Relationship.ToOneNullable(resDef).Get(fun _ _ -> None)
  let r2 = define.Relationship.ToOneNullable(resDef).Get(fun _ _ -> None)
  let related = define.Relationship.ToMany(resDef).Get(fun ctx _ -> ctx.Related)

  let getColl = define.Operation.GetCollection(fun ctx -> ctx.Resources)


[<MemoryDiagnoser>]
type Benchmark () =

  [<DefaultValue; Params(10, 100, 1000, 10000)>]
  val mutable NRes : int

  [<DefaultValue; Params(1, 5)>]
  val mutable NInc : int

  let mutable context : Context = Unchecked.defaultof<Context>

  let server =
    new TestServer(
      WebHostBuilder()
        .ConfigureServices(fun services ->
          services
            .AddGiraffe()
            .AddJsonApi()
              .BaseUrl("http://example.com")
              .GetCtx(fun _ -> context)
              .Add()
          |> ignore
        )
        .Configure(fun app -> app.UseGiraffe jsonApi<Context>)
    )

  let client = server.CreateClient ()


  [<GlobalSetup>]
  member this.Setup() =
    context <- {
      Resources = List.init this.NRes (fun _ -> { Id = Guid.NewGuid().ToString() })
      Related = List.init this.NInc (fun _ -> { Id = Guid.NewGuid().ToString() })
    }

  [<Benchmark>]
  member _.GetCollection() =
    let msg = new HttpRequestMessage(HttpMethod.Get, "http://example.com/resources?include=related")
    msg.Headers.Accept.ParseAdd "application/vnd.api+json"
    client.SendAsync(msg)


module Program =

  [<EntryPoint>]
  let main argv =

    // Uncomment for manual run, e.g. profiling

    //let b = Benchmark()
    //b.NRes <- 10000
    //b.NInc <- 5
    //b.Setup()
    //while true do
    //  Console.WriteLine("Press Enter to run")
    //  Console.ReadLine() |> ignore
    //  Console.WriteLine("Running...")
    //  b.GetCollection().Result |> ignore
    //  Console.WriteLine("Done")


    // Uncomment for BenchmarkDotNet run

    BenchmarkRunner.Run<Benchmark>(
      DefaultConfig.Instance.With(Job.Default.WithGcServer(true))
    )
    |> ignore

    0
