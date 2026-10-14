module ``Request body reader``

open System.Net.Http
open System.Text
open Expecto
open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Felicity


type Ctx = Ctx
type Item = Item


module Item =

    let define = Define<Ctx, Item, string>()
    let resId = define.Id.Simple(fun _ -> "1")
    let resDef = define.Resource("item", resId).CollectionName("items")
    let lookup = define.Operation.Lookup(fun _ -> Some Item)
    let get = define.Operation.GetResource()
    let post = define.Operation.Post(fun () -> Item).AfterCreate(ignore)

    let child =
        define.Relationship
            .ToOneNullable(resDef)
            .Get(fun _ -> None)
            .Set(fun (_: string option) item -> item)
            .AfterModifySelf(ignore)

    let children =
        define.Relationship
            .ToMany(resDef)
            .Get(fun _ -> [])
            .SetAll(fun (_: string list) item -> item)
            .AfterModifySelf(ignore)


let private createServer configureReader =
    new TestServer(
        WebHostBuilder()
            .ConfigureServices(fun services ->
                services
                    .AddGiraffe()
                    .AddRouting()
                    .AddJsonApi()
                    .GetCtx(fun _ -> Ctx)
                    .ConfigureRequestBodyReader(configureReader "first")
                    .ConfigureRequestBodyReader(configureReader "second")
                    .Add()
                |> ignore
            )
            .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<Ctx>() |> ignore)
    )


[<Tests>]
let tests =
    testList "Request body reader" [
        for method, path, body in
            [
                HttpMethod.Post, "/items", """{"data":{"type":"item"}}"""
                HttpMethod.Patch, "/items/1/relationships/child", """{"data":null}"""
                HttpMethod.Patch, "/items/1/relationships/children", """{"data":[]}"""
            ] do
            testCaseAsync (string method + " " + path)
            <| async {
                let events = ResizeArray<string>()

                let wrap name readBody ctx =
                    task {
                        events.Add(name + " start")

                        try
                            return! readBody ctx
                        finally
                            events.Add(name + " end")
                    }

                use server = createServer wrap
                use client = server.CreateClient()
                use request = new HttpRequestMessage(method, path)
                request.Headers.Accept.ParseAdd("application/vnd.api+json")
                request.Content <- new StringContent(body, Encoding.UTF8, "application/vnd.api+json")
                request.Content.Headers.ContentType.CharSet <- null
                use! response = client.SendAsync(request) |> Async.AwaitTask
                let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
                Expect.isTrue response.IsSuccessStatusCode responseBody

                Expect.sequenceEqual
                    events
                    [ "second start"; "first start"; "first end"; "second end" ]
                    "Wrappers compose around body reading"
            }

        testCaseAsync "GET does not read the request body"
        <| async {
            let wrap _ _ _ =
                failwith "The reader should not be called"

            use server = createServer wrap
            use client = server.CreateClient()
            client.DefaultRequestHeaders.Accept.ParseAdd("application/vnd.api+json")
            use! response = client.GetAsync("/items/1") |> Async.AwaitTask
            Expect.isTrue response.IsSuccessStatusCode "GET should succeed without reading a body"
        }
    ]
