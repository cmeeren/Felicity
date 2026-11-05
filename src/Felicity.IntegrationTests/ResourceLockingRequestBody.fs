module ``Resource locking and request body``

open System
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading
open System.Threading.Tasks
open Expecto
open Giraffe
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Felicity


let private lockTimeout = TimeSpan.FromSeconds 2.


type Ctx = {
    CreationLock: SemaphoreSlim
    BeforeLookup: unit -> Task
}


type Item = Item


module Item =

    let define = Define<Ctx, Item, string>()
    let resId = define.Id.Simple(fun _ -> "1")

    let resDef =
        define
            .Resource("item", resId)
            .CollectionName("items")
            .Lock(lockTimeout)
            .CustomResourceCreationLock(fun (ctx: Ctx) ->
                task {
                    let! acquired = ctx.CreationLock.WaitAsync lockTimeout

                    return
                        if acquired then
                            Some
                                { new IDisposable with
                                    member _.Dispose() = ctx.CreationLock.Release() |> ignore
                                }
                        else
                            None
                }
            )

    let lookup =
        define.Operation.LookupTask(fun (ctx: Ctx) (_: string) ->
            task {
                do! ctx.BeforeLookup()
                return Some Item
            }
        )

    let get = define.Operation.GetResource()
    let post = define.Operation.Post(fun () -> Item).AfterCreate(ignore)
    let patch = define.Operation.Patch().AfterUpdate(ignore)

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
            .Add(fun (_: string list) item -> item)
            .Remove(fun (_: string list) item -> item)
            .AfterModifySelf(ignore)


module Unlocked =

    let define = Define<Ctx, Item, string>()
    let resId = define.Id.Simple(fun _ -> "1")
    let resDef = define.Resource("unlocked", resId).CollectionName("unlocked")
    let lookup = define.Operation.Lookup(fun _ -> None)
    let get = define.Operation.GetResource()
    let patch = define.Operation.Patch().AfterUpdate(ignore)

    let child =
        define.Relationship
            .ToOneNullable(resDef)
            .Get(fun _ -> None)
            .Set(fun (_: string option) item -> item)
            .AfterModifySelf(ignore)


/// Sends the first part of the body, then waits for the gate before sending the rest.
type private GatedContent(firstPart: string, rest: string, gate: Task) as this =
    inherit HttpContent()

    do this.Headers.ContentType <- MediaTypeHeaderValue("application/vnd.api+json")

    override _.SerializeToStreamAsync(stream: Stream, _: TransportContext) : Task =
        task {
            let firstBytes = Encoding.UTF8.GetBytes firstPart
            do! stream.WriteAsync(firstBytes, 0, firstBytes.Length)
            do! stream.FlushAsync()
            do! gate
            let restBytes = Encoding.UTF8.GetBytes rest
            do! stream.WriteAsync(restBytes, 0, restBytes.Length)
        }

    override _.TryComputeLength(length: byref<int64>) =
        length <- 0L
        false


let private createServerWith (bodyReadStarted: TaskCompletionSource) beforeLookup =
    let creationLock = new SemaphoreSlim(1)

    new TestServer(
        WebHostBuilder()
            .ConfigureServices(fun services ->
                services
                    .AddGiraffe()
                    .AddRouting()
                    .AddJsonApi()
                    .GetCtx(fun _ -> {
                        CreationLock = creationLock
                        BeforeLookup = beforeLookup
                    })
                    .ConfigureRequestBodyReader(fun readBody ctx ->
                        bodyReadStarted.TrySetResult() |> ignore
                        readBody ctx
                    )
                    .Add()
                |> ignore
            )
            .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<Ctx>() |> ignore)
    )


let private createServer bodyReadStarted =
    createServerWith bodyReadStarted (fun () -> Task.CompletedTask)


let private createRequest (method: HttpMethod) (path: string) (content: HttpContent) =
    let request = new HttpRequestMessage(method, path)
    request.Headers.Accept.ParseAdd("application/vnd.api+json")
    request.Content <- content
    request


let private createJsonApiRequest method path (body: string) =
    let content = new StringContent(body, Encoding.UTF8, "application/vnd.api+json")
    content.Headers.ContentType.CharSet <- null
    createRequest method path content


let private expectSuccess (response: HttpResponseMessage) message =
    task {
        let! body = response.Content.ReadAsStringAsync()
        Expect.isTrue response.IsSuccessStatusCode $"%s{message}: %i{int response.StatusCode} %s{body}"
    }


let private waitFor (timeout: TimeSpan) (t: Task) message =
    task {
        let! completed = Task.WhenAny(t, Task.Delay timeout)

        if completed <> t then
            failtest message
    }


let private expectSecondWriteNotBlockedByFirstBody (method: HttpMethod) (path: string) (body: string) =
    task {
        let bodyReadStarted = TaskCompletionSource()
        let gate = TaskCompletionSource()
        use server = createServer bodyReadStarted
        use client = server.CreateClient()

        let splitAt = body.Length / 2

        use slowRequest =
            createRequest method path (new GatedContent(body[.. splitAt - 1], body[splitAt..], gate.Task))

        let slowResponse = client.SendAsync(slowRequest)

        try
            do!
                waitFor
                    (TimeSpan.FromSeconds 10.)
                    bodyReadStarted.Task
                    "The first request did not start reading its body"

            use fastRequest = createJsonApiRequest method path body
            let fastResponse = client.SendAsync(fastRequest)

            do!
                waitFor
                    (TimeSpan.FromSeconds 10.)
                    fastResponse
                    "The second request did not complete while the first request's body was incomplete"

            use! fastResponse = fastResponse
            do! expectSuccess fastResponse "The second request should not wait for the first request's body"
        finally
            gate.TrySetResult() |> ignore

        use! slowResponse = slowResponse
        do! expectSuccess slowResponse "The first request should succeed once its body is complete"
    }


let private expectLookupInsideLock (method: HttpMethod) (path: string) (body: string) =
    task {
        let lookupStarted = TaskCompletionSource()
        let lookupGate = TaskCompletionSource()
        let lookupCount = ref 0

        let beforeLookup () =
            if Interlocked.Increment(&lookupCount.contents) = 1 then
                lookupStarted.TrySetResult() |> ignore
                lookupGate.Task
            else
                Task.CompletedTask

        use server = createServerWith (TaskCompletionSource()) beforeLookup
        use client = server.CreateClient()
        use firstRequest = createJsonApiRequest method path body
        let firstResponse = client.SendAsync(firstRequest)

        try
            do! waitFor (TimeSpan.FromSeconds 10.) lookupStarted.Task "The first request did not start its lookup"
            use secondRequest = createJsonApiRequest method path body
            let secondResponse = client.SendAsync(secondRequest)

            do!
                waitFor
                    (TimeSpan.FromSeconds 10.)
                    secondResponse
                    "The second request did not complete while the first request's lookup was blocked"

            use! secondResponse = secondResponse
            let! secondBody = secondResponse.Content.ReadAsStringAsync()

            Expect.equal
                secondResponse.StatusCode
                HttpStatusCode.ServiceUnavailable
                $"The second request should time out waiting for the lock: %s{secondBody}"
        finally
            lookupGate.TrySetResult() |> ignore

        use! firstResponse = firstResponse
        do! expectSuccess firstResponse "The first request should succeed once its lookup completes"
    }


[<Tests>]
let tests =
    testList "Resource locking and request body" [
        for method, path, body in
            [
                HttpMethod.Post, "/items", """{"data":{"type":"item"}}"""
                HttpMethod.Patch, "/items/1", """{"data":{"type":"item","id":"1"}}"""
                HttpMethod.Patch, "/items/1/relationships/child", """{"data":null}"""
                HttpMethod.Patch, "/items/1/relationships/children", """{"data":[]}"""
                HttpMethod.Post, "/items/1/relationships/children", """{"data":[]}"""
                HttpMethod.Delete, "/items/1/relationships/children", """{"data":[]}"""
            ] do
            testCaseAsync $"%O{method} %s{path} does not hold the lock while receiving the request body"
            <| async { do! expectSecondWriteNotBlockedByFirstBody method path body |> Async.AwaitTask }

        for method, path, body in
            [
                HttpMethod.Patch, "/items/1", """{"data":{"type":"item","id":"1"}}"""
                HttpMethod.Patch, "/items/1/relationships/child", """{"data":null}"""
                HttpMethod.Patch, "/items/1/relationships/children", """{"data":[]}"""
                HttpMethod.Post, "/items/1/relationships/children", """{"data":[]}"""
                HttpMethod.Delete, "/items/1/relationships/children", """{"data":[]}"""
            ] do
            testCaseAsync $"%O{method} %s{path} looks up the resource while holding the lock"
            <| async { do! expectLookupInsideLock method path body |> Async.AwaitTask }

        for path, body in
            [
                "/unlocked/1", """{"data":{"type":"unlocked","id":"1"}}"""
                "/unlocked/1/relationships/child", """{"data":null}"""
            ] do
            testCaseAsync $"PATCH %s{path} without locks looks up the resource before reading the request body"
            <| async {
                let bodyReadStarted = TaskCompletionSource()
                use server = createServer bodyReadStarted
                use client = server.CreateClient()
                use request = createJsonApiRequest HttpMethod.Patch path body
                use! response = client.SendAsync(request) |> Async.AwaitTask
                Expect.equal response.StatusCode HttpStatusCode.NotFound "The resource should not be found"
                Expect.isFalse bodyReadStarted.Task.IsCompleted "The request body should not be read"
            }
    ]
