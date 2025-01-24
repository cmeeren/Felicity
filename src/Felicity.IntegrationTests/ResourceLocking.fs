module ResourceLocking

open System
open System.Threading
open System.Collections.Concurrent
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Hopac
open Felicity


// Only test with one type of operation; assume locking for all operations work the same


type Ctx = Ctx of int ref


module A =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("a", resId).CollectionName("as")
    let lookup = define.Operation.Lookup(Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module B =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("b", resId).CollectionName("bs").Lock()
    let lookup = define.Operation.Lookup(Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module C =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)

    let b = define.Relationship.ToOne(B.resDef)

    let resDef =
        define
            .Resource("c", resId)
            .CollectionName("cs")
            .LockOtherForResourceCreation(B.resDef, b)
            .LockOtherForModification(B.resDef, id)

    let lookup = define.Operation.Lookup(Some)

    let get = define.Operation.GetResource()

    let post =
        define.Operation
            .Post(fun (Ctx i) parser ->
                i := !i + 1
                parser.For(id, b)
            )
            .AfterCreate(ignore)

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module C2 =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)

    let bNullable = define.Relationship.ToOneNullable(B.resDef)

    let resDef =
        define
            .Resource("c2", resId)
            .CollectionName("c2s")
            .LockOtherForResourceCreation(B.resDef, bNullable)

    let post =
        define.Operation
            .Post(fun (Ctx i) parser ->
                i := !i + 1
                parser.For((fun _ -> ""), bNullable)
            )
            .AfterCreate(ignore)


module D =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)

    let resDef =
        define
            .Resource("d", resId)
            .CollectionName("ds")
            .Lock(TimeSpan.FromMilliseconds 10)

    let lookup = define.Operation.Lookup(Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun ctx parser responder _ ->
                async {
                    do! Async.Sleep 10000
                    return setStatusCode 200 |> Ok
                }
            )


type ResourceId =
    | ResourceId of string

    static member value(ResourceId x) = x


let customGlobalLock =
    let semaphore = new SemaphoreSlim(1)

    fun () ->
        async {
            let! success = semaphore.WaitAsync(TimeSpan.FromSeconds 5L) |> Async.AwaitTask

            if success then
                return
                    Some
                        { new IDisposable with
                            member _.Dispose() = semaphore.Release() |> ignore
                        }
            else
                return None
        }


let customIdLock =
    let semaphores = ConcurrentDictionary<string, SemaphoreSlim>()

    fun (ResourceId id) ->
        async {
            let sem =
                lock semaphores (fun () -> semaphores.GetOrAdd(id, (fun _ -> new SemaphoreSlim(1))))

            do! sem.WaitAsync() |> Async.AwaitTask

            return
                Some
                    { new IDisposable with
                        member _.Dispose() = sem.Release() |> ignore
                    }
        }


let customIdLockTimeout =
    let semaphore = new SemaphoreSlim(1)

    fun (_id: string) ->
        async {
            let! success = semaphore.WaitAsync(TimeSpan.FromMilliseconds 10) |> Async.AwaitTask

            if success then
                return
                    Some
                        { new IDisposable with
                            member _.Dispose() = semaphore.Release() |> ignore
                        }
            else
                return None
        }


module E =

    let define = Define<Ctx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let resDef =
        define.Resource("e", resId).CollectionName("es").CustomLock(customIdLock)

    let lookup = define.Operation.Lookup(ResourceId.value >> Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )



module F =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)

    let e = define.Relationship.ToOne(E.resDef)

    let resDef =
        define
            .Resource("f", resId)
            .CollectionName("fs")
            .LockOtherForResourceCreation(E.resDef, e)
            .LockOtherForModification(E.resDef, ResourceId)

    let lookup = define.Operation.Lookup(Some)

    let get = define.Operation.GetResource()

    let post =
        define.Operation
            .Post(fun (Ctx i) parser ->
                i := !i + 1
                parser.For(ResourceId.value, e)
            )
            .AfterCreate(ignore)

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module G =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)

    let resDef =
        define.Resource("g", resId).CollectionName("gs").CustomLock(customIdLockTimeout)

    let lookup = define.Operation.Lookup(Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun ctx parser responder _ ->
                async {
                    do! Async.Sleep 10000
                    return setStatusCode 200 |> Ok
                }
            )



type MultiLockResult =
    | Success of ms: int
    | Timeout of ms: int
    | Exn of ms: int


type MultiLockState = {
    Result: MultiLockResult
    mutable LockTaken: bool
    mutable IsLocked: bool
}

module MultiLockState =

    let success ms = {
        Result = Success ms
        LockTaken = false
        IsLocked = false
    }

    let timeout ms = {
        Result = Timeout ms
        LockTaken = false
        IsLocked = false
    }

    let exn ms = {
        Result = Exn ms
        LockTaken = false
        IsLocked = false
    }


type MultiLockCtx = MultiLockCtx of MultiLockState * MultiLockState * MultiLockState * int ref



let multiLock (state: MultiLockState) (semaphores: ConcurrentDictionary<string, SemaphoreSlim>) =
    fun (ResourceId id) ->
        async {
            let sem =
                lock semaphores (fun () -> semaphores.GetOrAdd(id, (fun _ -> new SemaphoreSlim(1))))

            match state.Result with
            | Success waitMs ->
                do! Async.Sleep waitMs
                do! sem.WaitAsync() |> Async.AwaitTask
                state.LockTaken <- true
                state.IsLocked <- true

                return
                    Some
                        { new IDisposable with
                            member _.Dispose() =
                                sem.Release() |> ignore
                                state.IsLocked <- false
                        }
            | Timeout waitMs ->
                do! Async.Sleep waitMs
                return None
            | Exn waitMs ->
                do! Async.Sleep waitMs
                return failwith "test lock error"
        }



let multiLock1Semaphores = ConcurrentDictionary<string, SemaphoreSlim>()
let multiLock2Semaphores = ConcurrentDictionary<string, SemaphoreSlim>()
let multiLock3Semaphores = ConcurrentDictionary<string, SemaphoreSlim>()

let multiLock1 (MultiLockCtx(x, _, _, _)) = multiLock x multiLock1Semaphores
let multiLock2 (MultiLockCtx(_, x, _, _)) = multiLock x multiLock2Semaphores
let multiLock3 (MultiLockCtx(_, _, x, _)) = multiLock x multiLock3Semaphores


module H =

    let define = Define<MultiLockCtx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let resDef =
        define
            .Resource("h", resId)
            .CollectionName("hs")
            .CustomLock(multiLock1)
            .CustomLock(multiLock2)
            .CustomLock(multiLock3)
            .MultiLockTotalTimeout(TimeSpan.FromSeconds 1L)

    let lookup = define.Operation.Lookup(ResourceId.value >> Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (MultiLockCtx(_, _, _, i)) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module I =

    let define = Define<Ctx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let resDef =
        define
            .Resource("i", resId)
            .CollectionName("is")
            .CustomResourceCreationLock(fun () -> customIdLock (ResourceId ""))

    let lookup = define.Operation.Lookup(ResourceId.value >> Some)

    let get = define.Operation.GetResource()

    let post =
        define.Operation
            .Post(fun (Ctx i) ->
                i := !i + 1
                ""
            )
            .AfterCreate(ignore)

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module J =

    let define = Define<Ctx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let resDef =
        define
            .Resource("j", resId)
            .CollectionName("js")
            .CustomLock(customIdLock)
            .CustomResourceCreationLock(fun () ->
                failwith<IDisposable option> "CustomResourceCreationLock should not be called"
            )


module K =

    let define = Define<Ctx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let resDef =
        define
            .Resource("k", resId)
            .CollectionName("ks")
            .CustomLock(fun _ -> failwith<IDisposable option> "CustomLock should not be called")
            .CustomResourceCreationLock(fun () ->
                failwith<IDisposable option> "CustomResourceCreationLock should not be called"
            )


module L =

    let define = Define<Ctx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let j = define.Relationship.ToOne(J.resDef)

    let resDef =
        define
            .Resource("l", resId)
            .CollectionName("ls")
            .LockOtherForResourceCreation(J.resDef, j)
            .LockOtherForModification(K.resDef, id)

    let post =
        define.Operation
            .Post(fun _ parser -> parser.For((fun _ -> ""), j))
            .AfterCreate(ignore)


module M =

    let define = Define<Ctx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let k = define.Relationship.ToOne(K.resDef)

    let resDef =
        define
            .Resource("m", resId)
            .CollectionName("ms")
            .LockOtherForResourceCreation(K.resDef, k)
            .LockOtherForModification(J.resDef, id)

    let lookup = define.Operation.Lookup(ResourceId.value >> Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module N =

    let define = Define<Ctx, string, ResourceId>()

    let resId =
        define.Id.ParsedRes(ResourceId.value, (fun _ -> Error "Invalid ID"), ResourceId)

    let k = define.Relationship.ToOne(K.resDef)

    let resDef = define.Resource("n", resId).CollectionName("ns").Lock()

    let lookup = define.Operation.Lookup(ResourceId.value >> Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module O =

    let define = Define<Ctx, string, ResourceId>()

    let resId =
        define.Id.ParsedRes(ResourceId.value, (fun _ -> Error "Invalid ID"), ResourceId)

    let k = define.Relationship.ToOne(K.resDef)

    let resDef =
        define
            .Resource("o", resId)
            .CollectionName("os")
            .CustomLock(fun _ -> failwith<IDisposable option> "CustomLock should not be called")

    let lookup = define.Operation.Lookup(ResourceId.value >> Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module P =

    let define = Define<Ctx, string, ResourceId>()

    let resId =
        define.Id.ParsedRes(ResourceId.value, (fun _ -> Error "Invalid ID"), ResourceId)

    let resDef =
        define
            .Resource("p", resId)
            .CollectionName("ps")
            .LockOtherForModification(K.resDef, id)

    let lookup = define.Operation.Lookup(ResourceId.value >> Some)

    let get = define.Operation.GetResource()

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .PostAsync(fun (Ctx i) parser responder _ ->
                i := !i + 1
                setStatusCode 200 |> Ok |> async.Return
            )


module Q =

    let define = Define<Ctx, string, ResourceId>()
    let resId = define.Id.Parsed(ResourceId.value, ResourceId, ResourceId)

    let k = define.Relationship.ToOneNullable(K.resDef)

    let resDef =
        define
            .Resource("q", resId)
            .CollectionName("qs")
            .LockOtherForResourceCreation(K.resDef, k)

    let post =
        define.Operation
            .Post(fun _ parser -> parser.For((fun _ -> ""), k))
            .AfterCreate(ignore)



[<PTests>] // TODO: These tests often fail on CI (and some occasionally locally), find out why or make them less flaky
let tests =
    testSequenced
    <| testList "Resource locking" [

        testJob "Unlocked resources are not thread-safe" {
            let i = ref 0
            let ctx = Ctx i

            do!
                Request.post ctx "/as/ignoredId/customOp"
                |> getResponse
                |> Array.replicate 1000
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i < 1000 @>
        }

        testJob "Locked resources are thread-safe" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                Request.createWithClient testClient Post (Uri("http://example.com/bs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse
                |> Array.replicate 1000
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob "Cross-locked resources are collectively thread-safe when IDs match" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/bs/someId/customOp"))
                    Request.createWithClient testClient Post (Uri("http://example.com/cs/someId/customOp"))
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 500
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob "Cross-locked resources are not collectively thread-safe when IDs don't match" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/bs/id1/customOp"))
                    Request.createWithClient testClient Post (Uri("http://example.com/cs/id2/customOp"))
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 500
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i < 1000 @>
        }

        testJob "Cross-locked resources are collectively thread-safe when using POST collection" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/bs/someId/customOp"))

                    Request.createWithClient testClient Post (Uri("http://example.com/cs"))
                    |> Request.bodySerialized {|
                        data = {|
                            ``type`` = "c"
                            relationships = {|
                                b = {|
                                    data = {| ``type`` = "b"; id = "someId" |}
                                |}
                            |}
                        |}
                    |}
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 500
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob
            "Cross-locked resources are collectively thread-safe when using POST collection with nullable relationship" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/c2s"))
                    |> Request.bodySerialized {|
                        data = {|
                            ``type`` = "c2"
                            relationships = {|
                                bNullable = {|
                                    data = {| ``type`` = "b"; id = "someId" |}
                                |}
                            |}
                        |}
                    |}
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 1000
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob
            "Cross-locked resources are not collectively thread-safe when using POST collection with nullable relationship set to null" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/c2s"))
                    |> Request.bodySerialized {|
                        data = {|
                            ``type`` = "c2"
                            relationships = {| bNullable = {| data = null |} |}
                        |}
                    |}
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 1000
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i < 1000 @>
        }

        testJob "Locked resources return 503 if lock times out" {
            let testClient = startTestServer (Ctx(ref 0))

            let getTask () =
                Request.createWithClient testClient Post (Uri("http://example.com/ds/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            do! getTask () |> Job.toAsync |> Async.StartChild |> Async.Ignore
            do! timeOutMillis 100 // give the first Task a chance to lock
            let! secondResp = getTask ()
            secondResp |> testStatusCode 503
            test <@ secondResp.headers.ContainsKey LastModified = false @>
            let! json = secondResp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "503" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Timed out waiting for the completion of other operations on the requested resource"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Custom-locked resources are thread-safe" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                Request.createWithClient testClient Post (Uri("http://example.com/es/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse
                |> Array.replicate 1000
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob "Custom-cross-locked resources are collectively thread-safe when IDs match" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/es/someId/customOp"))
                    Request.createWithClient testClient Post (Uri("http://example.com/fs/someId/customOp"))
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 500
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob "Custom-cross-locked resources are not collectively thread-safe when IDs don't match" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/es/id1/customOp"))
                    Request.createWithClient testClient Post (Uri("http://example.com/fs/id2/customOp"))
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 500
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i < 1000 @>
        }

        testJob "Custom-cross-locked resources are collectively thread-safe when using POST collection" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                [|
                    Request.createWithClient testClient Post (Uri("http://example.com/es/someId/customOp"))

                    Request.createWithClient testClient Post (Uri("http://example.com/fs"))
                    |> Request.bodySerialized {|
                        data = {|
                            ``type`` = "f"
                            relationships = {|
                                e = {|
                                    data = {| ``type`` = "e"; id = "someId" |}
                                |}
                            |}
                        |}
                    |}
                |]
                |> Array.map (Request.jsonApiHeaders >> getResponse)
                |> Array.replicate 500
                |> Array.collect id
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testAsync "Custom-locked resources return 503 if lock times out" {
            let testClient = startTestServer (Ctx(ref 0))

            let getJob () =
                Request.createWithClient testClient Post (Uri("http://example.com/gs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse
                |> Alt.toAsync

            getJob () |> Async.Ignore<Response> |> Async.Start
            do! Async.Sleep 100
            let! secondResp = getJob ()
            secondResp |> testStatusCode 503
            test <@ secondResp.headers.ContainsKey LastModified = false @>
            let! json = secondResp |> Response.readBodyAsString |> Job.toAsync
            test <@ json |> getPath "errors[0].status" = "503" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Timed out waiting for the completion of other operations on the requested resource"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Multi-locked resources are thread-safe" {
            let i = ref 0

            let ctx =
                MultiLockCtx(MultiLockState.success 0, MultiLockState.success 0, MultiLockState.success 0, i)

            let testClient = startTestServer ctx

            do!
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse
                |> Array.replicate 1000
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob "Multi-locked resources returns error if any lock times out - 1" {
            let ctx =
                MultiLockCtx(MultiLockState.timeout 0, MultiLockState.success 0, MultiLockState.success 0, ref 0)

            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 503
            let! json = resp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "503" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Timed out waiting for the completion of other operations on the requested resource"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Multi-locked resources returns error if any lock times out - 2" {
            let ctx =
                MultiLockCtx(MultiLockState.success 0, MultiLockState.timeout 0, MultiLockState.success 0, ref 0)

            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 503
            let! json = resp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "503" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Timed out waiting for the completion of other operations on the requested resource"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Multi-locked resources returns error if any lock times out - 3" {
            let ctx =
                MultiLockCtx(MultiLockState.success 0, MultiLockState.success 0, MultiLockState.timeout 0, ref 0)

            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 503
            let! json = resp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "503" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Timed out waiting for the completion of other operations on the requested resource"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Multi-locked resources returns error if any lock throws - 1" {
            let ctx =
                MultiLockCtx(MultiLockState.exn 0, MultiLockState.success 0, MultiLockState.success 0, ref 0)

            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 500
            let! json = resp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "500" @>
            test <@ json |> getPath "errors[0].detail" = "An unknown error has occurred" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Multi-locked resources returns error if any lock throws - 2" {
            let ctx =
                MultiLockCtx(MultiLockState.success 0, MultiLockState.exn 0, MultiLockState.success 0, ref 0)

            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 500
            let! json = resp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "500" @>
            test <@ json |> getPath "errors[0].detail" = "An unknown error has occurred" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Multi-locked resources returns error if any lock throws - 3" {
            let ctx =
                MultiLockCtx(MultiLockState.success 0, MultiLockState.success 0, MultiLockState.exn 0, ref 0)

            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 500
            let! json = resp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "500" @>
            test <@ json |> getPath "errors[0].detail" = "An unknown error has occurred" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob
            "Multi-locked resources take locks in order, and dispose all existing locks and does not take existing locks after the first timeout - 1" {
            let state1 = MultiLockState.success 0
            let state2 = MultiLockState.timeout 0
            let state3 = MultiLockState.success 0
            let ctx = MultiLockCtx(state1, state2, state3, ref 0)
            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 503

            test <@ state1.LockTaken = true @>
            test <@ state1.IsLocked = false @>
            test <@ state2.LockTaken = false @>
            test <@ state2.IsLocked = false @>
            test <@ state3.LockTaken = false @>
            test <@ state3.IsLocked = false @>
        }

        testJob
            "Multi-locked resources take locks in order, dispose all existing locks and does not take existing locks after the first timeout - 2" {
            let state1 = MultiLockState.success 0
            let state2 = MultiLockState.success 0
            let state3 = MultiLockState.timeout 0
            let ctx = MultiLockCtx(state1, state2, state3, ref 0)
            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 503

            test <@ state1.LockTaken = true @>
            test <@ state1.IsLocked = false @>
            test <@ state2.LockTaken = true @>
            test <@ state2.IsLocked = false @>
            test <@ state3.LockTaken = false @>
            test <@ state3.IsLocked = false @>
        }

        testJob
            "Multi-locked resources take locks in order, dispose all existing locks and does not take existing locks after the first exception - 1" {
            let state1 = MultiLockState.success 0
            let state2 = MultiLockState.exn 0
            let state3 = MultiLockState.success 0
            let ctx = MultiLockCtx(state1, state2, state3, ref 0)
            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 500

            test <@ state1.LockTaken = true @>
            test <@ state1.IsLocked = false @>
            test <@ state2.LockTaken = false @>
            test <@ state2.IsLocked = false @>
            test <@ state3.LockTaken = false @>
            test <@ state3.IsLocked = false @>
        }

        testJob
            "Multi-locked resources take locks in order, dispose all existing locks and does not take existing locks after the first exception - 2" {
            let state1 = MultiLockState.success 0
            let state2 = MultiLockState.success 0
            let state3 = MultiLockState.exn 0
            let ctx = MultiLockCtx(state1, state2, state3, ref 0)
            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 500

            test <@ state1.LockTaken = true @>
            test <@ state1.IsLocked = false @>
            test <@ state2.LockTaken = true @>
            test <@ state2.IsLocked = false @>
            test <@ state3.LockTaken = false @>
            test <@ state3.IsLocked = false @>
        }

        testJob "Multi-locked resources, after the total timeout, give up without waiting for in-progress locks" {
            let state1 = MultiLockState.success 500
            let state2 = MultiLockState.success 3000
            let state3 = MultiLockState.success 0
            let ctx = MultiLockCtx(state1, state2, state3, ref 0)
            let testClient = startTestServer ctx

            let sw = Diagnostics.Stopwatch.StartNew()

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            sw.Stop()

            resp |> testStatusCode 503

            test <@ sw.ElapsedMilliseconds >= 1000L @>
            test <@ sw.ElapsedMilliseconds < 1500L @>
        }

        testJob
            "Multi-locked resources, after the total timeout, return error, release existing locks, and do not take additional locks" {
            let state1 = MultiLockState.success 500
            let state2 = MultiLockState.success 1500
            let state3 = MultiLockState.success 0
            let ctx = MultiLockCtx(state1, state2, state3, ref 0)
            let testClient = startTestServer ctx

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/hs/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            // Wait so that all in-progress locks are taken and released
            do! Async.Sleep 2000

            resp |> testStatusCode 503
            let! json = resp |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "503" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Timed out waiting for the completion of other operations on the requested resource"
                @>

            test <@ json |> hasNoPath "errors[1]" @>

            test <@ state1.LockTaken = true @>
            test <@ state1.IsLocked = false @>
            test <@ state2.LockTaken = true @>
            test <@ state2.IsLocked = false @>
            test <@ state3.LockTaken = false @>
            test <@ state3.IsLocked = false @>
        }

        testJob "POST collection is thread-safe with CustomResourceCreationLock" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                Request.createWithClient testClient Post (Uri("http://example.com/is"))
                |> Request.bodySerialized {| data = {| ``type`` = "i" |} |}
                |> Request.jsonApiHeaders
                |> getResponse
                |> Array.replicate 1000
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i = 1000 @>
        }

        testJob "Non-creation operations are not thread-safe with CustomResourceCreationLock" {
            let i = ref 0
            let ctx = Ctx i
            let testClient = startTestServer ctx

            do!
                Request.createWithClient testClient Post (Uri("http://example.com/is/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse
                |> Array.replicate 1000
                |> Array.map Job.toAsync
                |> Async.Parallel
                |> Async.Ignore<Response[]>

            test <@ !i > 0 @>
            test <@ !i < 1000 @>
        }

        testJob
            "Should not lock using LockOtherForModification when creating resource, and should only use modification locks in LockOtherForResourceCreation" {
            let testClient = startTestServer (Ctx(ref 0))

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/ls"))
                |> Request.jsonApiHeaders
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "l"
                        relationships = {|
                            j = {|
                                data = {| ``type`` = "j"; id = "someId" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            resp |> testSuccessStatusCode
        }

        testJob
            "Should not lock using LockOtherForResourceCreation when modifying (not creating) resource, and should only use modification locks in LockOtherForResourceCreation" {
            let testClient = startTestServer (Ctx(ref 0))

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/ms/someId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testSuccessStatusCode
        }

        testJob "Returns correct error when resource ID fails to parse when using Lock" {
            let testClient = startTestServer (Ctx(ref 0))

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/ns/invalidId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 404
        }

        testJob "Returns correct error when resource ID fails to parse when using CustomLock" {
            let testClient = startTestServer (Ctx(ref 0))

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/os/invalidId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 404
        }

        testJob "Returns correct error when resource ID fails to parse when using LockOtherForModification" {
            let testClient = startTestServer (Ctx(ref 0))

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/ps/invalidId/customOp"))
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testStatusCode 404
        }

        testJob "Does not lock other resource when using LockOtherForResourceCreation and relationship is null" {
            let testClient = startTestServer (Ctx(ref 0))

            let! resp =
                Request.createWithClient testClient Post (Uri("http://example.com/qs"))
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "q"
                        relationships = {| k = {| data = null |} |}
                    |}
                |}
                |> Request.jsonApiHeaders
                |> getResponse

            resp |> testSuccessStatusCode
        }

    ]
