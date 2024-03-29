﻿[<AutoOpen>]
module internal Utils

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Giraffe


// TODO: Further optimization of reflection etc.?

// TODO: Refactor
//   - can ASP.NET Core DI be used other places?
//   - taskResult/asyncResult CE can improve some things

// TODO: Auto-generate docs somehow?
//   - Concerns about contract-driven dev
//   - Must be able to merge with an existing OpenAPI document with other routes
//   - Must be able to specify descriptions, examples, deprecations (and drafts?) - noisy?
//   - Tag by collection? And use type names for non-collection resources?


let (|TryFindIndexed|_|) predicate seq =
    seq |> Seq.indexed |> Seq.tryFind (fun (_, x) -> predicate x)


/// Indicates if the value is null. Boxes the value before checking so even types
/// that cannot normally be null can be checked. Note that this will return true
/// if the value is None.
let isBoxedNull x = x |> box |> isNull

/// Indicates if the Skippable-wrapped value is null. Returns false if Skip.
/// Boxes the inner value before checking so even types that cannot normally
/// be null can be checked. Note that this will return true if the inner value
/// is None.
let isIncludedNull =
    function
    | Include x -> isBoxedNull x
    | _ -> false

/// Matches null values. Boxes the value before checking so even types that
/// cannot normally be null can be checked. Note that this will return true
/// if the value is None.
[<return: Struct>]
let (|BoxedNull|_|) x =
    if isBoxedNull x then ValueSome() else ValueNone


let flip f a b = f b a


module Result =

    let requireSome errIfNone =
        function
        | Some x -> Ok x
        | None -> Error errIfNone

    /// Combines two results, returning the Error case if at least one of the
    /// inputs is Error.
    let combine res1 res2 =
        match res1, res2 with
        | Ok l1, Ok l2 -> Ok(l1 @ l2)
        | Ok _, Error errs
        | Error errs, Ok _ -> Error errs
        | Error errs1, Error errs2 -> Error(errs1 @ errs2)

    let defaultValue valueIfError =
        function
        | Ok ok -> ok
        | Error _ -> valueIfError


[<AutoOpen>]
module ExceptionExtensions =

    open System.Runtime.ExceptionServices


    type Exception with

        member this.Reraise() =
            (ExceptionDispatchInfo.Capture this).Throw()
            Unchecked.defaultof<_>


module Async =

    let inline map f asnc =
        async {
            let! x = asnc
            return f x
        }

    let inline bind f asnc =
        async {
            let! x = asnc
            return! f x
        }


module Task =


    let inline mapWhenAllIgnore (f: 'a -> #Task) (xs: ICollection<'a>) : Task =
        let tasks = Array.zeroCreate xs.Count
        let mutable i = 0

        for b in xs do
            tasks[i] <- f b :> Task
            i <- i + 1

        Task.WhenAll(tasks)


    let inline mapWhenAllIgnoreWithCount count (f: 'a -> #Task) (xs: seq<'a>) : Task =
        let tasks = Array.zeroCreate count
        let mutable i = 0

        for b in xs do
            tasks[i] <- f b :> Task
            i <- i + 1

        Task.WhenAll(tasks)


    let inline mapWhenAll (f: 'a -> Task<'b>) (xs: ICollection<'a>) : Task<'b[]> =
        let tasks = Array.zeroCreate xs.Count
        let mutable i = 0

        for b in xs do
            tasks[i] <- f b
            i <- i + 1

        Task.WhenAll(tasks)


    let inline mapiWhenAll (f: int -> 'a -> Task<'b>) (xs: ICollection<'a>) : Task<'b[]> =
        let tasks = Array.zeroCreate xs.Count
        let mutable i = 0

        for b in xs do
            tasks[i] <- f i b
            i <- i + 1

        Task.WhenAll(tasks)


    let inline mapWhenAllWithCount count (f: 'a -> Task<'b>) (xs: seq<'a>) : Task<'b[]> =
        let tasks = Array.zeroCreate count
        let mutable i = 0

        for b in xs do
            tasks[i] <- f b
            i <- i + 1

        Task.WhenAll(tasks)

    let inline map f t =
        task {
            let! x = t
            return f x
        }

    let inline ignore<'a> (t: Task<'a>) = t :> Task

    let inline fromAsync x = Async.StartAsTask x

    let inline toAsync x = Async.AwaitTask x

    let inline result x = Task.FromResult x

    let inline lift f = fun a -> f a |> result

    let inline lift2 f = fun a b -> f a b |> result

    let inline lift3 f = fun a b c -> f a b c |> result

    let inline lift4 f = fun a b c d -> f a b c d |> result

    let inline lift5 f = fun a b c d e -> f a b c d e |> result

    let inline liftFunc (f: Func<_, _>) =
        Func<_, _>(fun a -> f.Invoke a |> result)

    let inline liftFunc2 (f: Func<_, _, _>) =
        Func<_, _, _>(fun a b -> f.Invoke(a, b) |> result)

    let inline liftFunc3 (f: Func<_, _, _, _>) =
        Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c) |> result)

    let inline liftFunc4 (f: Func<_, _, _, _, _>) =
        Func<_, _, _, _, _>(fun a b c d -> f.Invoke(a, b, c, d) |> result)

    let inline liftFunc5 (f: Func<_, _, _, _, _, _>) =
        Func<_, _, _, _, _, _>(fun a b c d e -> f.Invoke(a, b, c, d, e) |> result)

    let inline liftAsync f = fun a -> f a |> fromAsync

    let inline liftAsync2 f = fun a b -> f a b |> fromAsync

    let inline liftAsync3 f = fun a b c -> f a b c |> fromAsync

    let inline liftAsync4 f = fun a b c d -> f a b c d |> fromAsync

    let inline liftAsync5 f =
        fun a b c d e -> f a b c d e |> fromAsync

    let inline liftAsyncFunc (f: Func<_, _>) =
        Func<_, _>(fun a -> f.Invoke a |> fromAsync)

    let inline liftAsyncFunc2 (f: Func<_, _, _>) =
        Func<_, _, _>(fun a b -> f.Invoke(a, b) |> fromAsync)

    let inline liftAsyncFunc3 (f: Func<_, _, _, _>) =
        Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c) |> fromAsync)

    let inline liftAsyncFunc4 (f: Func<_, _, _, _, _>) =
        Func<_, _, _, _, _>(fun a b c d -> f.Invoke(a, b, c, d) |> fromAsync)

    let inline liftAsyncFunc5 (f: Func<_, _, _, _, _, _>) =
        Func<_, _, _, _, _, _>(fun a b c d e -> f.Invoke(a, b, c, d, e) |> fromAsync)

    let inline liftTask2 f = fun a b -> f a b



module AsyncResult =

    let inline map f = Task.map (Result.map f)

    let inline mapError f = Task.map (Result.mapError f)

    let inline requireSome errIfNone =
        Task.map (Result.bind (Result.requireSome errIfNone))

    let inline bind f asncRes =
        async {
            match! asncRes with
            | Error err -> return Error err
            | Ok x -> return! f x
        }

    let inline bindResult f = Task.map (Result.bind f)

    let inline apply
        (fAsyncRes: Async<Result<'a -> 'b, 'c list>>)
        (xAsyncRes: Async<Result<'a, 'c list>>)
        : Async<Result<'b, 'c list>> =
        async {
            let! f = fAsyncRes
            let! x = xAsyncRes

            return
                match f, x with
                | Ok f, Ok x -> Ok(f x)
                | Ok _, Error errs
                | Error errs, Ok _ -> Error errs
                | Error errs1, Error errs2 -> Error(errs1 @ errs2)
        }



module TaskResult =

    let inline lift f = fun a -> f a |> Ok |> Task.result

    let inline lift2 f = fun a b -> f a b |> Ok |> Task.result

    let inline lift3 f =
        fun a b c -> f a b c |> Ok |> Task.result

    let inline lift4 f =
        fun a b c d -> f a b c d |> Ok |> Task.result

    let inline lift5 f =
        fun a b c d e -> f a b c d e |> Ok |> Task.result

    let inline liftTask f = fun a -> f a |> Task.map Ok

    let inline liftTask2 f = fun a b -> f a b |> Task.map Ok

    let inline liftTask3 f = fun a b c -> f a b c |> Task.map Ok

    let inline liftTask4 f = fun a b c d -> f a b c d |> Task.map Ok

    let inline liftTask5 f =
        fun a b c d e -> f a b c d e |> Task.map Ok

    let inline liftFunc (f: Func<_, _>) =
        Func<_, _>(fun a -> f.Invoke a |> Ok |> Task.result)

    let inline liftFunc2 (f: Func<_, _, _>) =
        Func<_, _, _>(fun a b -> f.Invoke(a, b) |> Ok |> Task.result)

    let inline liftFunc3 (f: Func<_, _, _, _>) =
        Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c) |> Ok |> Task.result)

    let inline liftFunc4 (f: Func<_, _, _, _, _>) =
        Func<_, _, _, _, _>(fun a b c d -> f.Invoke(a, b, c, d) |> Ok |> Task.result)

    let inline liftFunc5 (f: Func<_, _, _, _, _, _>) =
        Func<_, _, _, _, _, _>(fun a b c d e -> f.Invoke(a, b, c, d, e) |> Ok |> Task.result)

    let inline map f = Task.map (Result.map f)

    let inline mapError f = Task.map (Result.mapError f)

    let inline requireSome errIfNone =
        Task.map (Result.bind (Result.requireSome errIfNone))

    let inline bind (f: _ -> Task<Result<_, _>>) (taskRes: Task<Result<_, _>>) =
        task {
            match! taskRes with
            | Error err -> return Error err
            | Ok x -> return! f x
        }

    let inline bindResult f = Task.map (Result.bind f)

    let inline apply
        (fTaskRes: Task<Result<'a -> 'b, 'c list>>)
        (xTaskRes: Task<Result<'a, 'c list>>)
        : Task<Result<'b, 'c list>> =
        task {
            let! f = fTaskRes
            let! x = xTaskRes

            return
                match f, x with
                | Ok f, Ok x -> Ok(f x)
                | Ok _, Error errs
                | Error errs, Ok _ -> Error errs
                | Error errs1, Error errs2 -> Error(errs1 @ errs2)
        }



module Option =

    let inline traverseResult f opt =
        match opt with
        | None -> Ok None
        | Some v -> f v |> Result.map Some

    let inline traverseTask f opt =
        match opt with
        | None -> Task.result None
        | Some v -> f v |> Task.map Some

    let inline traverseTaskResult f opt =
        match opt with
        | None -> Task.result (Ok None)
        | Some v -> f v |> TaskResult.map Some

    let inline fromResult res =
        match res with
        | Ok x -> Some x
        | Error _ -> None



module List =

    let inline traverseResultA f list =
        (list, Ok [])
        ||> List.foldBack (fun t state ->
            match f t, state with
            | Ok x, Ok xs -> Ok(x :: xs)
            | Ok _, Error errs
            | Error errs, Ok _ -> Error errs
            | Error newErrs, Error existingErrs -> Error(newErrs @ existingErrs)
        )

    let inline traverseTaskResultA (f: _ -> Task<_>) (list: _ list) =
        task {
            let! results = list |> Task.mapWhenAllWithCount list.Length f

            return
                (results, Ok [])
                ||> Array.foldBack (fun t state ->
                    match t, state with
                    | Ok x, Ok xs -> Ok(x :: xs)
                    | Ok _, Error errs
                    | Error errs, Ok _ -> Error errs
                    | Error newErrs, Error existingErrs -> Error(newErrs @ existingErrs)
                )
        }



module Array =

    /// Executes the function on each item on the array and returns the input
    /// array.
    let tee f xs =
        xs |> Array.iter f
        xs

    let traverseResultA f (xs: _[]) =
        let errors = ResizeArray()
        let out = Array.zeroCreate xs.Length

        xs
        |> Array.iteri (fun i x ->
            match f x with
            | Ok x -> out[i] <- x
            | Error errs -> errors.AddRange errs
        )

        if errors.Count > 0 then
            errors |> Seq.toList |> Error
        else
            Ok out

    let traverseResultAIndexed f (xs: _[]) =
        let errors = ResizeArray()
        let out = Array.zeroCreate xs.Length

        xs
        |> Array.iteri (fun i x ->
            match f i x with
            | Ok x -> out[i] <- x
            | Error errs -> errors.AddRange errs
        )

        if errors.Count > 0 then
            errors |> Seq.toList |> Error
        else
            Ok out

    let traverseTaskResultAIndexed (f: _ -> _ -> Task<_>) (xs: _[]) =
        task {
            let! results = Task.mapiWhenAll f xs
            let errors = ResizeArray()
            let out = Array.zeroCreate xs.Length

            results
            |> Seq.iteri (fun i x ->
                match x with
                | Ok x -> out[i] <- x
                | Error errs -> errors.AddRange errs
            )

            return
                if errors.Count > 0 then
                    errors |> Seq.toList |> Error
                else
                    Ok out
        }


module Set =


    let intersects (set1: Set<_>) (set2: Set<_>) = set1 |> Seq.exists set2.Contains



module String =

    /// Splits a string by the given separator.
    let split (separator: string) (str: string) =
        str.Split([| separator |], StringSplitOptions.None) |> List.ofArray

    /// Joins a sequence of strings using the specified separator.
    let join (separator: string) (strings: seq<string>) = String.Join(separator, strings)

    /// Truncates the string using the specified suffix.
    let truncate (suffix: string) maxLength (str: string) =
        if str.Length <= maxLength then
            str
        else
            str.Substring(0, maxLength - suffix.Length) + suffix



/// A semaphore with FIFO semantics (operations are guaranteed to be executed
/// in the order they started waiting on the semaphore).
type SemaphoreQueue() =
    let semaphore = new SemaphoreSlim 1
    let queue = ConcurrentQueue<TaskCompletionSource<bool>>()

    let waitAsync (timeout: TimeSpan) =
        let tcs = TaskCompletionSource<bool>()
        queue.Enqueue(tcs)

        semaphore
            .WaitAsync(timeout)
            .ContinueWith(fun (t: Task<bool>) ->
                match queue.TryDequeue() with
                | true, popped -> popped.SetResult(t.Result)
                | false, _ -> ()
            )
        |> ignore

        tcs.Task

    let release () = semaphore.Release()

    member _.IsIdle = semaphore.CurrentCount = 0 && queue.IsEmpty

    /// Queues a lock on the SemaphoreQueue. The lock is released when the returned object
    /// is disposed. ALWAYS remember to dispose, otherwise the order can not be edited. Use
    /// the "use" keyword to ensure disposal. Returns Error if the lock times out.
    member _.Lock(timeout) =
        task {
            let! locked = waitAsync timeout

            if not locked then
                return None
            else
                return
                    Some
                        { new IDisposable with
                            member _.Dispose() = release () |> ignore
                        }
        }

    interface IDisposable with
        member _.Dispose() = semaphore.Dispose()



type SemaphoreQueueFactory<'ctx>() =

    let queues = ConcurrentDictionary<string * string, SemaphoreQueue>()

    let cleanIdle () =
        let idle = queues |> Seq.filter (fun kvp -> kvp.Value.IsIdle) |> Seq.toList

        if not idle.IsEmpty then
            lock
                queues
                (fun () ->
                    for kvp in idle do
                        if kvp.Value.IsIdle then
                            (kvp.Value :> IDisposable).Dispose()
                            queues.TryRemove kvp.Key |> ignore
                )

    do
        let timer =
            new Timers.Timer(TimeSpan.FromMinutes(5.).TotalMilliseconds, AutoReset = true, Enabled = true)

        timer.Elapsed.Add(ignore >> cleanIdle)
        timer.Start()

    member _.GetFor(lockIdPrefix, resourceId) =
        match queues.TryGetValue((lockIdPrefix, resourceId)) with
        | true, q -> q
        | false, _ ->
            lock queues (fun () -> queues.GetOrAdd((lockIdPrefix, resourceId), (fun _ -> new SemaphoreQueue())))


let internal logFieldTrackerPolymorphicRelTraversalWarning (httpCtx: HttpContext) resType relName =
    let logger = httpCtx.GetLogger("Felicity.FieldTracker")

    logger.LogWarning(
        "Field usage tracker is unable to traverse polymorphic relationship '{RelationshipName}' on resource '{ResourceType}' because its allowed types are unknown. Add a suitable number of calls to AddIdParser in the relationship definition to provide this information.",
        relName,
        resType
    )



type JsonException with

    member this.SafeMessage =
        let baseMsg = "Invalid JSON or incorrect data type"

        let extraInfo =
            [
                if not (String.IsNullOrWhiteSpace this.Path) then
                    "path " + this.Path

                if this.LineNumber.HasValue then
                    "line " + string this.LineNumber.Value

                if this.BytePositionInLine.HasValue then
                    "position " + string this.BytePositionInLine.Value
            ]
            |> String.concat ", "

        if String.IsNullOrEmpty extraInfo then
            baseMsg
        else
            $"{baseMsg} at {extraInfo}"

    member this.SafeMessageForField(fieldName) =
        $"Invalid JSON or incorrect data type in field '%s{fieldName}'"
