[<AutoOpen>]
module internal Utils

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open System.Text.Json.Serialization
open FSharp.Control.Tasks
open Hopac


// TODO: Further optimization of reflection etc.?

// TODO: Refactor
//   - can ASP.NET Core DI be used other places?
//   - taskResult/asyncResult CE can improve some things

// TODO: Strict mode validation, see JsonApiContext.fs in FSharp.JsonApi

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
let isBoxedNull x =
  x |> box |> isNull

/// Indicates if the Skippable-wrapped value is null. Returns false if Skip.
/// Boxes the inner value before checking so even types that cannot normally
/// be null can be checked. Note that this will return true if the inner value
/// is None.
let isIncludedNull = function
  | Include x -> isBoxedNull x
  | _ -> false

/// Matches null values. Boxes the value before checking so even types that
/// cannot normally be null can be checked. Note that this will return true
/// if the value is None.
[<return: Struct>]
let (|BoxedNull|_|) x =
  if isBoxedNull x then ValueSome () else ValueNone


module Result =

  let requireSome errIfNone = function
    | Some x -> Ok x
    | None -> Error errIfNone

  /// Combines two results, returning the Error case if at least one of the
  /// inputs is Error.
  let combine res1 res2 =
    match res1, res2 with
    | Ok l1, Ok l2 -> Ok (l1 @ l2)
    | Ok _, Error errs | Error errs, Ok _ -> Error errs
    | Error errs1, Error errs2 -> Error (errs1 @ errs2)

  let defaultValue valueIfError = function
    | Ok ok -> ok
    | Error _ -> valueIfError


[<AutoOpen>]
module ExceptionExtensions =

  open System.Runtime.ExceptionServices


  type Exception with
    member this.Reraise () =
      (ExceptionDispatchInfo.Capture this).Throw ()
      Unchecked.defaultof<_>


module Async =

  let map f asnc =
    async {
      let! x = asnc
      return f x
    }

  let bind f asnc =
    async {
      let! x = asnc
      return! f x
    }


module Job =

  let inline lift2 f =
    fun a b -> f a b |> Job.result

  let inline lift3 f =
    fun a b c -> f a b c |> Job.result

  let inline lift4 f =
    fun a b c d -> f a b c d |> Job.result

  let inline lift5 f =
    fun a b c d e -> f a b c d e |> Job.result

  let inline liftFunc (f: Func<_,_>) =
    Func<_,_>(fun a -> f.Invoke a |> Job.result)

  let inline liftFunc2 (f: Func<_,_,_>) =
    Func<_,_,_>(fun a b -> f.Invoke(a, b) |> Job.result)

  let inline liftFunc3 (f: Func<_,_,_,_>) =
    Func<_,_,_,_>(fun a b c -> f.Invoke(a, b, c) |> Job.result)

  let inline liftFunc4 (f: Func<_,_,_,_,_>) =
    Func<_,_,_,_,_>(fun a b c d -> f.Invoke(a, b, c, d) |> Job.result)

  let inline liftFunc5 (f: Func<_,_,_,_,_,_>) =
    Func<_,_,_,_,_,_>(fun a b c d e -> f.Invoke(a, b, c, d, e) |> Job.result)

  let inline liftAsync f =
    fun a -> f a |> Job.fromAsync

  let inline liftAsync2 f =
    fun a b -> f a b |> Job.fromAsync

  let inline liftAsync3 f =
    fun a b c -> f a b c |> Job.fromAsync

  let inline liftAsync4 f =
    fun a b c d -> f a b c d |> Job.fromAsync

  let inline liftAsync5 f =
    fun a b c d e -> f a b c d e |> Job.fromAsync

  let inline liftAsyncFunc (f: Func<_,_>) =
    Func<_,_>(fun a -> f.Invoke a |> Job.fromAsync)

  let inline liftAsyncFunc2 (f: Func<_,_,_>) =
    Func<_,_,_>(fun a b -> f.Invoke(a, b) |> Job.fromAsync)

  let inline liftAsyncFunc3 (f: Func<_,_,_,_>) =
    Func<_,_,_,_>(fun a b c -> f.Invoke(a, b, c) |> Job.fromAsync)

  let inline liftAsyncFunc4 (f: Func<_,_,_,_,_>) =
    Func<_,_,_,_,_>(fun a b c d -> f.Invoke(a, b, c, d) |> Job.fromAsync)

  let inline liftAsyncFunc5 (f: Func<_,_,_,_,_,_>) =
    Func<_,_,_,_,_,_>(fun a b c d e -> f.Invoke(a, b, c, d, e) |> Job.fromAsync)

  let inline liftTask2 f =
    fun a b -> f a b |> Job.fromTask


  let startAsTask (xJ: Job<'x>): Task<'x> =
    let tcs = new TaskCompletionSource<'x>()
    job {
      try
        let! x = xJ
        tcs.SetResult x
      with ex -> tcs.SetException ex
    }
    |> start
    tcs.Task



module AsyncResult =

  let map f = Job.map (Result.map f)
  
  let mapError f = Job.map (Result.mapError f)
  
  let requireSome errIfNone = Job.map (Result.bind (Result.requireSome errIfNone))
  
  let bind f asncRes =
    async {
      match! asncRes with
      | Error err -> return Error err
      | Ok x -> return! f x
    }

  let bindResult f = Job.map (Result.bind f)

  let apply (fAsyncRes: Async<Result<'a->'b, 'c list>>) (xAsyncRes: Async<Result<'a, 'c list>>) : Async<Result<'b, 'c list>> =
    async {
      let! f = fAsyncRes
      let! x = xAsyncRes
      return
        match f, x with
        | Ok f, Ok x -> Ok (f x)
        | Ok _, Error errs | Error errs, Ok _ -> Error errs
        | Error errs1, Error errs2 -> Error (errs1 @ errs2)
    }



module JobResult =

  let inline lift f =
    fun a -> f a |> Ok |> Job.result

  let inline lift2 f =
    fun a b -> f a b |> Ok |> Job.result

  let inline lift3 f =
    fun a b c -> f a b c |> Ok |> Job.result

  let inline lift4 f =
    fun a b c d -> f a b c d |> Ok |> Job.result

  let inline lift5 f =
    fun a b c d e -> f a b c d e |> Ok |> Job.result

  let inline liftJob f =
    fun a -> f a |> Job.map Ok

  let inline liftJob2 f =
    fun a b -> f a b |> Job.map Ok

  let inline liftJob3 f =
    fun a b c -> f a b c |> Job.map Ok

  let inline liftJob4 f =
    fun a b c d -> f a b c d |> Job.map Ok

  let inline liftJob5 f =
    fun a b c d e -> f a b c d e |> Job.map Ok

  let inline liftFunc (f: Func<_,_>) =
    Func<_,_>(fun a -> f.Invoke a |> Ok |> Job.result)

  let inline liftFunc2 (f: Func<_,_,_>) =
    Func<_,_,_>(fun a b -> f.Invoke(a, b) |> Ok |> Job.result)

  let inline liftFunc3 (f: Func<_,_,_,_>) =
    Func<_,_,_,_>(fun a b c -> f.Invoke(a, b, c) |> Ok |> Job.result)

  let inline liftFunc4 (f: Func<_,_,_,_,_>) =
    Func<_,_,_,_,_>(fun a b c d -> f.Invoke(a, b, c, d) |> Ok |> Job.result)

  let inline liftFunc5 (f: Func<_,_,_,_,_,_>) =
    Func<_,_,_,_,_,_>(fun a b c d e -> f.Invoke(a, b, c, d, e) |> Ok |> Job.result)

  let map f = Job.map (Result.map f)
  
  let mapError f = Job.map (Result.mapError f)
  
  let requireSome errIfNone = Job.map (Result.bind (Result.requireSome errIfNone))
  
  let bind (f: _ -> Job<Result<_,_>>) (jobRes: Job<Result<_,_>>) =
    job {
      match! jobRes with
      | Error err -> return Error err
      | Ok x -> return! f x
    }

  let bindResult f = Job.map (Result.bind f)

  let apply (fJobRes: Job<Result<'a->'b, 'c list>>) (xJobRes: Job<Result<'a, 'c list>>) : Job<Result<'b, 'c list>> =
    job {
      let! f = fJobRes
      let! x = xJobRes
      return
        match f, x with
        | Ok f, Ok x -> Ok (f x)
        | Ok _, Error errs | Error errs, Ok _ -> Error errs
        | Error errs1, Error errs2 -> Error (errs1 @ errs2)
    }




module Option =

  let traverseResult f opt =
    match opt with
    | None -> Ok None
    | Some v -> f v |> Result.map Some

  let traverseJob f opt =
    match opt with
    | None -> Job.result None
    | Some v -> f v |> Job.map Some

  let traverseJobResult f opt =
    match opt with
    | None -> Job.result (Ok None)
    | Some v -> f v |> JobResult.map Some

  let fromResult = function
    | Ok x -> Some x
    | Error _ -> None



module List =

  let traverseResultA f list =
      (list, Ok [])
      ||> List.foldBack (fun t state ->
        match f t, state with
        | Ok x, Ok xs -> Ok (x :: xs)
        | Ok _, Error errs | Error errs, Ok _ -> Error errs
        | Error newErrs, Error existingErrs -> Error (newErrs @ existingErrs)
      )

  let traverseJobResultA f list =
    job {
      let! results = list |> List.map f |> Job.conCollect
      return
        (Seq.toArray results, Ok [])
        ||> Array.foldBack (fun t state ->
          match t, state with
          | Ok x, Ok xs -> Ok (x :: xs)
          | Ok _, Error errs | Error errs, Ok _ -> Error errs
          | Error newErrs, Error existingErrs -> Error (newErrs @ existingErrs)
        )
    }



module Array =

  /// Executes the function on each item on the array and returns the input
  /// array.
  let tee f xs =
    xs |> Array.iter f
    xs

  let sequenceResultA (xs: Result<'a, 'b list> []) : Result<'a [], 'b list> =
    let mutable errs = []
    let resArray = Array.zeroCreate xs.Length
    xs |> Array.iteri (fun i x ->
      match x with
      | Ok x -> resArray[i] <- x
      | Error newErrs -> errs <- errs @ newErrs
    )
    if errs.IsEmpty then Ok resArray else Error errs


module Set =


  let intersects (set1: Set<_>) (set2: Set<_>) =
    set1 |> Seq.exists set2.Contains



module String =

  /// Splits a string by the given separator.
  let split (separator: string) (str: string) =
    str.Split([| separator |], StringSplitOptions.None) |> List.ofArray

  /// Joins a sequence of strings using the specified separator.
  let join (separator: string) (strings: seq<string>) =
    String.Join(separator, strings)



module Exception =

  let rec getInnerMsg (ex: Exception) =
    if isNull ex.InnerException then ex.Message else getInnerMsg ex.InnerException



/// A semaphore with FIFO semantics (operations are guaranteed to be executed
/// in the order they started waiting on the semaphore).
type SemaphoreQueue() =
  let semaphore = new SemaphoreSlim 1
  let queue = ConcurrentQueue<TaskCompletionSource<bool>>()

  let waitAsync (timeout: TimeSpan) =
    let tcs = TaskCompletionSource<bool>()
    queue.Enqueue(tcs)
    semaphore.WaitAsync(timeout).ContinueWith(fun (t: Task<bool>) ->
      match queue.TryDequeue() with
      | true, popped -> popped.SetResult(t.Result)
      | false, _ -> ()
    ) |> ignore
    tcs.Task

  let release () =
    semaphore.Release()

  member _.IsIdle =
    semaphore.CurrentCount = 0
    && queue.IsEmpty

  /// Queues a lock on the SemaphoreQueue. The lock is released when the returned object
  /// is disposed. ALWAYS remember to dispose, otherwise the order can not be edited. Use
  /// the "use" keyword to ensure disposal. Returns Error if the lock times out.
  member _.Lock(timeout) =
    task {
      let! locked = waitAsync timeout
      if not locked then return None
      else return Some { new IDisposable with member _.Dispose() = release() |> ignore }
    }

  interface IDisposable with
    member _.Dispose () =
      semaphore.Dispose ()



type SemaphoreQueueFactory<'ctx>() =

  let queues = ConcurrentDictionary<string * string, SemaphoreQueue>()

  let cleanIdle () =
    let idle = queues |> Seq.filter (fun kvp -> kvp.Value.IsIdle) |> Seq.toList
    if not idle.IsEmpty then
      lock queues (fun () ->
        for kvp in idle do
          if kvp.Value.IsIdle then
            (kvp.Value :> IDisposable).Dispose ()
            queues.TryRemove kvp.Key |> ignore
      )

  do
    let timer =
      new Timers.Timer(
        TimeSpan.FromMinutes(5.).TotalMilliseconds,
        AutoReset = true,
        Enabled = true)
    timer.Elapsed.Add (ignore >> cleanIdle)
    timer.Start()

  member _.GetFor(lockIdPrefix, resourceId) =
    match queues.TryGetValue ((lockIdPrefix, resourceId)) with
    | true, q -> q
    | false, _ ->
        lock queues (fun () ->
          queues.GetOrAdd((lockIdPrefix, resourceId), fun _ -> new SemaphoreQueue())
        )
