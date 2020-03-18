[<AutoOpen>]
module internal Utils

open System
open System.Text.Json.Serialization


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


let ignoreUnitArray (us: unit []) = ()


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
let (|BoxedNull|_|) x =
  if isBoxedNull x then Some () else None


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

module AsyncResult =

  let map f = Async.map (Result.map f)
  
  let mapError f = Async.map (Result.mapError f)
  
  let requireSome errIfNone = Async.map (Result.bind (Result.requireSome errIfNone))
  
  let bind f asncRes =
    async {
      match! asncRes with
      | Error err -> return Error err
      | Ok x -> return! f x
    }

  let bindResult f = Async.map (Result.bind f)

  let apply (fAsyncRes: Async<Result<('a->'b), 'c list>>) (xAsyncRes: Async<Result<'a, 'c list>>) : Async<Result<'b, 'c list>> =
    async {
      let! f = fAsyncRes
      let! x = xAsyncRes
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

  let traverseAsync f opt =
    match opt with
    | None -> async.Return None
    | Some v -> f v |> Async.map Some

  let traverseAsyncResult f opt =
    match opt with
    | None -> async.Return (Ok None)
    | Some v -> f v |> AsyncResult.map Some



module List =

  let traverseResultA f list =
      (list, Ok [])
      ||> List.foldBack (fun t state ->
        match f t, state with
        | Ok x, Ok xs -> Ok (x :: xs)
        | Ok _, Error errs | Error errs, Ok _ -> Error errs
        | Error newErrs, Error existingErrs -> Error (newErrs @ existingErrs)
      )

  let traverseAsyncResultA f list =
    async {
      let! results = list |> List.map f |> Async.Parallel
      return
        (results, Ok [])
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
      | Ok x -> resArray.[i] <- x
      | Error newErrs -> errs <- errs @ newErrs
    )
    if errs.IsEmpty then Ok resArray else Error errs



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



[<RequireQualifiedAccess>]
module Uri =


  /// Adds a path segment to a URI, adding a slash if needed. Does not change
  /// query parameters or fragment.
  let addSegment (segment: string) (uri: Uri) =
    let b = UriBuilder(uri)
    b.Path <- (b.Path.TrimEnd '/') + "/" + (segment.TrimStart '/')
    b.Uri

  /// Adds several path segments to a URI, adding slashes as needed. Does not
  /// change query parameters or fragment.
  let addSegments (segments: #seq<string>) (uri: Uri) =
    segments |> Seq.fold (fun uri segment -> addSegment segment uri) uri

  /// Adds a name-value pair to a URI's query string. A name may be added multiple
  /// times.
  let addQuery (key: string) (value: string) (uri: Uri) =
    let b = UriBuilder(uri)
    b.Query <-
      b.Query.TrimStart('?')
      |> String.split "&"
      |> List.map (String.split "=")
      |> fun kvs -> kvs @ [[key; value]]
      |> List.map (String.join "=")
      |> String.join "&"
      |> fun s -> if s = "" then s else "?" + s
    b.Uri

  /// Sets a name-value pair in the URI's query string, overwriting any existing
  /// values for that name.
  let setQuery (key: string) (value: string) (uri: Uri) =
    let b = UriBuilder(uri)
    b.Query <-
      b.Query.TrimStart('?')
      |> String.split "&"
      |> List.map (String.split "=")
      |> List.filter (fun kv -> kv |> List.tryItem 0 |> (<>) (Some key))
      |> fun kvs -> kvs @ [[key; value]]
      |> List.map (String.join "=")
      |> String.join "&"
      |> fun s -> if s = "" then s else "?" + s
    b.Uri



[<AutoOpen>]
module UriExtensions =

  type Uri with

    /// Adds a path segment to a URI, adding a slash if needed. Does not change
    /// query parameters or fragment.
    member this.AddSegment segment =
      this |> Uri.addSegment segment

    /// Adds several path segments to a URI, adding slashes as needed. Does not
    /// change query parameters or fragment.
    member this.AddSegments segments =
      this |> Uri.addSegments segments

    /// Adds a name-value pair to a URI's query string. A name may be added
    /// multiple times.
    member this.AddQuery (key, value) =
      this |> Uri.addQuery key value

    /// Sets a name-value pair in the URI's query string, overwriting any
    /// existing values for that name.
    member this.SetQuery (key, value) =
      this |> Uri.setQuery key value
