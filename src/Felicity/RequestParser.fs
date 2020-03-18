namespace Felicity


open Errors


[<AutoOpen>]
module private RequestParserOperators =

  let inline (<!>) f x = AsyncResult.map f x

  let inline (<*>) f x = AsyncResult.apply f x


type RequestParser<'ctx, 'a> = internal {
  includedTypeAndId: (ResourceTypeName * ResourceId) option
  consumedFields: Set<ConsumedFieldName>
  consumedQueryParams: Set<ConsumedQueryParamName>
  parse: 'ctx -> Request -> Async<Result<'a, Error list>>
  ctx: 'ctx
  request: Request
  prohibited: ProhibitedRequestGetter list
} with

  static member internal Create(consumedFields, consumedQueryParams, includedTypeAndId, ctx: 'ctx, req: Request, parse: 'ctx -> Request -> Async<Result<'a, Error list>>) : RequestParser<'ctx, 'a> =
    {
      includedTypeAndId = includedTypeAndId
      consumedFields = consumedFields
      consumedQueryParams = consumedQueryParams
      parse = parse
      ctx = ctx
      request = req
      prohibited = []
    }

  member internal this.ParseWithConsumed () : Async<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'a, Error list>> =
    async {
      let prohibitedErrs =
        this.prohibited
        |> List.collect (fun p -> p.GetErrors(this.request, this.includedTypeAndId))
        |> List.rev

      if prohibitedErrs.IsEmpty then
        return!
          this.parse this.ctx this.request
          |> AsyncResult.map (fun x -> this.consumedFields, this.consumedQueryParams, x)
      else return Error prohibitedErrs
    }

  member this.Parse () : Async<Result<'a, Error list>> =
    this.ParseWithConsumed () |> AsyncResult.map (fun (_, _, x) -> x)

  member private this.MarkAsConsumed(getter: RequestGetter<'ctx, 'b>) =
    { this with
        consumedFields = match getter.FieldName with None -> this.consumedFields | Some n -> this.consumedFields.Add n
        consumedQueryParams = match getter.QueryParamName with None -> this.consumedQueryParams | Some n -> this.consumedQueryParams.Add n
    }

  member private this.MarkAsConsumed(getter: OptionalRequestGetter<'ctx, 'b>) =
    { this with
        consumedFields = match getter.FieldName with None -> this.consumedFields | Some n -> this.consumedFields.Add n
        consumedQueryParams = match getter.QueryParamName with None -> this.consumedQueryParams | Some n -> this.consumedQueryParams.Add n
    }

  member private this.MarkAsConsumed(getter: ProhibitedRequestGetter) =
    { this with
        consumedFields = match getter.FieldName with None -> this.consumedFields | Some n -> this.consumedFields.Add n
        consumedQueryParams = match getter.QueryParamName with None -> this.consumedQueryParams | Some n -> this.consumedQueryParams.Add n
    }

  member private this.AddAsyncRes (set: 'ctx -> Request -> 'b -> 'a -> Async<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    { this with
        parse =
          fun ctx req ->
            async {
              let! existingRes = this.parse ctx req
              let! newRes = getter.Get(ctx, req, this.includedTypeAndId)
              match existingRes, newRes with
              | Error errs1, Error errs2 -> return Error (errs1 @ errs2)
              | Error errs, Ok _ -> return Error errs
              | Ok _, Error errs -> return Error errs
              | Ok existing, Ok newOpt ->
                  match newOpt with
                  | None -> return Ok existing
                  | Some new' -> return! set ctx req new' existing
            }
    }.MarkAsConsumed(getter)

  member this.AddAsyncRes (set: 'b -> 'a -> Async<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddAsyncRes((fun _ _ b a -> set b a), getter)

  member this.AddAsyncRes (set: 'b -> 'c -> 'a -> Async<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this
      .AddAsyncRes((fun ctx req b a -> async {
          match! getC.Get(ctx, req, this.includedTypeAndId) with
          | Error errs -> return Error errs
          | Ok c -> return! set b c a
        }),
        getter)
      .MarkAsConsumed(getC)

  member this.AddAsync (set: 'b -> 'a -> Async<'a>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddAsyncRes ((fun b a -> set b a |> Async.map Ok), getter)

  member this.AddAsync (set: 'b -> 'c -> 'a -> Async<'a>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddAsyncRes ((fun b c a -> set b c a |> Async.map Ok), getter, getC)

  member this.AddRes (set: 'b -> 'c -> 'a -> Result<'a, Error list>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddAsyncRes ((fun b c a -> set b c a |> async.Return), getter, getC)

  member this.AddRes (set: 'b -> 'a -> Result<'a, Error list>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddAsyncRes ((fun b a -> set b a |> async.Return), getter)

  member this.Add (set: 'b -> 'a -> 'a, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddAsyncRes ((fun b a -> set b a |> Ok |> async.Return), getter)

  member this.Add (set: 'b -> 'c -> 'a -> 'a, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddAsyncRes ((fun b c a -> set b c a |> Ok |> async.Return), getter, getC)

  member this.RequireType(typeName: string) =
    { this with
        parse = fun ctx req ->
          match req.Document.Value with
          | Error errs -> Error errs |> async.Return
          | Ok None -> Error [reqParserMissingData ""] |> async.Return
          | Ok (Some { data = None }) -> Error [reqParserMissingData "/data"] |> async.Return
          | Ok (Some { data = Some { ``type`` = t } }) when t <> typeName -> Error [reqParserInvalidType typeName t "/data/type"] |> async.Return
          | Ok (Some { data = Some _ }) -> this.parse ctx req
    }

  member this.Prohibit (getter: ProhibitedRequestGetter) =
    { this with
        prohibited = getter :: this.prohibited
    }.MarkAsConsumed(getter)

  member this.Map (f: 'a -> 'b) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> AsyncResult.map f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindRes (f: 'a -> Result<'b, Error list>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> AsyncResult.bindResult f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindAsync (f: 'a -> Async<'b>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> AsyncResult.bind (f >> Async.map Ok)
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindAsyncRes (f: 'a -> Async<Result<'b, Error list>>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> AsyncResult.bind f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }



type RequestParserHelper<'ctx> internal (ctx: 'ctx, req: Request, ?includedTypeAndId) =

  member _.GetRequired(param: RequestGetter<'ctx, 'a>) : Async<Result<'a, Error list>> =
    RequestParser<'ctx, 'a>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).Parse()

  member _.GetOptional(param: OptionalRequestGetter<'ctx, 'a>) : Async<Result<'a option, Error list>> =
    RequestParser<'ctx, 'a option>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).Parse()

  // Arity 0

  member _.ForAsyncRes (create: Async<Result<'a, Error list>>) =
    RequestParser<'ctx, 'a>.Create (Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> create)

  member this.ForAsync (create: Async<'a>) =
    this.ForAsyncRes (create |> Async.map Ok)

  member this.ForRes (value: Result<'a, Error list>) =
    this.ForAsyncRes (value |> async.Return)

  member this.For (value: 'a) =
    this.ForAsyncRes (value |> Ok |> async.Return)

  // Arity 1

  member _.ForAsyncRes (create: 'p1 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>) =
    let consumedFields = [| p1.FieldName |] |> Array.choose id |> Set.ofArray
    let consumedQueryParams = [| p1.QueryParamName |] |> Array.choose id |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) |> AsyncResult.bind id)

  member this.ForAsync (create: 'p1 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForAsyncRes ((fun p1 -> create p1 |> Async.map Ok), p1)

  member this.ForRes (create: 'p1 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForAsyncRes ((fun p1 -> create p1 |> async.Return), p1)

  member this.For (create: 'p1 -> 'a, p1: RequestGetter<'ctx, 'p1>) =
    this.ForAsyncRes ((fun p1 -> create p1 |> Ok |> async.Return), p1)

  // Arity 2

  member _.ForAsyncRes (create: 'p1 -> 'p2 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    let consumedFields = [| p1.FieldName; p2.FieldName |] |> Array.choose id |> Set.ofArray
    let consumedQueryParams = [| p1.QueryParamName; p2.QueryParamName |] |> Array.choose id |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) |> AsyncResult.bind id)

  member this.ForAsync (create: 'p1 -> 'p2 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForAsyncRes ((fun p1 p2 -> create p1 p2 |> Async.map Ok), p1, p2)

  member this.ForRes (create: 'p1 -> 'p2 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForAsyncRes ((fun p1 p2 -> create p1 p2 |> async.Return), p1, p2)

  member this.For (create: 'p1 -> 'p2 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForAsyncRes ((fun p1 p2 -> create p1 p2 |> Ok |> async.Return), p1, p2)

  // Arity 3

  member _.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    let consumedFields =
      [| p1.FieldName; p2.FieldName; p3.FieldName |]
      |> Array.choose id
      |> Set.ofArray
    let consumedQueryParams =
      [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName|]
      |> Array.choose id
      |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) |> AsyncResult.bind id)

  member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForAsyncRes ((fun p1 p2 p3 -> create p1 p2 p3 |> Async.map Ok), p1, p2, p3)

  member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForAsyncRes ((fun p1 p2 p3 -> create p1 p2 p3 |> async.Return), p1, p2, p3)

  member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForAsyncRes ((fun p1 p2 p3 -> create p1 p2 p3 |> Ok |> async.Return), p1, p2, p3)

  // Arity 4

  member _.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    let consumedFields =
      [| p1.FieldName; p2.FieldName; p3.FieldName; p4.FieldName |]
      |> Array.choose id
      |> Set.ofArray
    let consumedQueryParams =
      [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName; p4.QueryParamName |]
      |> Array.choose id
      |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) <*> p4.Get(c, r, includedTypeAndId) |> AsyncResult.bind id)

  member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForAsyncRes ((fun p1 p2 p3 p4 -> create p1 p2 p3 p4 |> Async.map Ok), p1, p2, p3, p4)

  member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForAsyncRes ((fun p1 p2 p3 p4 -> create p1 p2 p3 p4 |> async.Return), p1, p2, p3, p4)

  member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForAsyncRes ((fun p1 p2 p3 p4 -> create p1 p2 p3 p4 |> Ok |> async.Return), p1, p2, p3, p4)

  // TODO: Higher arities
