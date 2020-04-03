namespace Felicity


open Hopac
open Errors


[<AutoOpen>]
module private RequestParserOperators =

  let inline (<!>) f x = JobResult.map f x

  let inline (<*>) f x = JobResult.apply f x


type RequestParser<'ctx, 'a> = internal {
  includedTypeAndId: (ResourceTypeName * ResourceId) option
  consumedFields: Set<ConsumedFieldName>
  consumedQueryParams: Set<ConsumedQueryParamName>
  parse: 'ctx -> Request -> Job<Result<'a, Error list>>
  ctx: 'ctx
  request: Request
  prohibited: ProhibitedRequestGetter list
} with

  static member internal Create(consumedFields, consumedQueryParams, includedTypeAndId, ctx: 'ctx, req: Request, parse: 'ctx -> Request -> Job<Result<'a, Error list>>) : RequestParser<'ctx, 'a> =
    {
      includedTypeAndId = includedTypeAndId
      consumedFields = consumedFields
      consumedQueryParams = consumedQueryParams
      parse = parse
      ctx = ctx
      request = req
      prohibited = []
    }

  member internal this.ParseWithConsumed () : Job<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'a, Error list>> =
    job {
      let prohibitedErrs =
        this.prohibited
        |> List.collect (fun p -> p.GetErrors(this.request, this.includedTypeAndId))
        |> List.rev

      if prohibitedErrs.IsEmpty then
        return!
          this.parse this.ctx this.request
          |> JobResult.map (fun x -> this.consumedFields, this.consumedQueryParams, x)
      else return Error prohibitedErrs
    }

  member this.ParseJob () : Job<Result<'a, Error list>> =
    this.ParseWithConsumed () |> JobResult.map (fun (_, _, x) -> x)

  member this.ParseAsync () : Async<Result<'a, Error list>> =
    this.ParseJob () |> Job.toAsync

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

  member private this.AddJobRes (set: 'ctx -> Request -> 'b -> 'a -> Job<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    { this with
        parse =
          fun ctx req ->
            job {
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

  member this.AddJobRes (set: 'b -> 'a -> Job<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddJobRes((fun _ _ b a -> set b a), getter)

  member this.AddAsyncRes (set: 'b -> 'a -> Async<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddJobRes(Job.liftAsync2 set, getter)

  member this.AddJobRes (set: 'b -> 'c -> 'a -> Job<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this
      .AddJobRes((fun ctx req b a ->
        job {
          match! getC.Get(ctx, req, this.includedTypeAndId) with
          | Error errs -> return Error errs
          | Ok c -> return! set b c a
        }),
        getter)
      .MarkAsConsumed(getC)

  member this.AddAsyncRes (set: 'b -> 'c -> 'a -> Async<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddJobRes(Job.liftAsync3 set, getter, getC)

  member this.AddJob (set: 'b -> 'a -> Job<'a>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddJobRes ((fun b a -> set b a |> Job.map Ok), getter)

  member this.AddJob (set: 'b -> 'c -> 'a -> Job<'a>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddJobRes ((fun b c a -> set b c a |> Job.map Ok), getter, getC)

  member this.AddAsync (set: 'b -> 'a -> Async<'a>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddJob (Job.liftAsync2 set, getter)

  member this.AddAsync (set: 'b -> 'c -> 'a -> Async<'a>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddJob (Job.liftAsync3 set, getter, getC)

  member this.AddRes (set: 'b -> 'c -> 'a -> Result<'a, Error list>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddJobRes ((fun b c a -> set b c a |> Job.result), getter, getC)

  member this.AddRes (set: 'b -> 'a -> Result<'a, Error list>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddJobRes ((fun b a -> set b a |> Job.result), getter)

  member this.Add (set: 'b -> 'a -> 'a, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddJobRes ((fun b a -> set b a |> Ok |> Job.result), getter)

  member this.Add (set: 'b -> 'c -> 'a -> 'a, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddJobRes ((fun b c a -> set b c a |> Ok |> Job.result), getter, getC)

  member this.RequireType(typeName: string) =
    { this with
        parse = fun ctx req ->
          match req.Document.Value with
          | Error errs -> Error errs |> Job.result
          | Ok None -> Error [reqParserMissingData ""] |> Job.result
          | Ok (Some { data = None }) -> Error [reqParserMissingData "/data"] |> Job.result
          | Ok (Some { data = Some { ``type`` = t } }) when t <> typeName -> Error [reqParserInvalidType typeName t "/data/type"] |> Job.result
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
      parse = fun ctx req -> this.parse ctx req |> JobResult.map f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindRes (f: 'a -> Result<'b, Error list>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> JobResult.bindResult f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindJob (f: 'a -> Job<'b>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> JobResult.bind (f >> Job.map Ok)
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindAsync (f: 'a -> Async<'b>) : RequestParser<'ctx, 'b> =
    this.BindJob(Job.liftAsync f)

  member this.BindJobRes (f: 'a -> Job<Result<'b, Error list>>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> JobResult.bind f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindAsyncRes (f: 'a -> Async<Result<'b, Error list>>) : RequestParser<'ctx, 'b> =
    this.BindJobRes(Job.liftAsync f)



type RequestParserHelper<'ctx> internal (ctx: 'ctx, req: Request, ?includedTypeAndId) =

  member _.GetRequired(param: RequestGetter<'ctx, 'a>) : Job<Result<'a, Error list>> =
    RequestParser<'ctx, 'a>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).ParseJob()

  member _.GetOptional(param: OptionalRequestGetter<'ctx, 'a>) : Job<Result<'a option, Error list>> =
    RequestParser<'ctx, 'a option>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).ParseJob()

  // Arity 0

  member _.ForJobRes (create: Job<Result<'a, Error list>>) =
    RequestParser<'ctx, 'a>.Create (Set.empty, Set.empty, includedTypeAndId, ctx, req, fun _ _ -> create)

  member _.ForAsyncRes (create: Async<Result<'a, Error list>>) =
    RequestParser<'ctx, 'a>.Create (Set.empty, Set.empty, includedTypeAndId, ctx, req, fun _ _ -> Job.fromAsync create)

  member this.ForJob (create: Job<'a>) =
    this.ForJobRes (create |> Job.map Ok)

  member this.ForAsync (create: Async<'a>) =
    this.ForJob (create |> Job.fromAsync)

  member this.ForRes (value: Result<'a, Error list>) =
    this.ForJobRes (value |> Job.result)

  member this.For (value: 'a) =
    this.ForJobRes (value |> Ok |> Job.result)

  // Arity 1

  member _.ForJobRes (create: 'p1 -> Job<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>) =
    let consumedFields = [| p1.FieldName |] |> Array.choose id |> Set.ofArray
    let consumedQueryParams = [| p1.QueryParamName |] |> Array.choose id |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) |> JobResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForJobRes (Job.liftAsync create, p1)

  member this.ForJob (create: 'p1 -> Job<'a>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForJobRes (JobResult.liftJob create, p1)

  member this.ForAsync (create: 'p1 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForJob (Job.liftAsync create, p1)

  member this.ForRes (create: 'p1 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForJobRes (Job.lift create, p1)

  member this.For (create: 'p1 -> 'a, p1: RequestGetter<'ctx, 'p1>) =
    this.ForJobRes (JobResult.lift create, p1)

  // Arity 2

  member _.ForJobRes (create: 'p1 -> 'p2 -> Job<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    let consumedFields = [| p1.FieldName; p2.FieldName |] |> Array.choose id |> Set.ofArray
    let consumedQueryParams = [| p1.QueryParamName; p2.QueryParamName |] |> Array.choose id |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) |> JobResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> 'p2 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForJobRes (Job.liftAsync2 create, p1, p2)

  member this.ForJob (create: 'p1 -> 'p2 -> Job<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForJobRes (JobResult.liftJob2 create, p1, p2)

  member this.ForAsync (create: 'p1 -> 'p2 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForJob (Job.liftAsync2 create, p1, p2)

  member this.ForRes (create: 'p1 -> 'p2 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForJobRes (Job.lift2 create, p1, p2)

  member this.For (create: 'p1 -> 'p2 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForJobRes (JobResult.lift2 create, p1, p2)

  // Arity 3

  member _.ForJobRes (create: 'p1 -> 'p2 -> 'p3 -> Job<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    let consumedFields =
      [| p1.FieldName; p2.FieldName; p3.FieldName |]
      |> Array.choose id
      |> Set.ofArray
    let consumedQueryParams =
      [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName|]
      |> Array.choose id
      |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) |> JobResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForJobRes (Job.liftAsync3 create, p1, p2, p3)

  member this.ForJob (create: 'p1 -> 'p2 -> 'p3 -> Job<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForJobRes (JobResult.liftJob3 create, p1, p2, p3)

  member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForJob (Job.liftAsync3 create, p1, p2, p3)

  member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForJobRes (Job.lift3 create, p1, p2, p3)

  member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForJobRes (JobResult.lift3 create, p1, p2, p3)

  // Arity 4

  member _.ForJobRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Job<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    let consumedFields =
      [| p1.FieldName; p2.FieldName; p3.FieldName; p4.FieldName |]
      |> Array.choose id
      |> Set.ofArray
    let consumedQueryParams =
      [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName; p4.QueryParamName |]
      |> Array.choose id
      |> Set.ofArray
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) <*> p4.Get(c, r, includedTypeAndId) |> JobResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForJobRes (Job.liftAsync4 create, p1, p2, p3, p4)

  member this.ForJob (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Job<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForJobRes (JobResult.liftJob4 create, p1, p2, p3, p4)

  member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForJob (Job.liftAsync4 create, p1, p2, p3, p4)

  member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForJobRes (Job.lift4 create, p1, p2, p3, p4)

  member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForJobRes (JobResult.lift4 create, p1, p2, p3, p4)

  // Arity 5
  
    member _.ForJobRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Job<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
      let consumedFields =
        [| p1.FieldName; p2.FieldName; p3.FieldName; p4.FieldName; p5.FieldName |]
        |> Array.choose id
        |> Set.ofArray
      let consumedQueryParams =
        [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName; p4.QueryParamName; p5.QueryParamName |]
        |> Array.choose id
        |> Set.ofArray
      RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) <*> p4.Get(c, r, includedTypeAndId) <*> p5.Get(c, r, includedTypeAndId) |> JobResult.bind id)
  
    member this.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
      this.ForJobRes (Job.liftAsync5 create, p1, p2, p3, p4, p5)

    member this.ForJob (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Job<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
      this.ForJobRes (JobResult.liftJob5 create, p1, p2, p3, p4, p5)

    member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
      this.ForJob (Job.liftAsync5 create, p1, p2, p3, p4, p5)
  
    member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
      this.ForJobRes (Job.lift5 create, p1, p2, p3, p4, p5)
  
    member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
      this.ForJobRes (JobResult.lift5 create, p1, p2, p3, p4, p5)

  // TODO: Higher arities
