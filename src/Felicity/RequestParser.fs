namespace Felicity


open System.Threading.Tasks
open Errors


[<AutoOpen>]
module private RequestParserOperators =

  let inline (<!>) f x = TaskResult.map f x

  let inline (<*>) f x = TaskResult.apply f x


type RequestParser<'ctx, 'a> = internal {
  includedTypeAndId: (ResourceTypeName * ResourceId) option
  consumedFields: Set<ConsumedFieldName>
  consumedQueryParams: Set<ConsumedQueryParamName>
  parse: 'ctx -> Request -> Task<Result<'a, Error list>>
  ctx: 'ctx
  request: Request
  prohibited: ProhibitedRequestGetter list
} with

  static member internal Create(consumedFields, consumedQueryParams, includedTypeAndId, ctx: 'ctx, req: Request, parse: 'ctx -> Request -> Task<Result<'a, Error list>>) : RequestParser<'ctx, 'a> =
    {
      includedTypeAndId = includedTypeAndId
      consumedFields = consumedFields
      consumedQueryParams = consumedQueryParams
      parse = parse
      ctx = ctx
      request = req
      prohibited = []
    }

  member internal this.ParseWithConsumed () : Task<Result<Set<ConsumedFieldName> * Set<ConsumedQueryParamName> * 'a, Error list>> =
    task {
      let prohibitedErrs =
        this.prohibited
        |> List.collect (fun p -> p.GetErrors(this.request, this.includedTypeAndId))
        |> List.rev

      if prohibitedErrs.IsEmpty then
        return!
          this.parse this.ctx this.request
          |> TaskResult.map (fun x -> this.consumedFields, this.consumedQueryParams, x)
      else return Error prohibitedErrs
    }

  member this.ParseTask () : Task<Result<'a, Error list>> =
    this.ParseWithConsumed () |> TaskResult.map (fun (_, _, x) -> x)

  member this.ParseAsync () : Async<Result<'a, Error list>> =
    this.ParseTask () |> Task.toAsync

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

  member private this.AddTaskRes (set: 'ctx -> Request -> 'b -> 'a -> Task<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    { this with
        parse =
          fun ctx req ->
            task {
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

  member this.AddTaskRes (set: 'b -> 'a -> Task<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddTaskRes((fun _ _ b a -> set b a), getter)

  member this.AddAsyncRes (set: 'b -> 'a -> Async<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddTaskRes(Task.liftAsync2 set, getter)

  member this.AddTaskRes (set: 'b -> 'c -> 'a -> Task<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this
      .AddTaskRes((fun ctx req b a ->
        task {
          match! getC.Get(ctx, req, this.includedTypeAndId) with
          | Error errs -> return Error errs
          | Ok c -> return! set b c a
        }),
        getter)
      .MarkAsConsumed(getC)

  member this.AddAsyncRes (set: 'b -> 'c -> 'a -> Async<Result<'a, Error list>>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddTaskRes(Task.liftAsync3 set, getter, getC)

  member this.AddTask (set: 'b -> 'a -> Task<'a>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddTaskRes ((fun b a -> set b a |> Task.map Ok), getter)

  member this.AddTask (set: 'b -> 'c -> 'a -> Task<'a>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddTaskRes ((fun b c a -> set b c a |> Task.map Ok), getter, getC)

  member this.AddAsync (set: 'b -> 'a -> Async<'a>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddTask (Task.liftAsync2 set, getter)

  member this.AddAsync (set: 'b -> 'c -> 'a -> Async<'a>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddTask (Task.liftAsync3 set, getter, getC)

  member this.AddRes (set: 'b -> 'c -> 'a -> Result<'a, Error list>, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddTaskRes ((fun b c a -> set b c a |> Task.result), getter, getC)

  member this.AddRes (set: 'b -> 'a -> Result<'a, Error list>, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddTaskRes ((fun b a -> set b a |> Task.result), getter)

  member this.Add (set: 'b -> 'a -> 'a, getter: OptionalRequestGetter<'ctx, 'b>) =
    this.AddTaskRes ((fun b a -> set b a |> Ok |> Task.result), getter)

  member this.Add (set: 'b -> 'c -> 'a -> 'a, getter: OptionalRequestGetter<'ctx, 'b>, getC: RequestGetter<'ctx, 'c>) =
    this.AddTaskRes ((fun b c a -> set b c a |> Ok |> Task.result), getter, getC)

  member this.RequireType(typeName: string) =
    { this with
        parse = fun ctx req ->
          match req.Document.Value with
          | Error errs -> Error errs |> Task.result
          | Ok None -> Error [reqParserMissingData ""] |> Task.result
          | Ok (Some { data = None }) -> Error [reqParserMissingData "/data"] |> Task.result
          | Ok (Some { data = Some { ``type`` = t } }) when t <> typeName -> Error [reqParserInvalidType typeName t "/data/type"] |> Task.result
          | Ok (Some { data = Some _ }) -> this.parse ctx req
    }

  member this.Prohibit (getter: ProhibitedRequestGetter) =
    { this with
        prohibited = getter :: this.prohibited
    }.MarkAsConsumed(getter)

  // Exempts this query parameter from causing strict mode warnings/errors.
  member this.IgnoreStrictMode (getter: OptionalRequestGetter<'ctx, 'b>) =
    this.MarkAsConsumed(getter)

  member this.Map (f: 'a -> 'b) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> TaskResult.map f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindRes (f: 'a -> Result<'b, Error list>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> TaskResult.bindResult f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindTask (f: 'a -> Task<'b>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> TaskResult.bind (f >> Task.map Ok)
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindAsync (f: 'a -> Async<'b>) : RequestParser<'ctx, 'b> =
    this.BindTask(Task.liftAsync f)

  member this.BindTaskRes (f: 'a -> Task<Result<'b, Error list>>) : RequestParser<'ctx, 'b> =
    {
      includedTypeAndId = this.includedTypeAndId
      consumedFields = this.consumedFields
      consumedQueryParams = this.consumedQueryParams
      parse = fun ctx req -> this.parse ctx req |> TaskResult.bind f
      ctx = this.ctx
      request = this.request
      prohibited = this.prohibited
    }

  member this.BindAsyncRes (f: 'a -> Async<Result<'b, Error list>>) : RequestParser<'ctx, 'b> =
    this.BindTaskRes(Task.liftAsync f)



type RequestParserHelper<'ctx> internal (ctx: 'ctx, req: Request, ?includedTypeAndId) =

  let mutable consumedSingleFields : Set<FieldName> = Set.empty
  let mutable consumedSingleParams : Set<FieldName> = Set.empty

  member _.GetRequiredTask(param: RequestGetter<'ctx, 'a>) : Task<Result<'a, Error list>> =
    match param.FieldName with None -> () | Some n -> consumedSingleFields <- consumedSingleFields.Add n
    match param.QueryParamName with None -> () | Some n -> consumedSingleParams <- consumedSingleParams.Add n
    RequestParser<'ctx, 'a>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).ParseTask()

  member _.GetOptionalTask(param: OptionalRequestGetter<'ctx, 'a>) : Task<Result<'a option, Error list>> =
    match param.FieldName with None -> () | Some n -> consumedSingleFields <- consumedSingleFields.Add n
    match param.QueryParamName with None -> () | Some n -> consumedSingleParams <- consumedSingleParams.Add n
    RequestParser<'ctx, 'a option>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).ParseTask()

  member _.GetRequiredAsync(param: RequestGetter<'ctx, 'a>) : Async<Result<'a, Error list>> =
    match param.FieldName with None -> () | Some n -> consumedSingleFields <- consumedSingleFields.Add n
    match param.QueryParamName with None -> () | Some n -> consumedSingleParams <- consumedSingleParams.Add n
    RequestParser<'ctx, 'a>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).ParseAsync()

  member _.GetOptionalAsync(param: OptionalRequestGetter<'ctx, 'a>) : Async<Result<'a option, Error list>> =
    match param.FieldName with None -> () | Some n -> consumedSingleFields <- consumedSingleFields.Add n
    match param.QueryParamName with None -> () | Some n -> consumedSingleParams <- consumedSingleParams.Add n
    RequestParser<'ctx, 'a option>.Create(Set.empty, Set.empty, includedTypeAndId, ctx, req, fun c r -> param.Get(c, r, includedTypeAndId)).ParseAsync()

  // Arity 0

  member _.ForTaskRes (create: Task<Result<'a, Error list>>) =
    RequestParser<'ctx, 'a>.Create (consumedSingleFields, consumedSingleParams, includedTypeAndId, ctx, req, fun _ _ -> create)

  member _.ForAsyncRes (create: Async<Result<'a, Error list>>) =
    RequestParser<'ctx, 'a>.Create (consumedSingleFields, consumedSingleParams, includedTypeAndId, ctx, req, fun _ _ -> Task.fromAsync create)

  member this.ForTask (create: Task<'a>) =
    this.ForTaskRes (create |> Task.map Ok)

  member this.ForAsync (create: Async<'a>) =
    this.ForTask (create |> Task.fromAsync)

  member this.ForRes (value: Result<'a, Error list>) =
    this.ForTaskRes (value |> Task.result)

  member this.For (value: 'a) =
    this.ForTaskRes (value |> Ok |> Task.result)

  // Arity 1

  member _.ForTaskRes (create: 'p1 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>) =
    let consumedFields = [| p1.FieldName |] |> Array.choose id |> Set.ofArray |> Set.union consumedSingleFields
    let consumedQueryParams = [| p1.QueryParamName |] |> Array.choose id |> Set.ofArray |> Set.union consumedSingleParams
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) |> TaskResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForTaskRes (Task.liftAsync create, p1)

  member this.ForTask (create: 'p1 -> Task<'a>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForTaskRes (TaskResult.liftTask create, p1)

  member this.ForAsync (create: 'p1 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForTask (Task.liftAsync create, p1)

  member this.ForRes (create: 'p1 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>) =
    this.ForTaskRes (Task.lift create, p1)

  member this.For (create: 'p1 -> 'a, p1: RequestGetter<'ctx, 'p1>) =
    this.ForTaskRes (TaskResult.lift create, p1)

  // Arity 2

  member _.ForTaskRes (create: 'p1 -> 'p2 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    let consumedFields = [| p1.FieldName; p2.FieldName |] |> Array.choose id |> Set.ofArray |> Set.union consumedSingleFields
    let consumedQueryParams = [| p1.QueryParamName; p2.QueryParamName |] |> Array.choose id |> Set.ofArray |> Set.union consumedSingleParams
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) |> TaskResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> 'p2 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForTaskRes (Task.liftAsync2 create, p1, p2)

  member this.ForTask (create: 'p1 -> 'p2 -> Task<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForTaskRes (TaskResult.liftTask2 create, p1, p2)

  member this.ForAsync (create: 'p1 -> 'p2 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForTask (Task.liftAsync2 create, p1, p2)

  member this.ForRes (create: 'p1 -> 'p2 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForTaskRes (Task.lift2 create, p1, p2)

  member this.For (create: 'p1 -> 'p2 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>) =
    this.ForTaskRes (TaskResult.lift2 create, p1, p2)

  // Arity 3

  member _.ForTaskRes (create: 'p1 -> 'p2 -> 'p3 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    let consumedFields =
      [| p1.FieldName; p2.FieldName; p3.FieldName |]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleFields
    let consumedQueryParams =
      [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName|]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleParams
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) |> TaskResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForTaskRes (Task.liftAsync3 create, p1, p2, p3)

  member this.ForTask (create: 'p1 -> 'p2 -> 'p3 -> Task<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForTaskRes (TaskResult.liftTask3 create, p1, p2, p3)

  member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForTask (Task.liftAsync3 create, p1, p2, p3)

  member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForTaskRes (Task.lift3 create, p1, p2, p3)

  member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>) =
    this.ForTaskRes (TaskResult.lift3 create, p1, p2, p3)

  // Arity 4

  member _.ForTaskRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    let consumedFields =
      [| p1.FieldName; p2.FieldName; p3.FieldName; p4.FieldName |]
      |> Array.choose id
      |> Set.ofArray
       |> Set.union consumedSingleFields
    let consumedQueryParams =
      [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName; p4.QueryParamName |]
      |> Array.choose id
      |> Set.ofArray
       |> Set.union consumedSingleParams
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) <*> p4.Get(c, r, includedTypeAndId) |> TaskResult.bind id)

  member this.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForTaskRes (Task.liftAsync4 create, p1, p2, p3, p4)

  member this.ForTask (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Task<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForTaskRes (TaskResult.liftTask4 create, p1, p2, p3, p4)

  member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForTask (Task.liftAsync4 create, p1, p2, p3, p4)

  member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForTaskRes (Task.lift4 create, p1, p2, p3, p4)

  member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>) =
    this.ForTaskRes (TaskResult.lift4 create, p1, p2, p3, p4)

  // Arity 5
  
  member _.ForTaskRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
    let consumedFields =
      [| p1.FieldName; p2.FieldName; p3.FieldName; p4.FieldName; p5.FieldName |]
      |> Array.choose id
      |> Set.ofArray
       |> Set.union consumedSingleFields
    let consumedQueryParams =
      [| p1.QueryParamName; p2.QueryParamName; p3.QueryParamName; p4.QueryParamName; p5.QueryParamName |]
      |> Array.choose id
      |> Set.ofArray
       |> Set.union consumedSingleParams
    RequestParser<'ctx, 'a>.Create (consumedFields, consumedQueryParams, includedTypeAndId, ctx, req, fun c r -> create <!> p1.Get(c, r, includedTypeAndId) <*> p2.Get(c, r, includedTypeAndId) <*> p3.Get(c, r, includedTypeAndId) <*> p4.Get(c, r, includedTypeAndId) <*> p5.Get(c, r, includedTypeAndId) |> TaskResult.bind id)
  
  member this.ForAsyncRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Async<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
    this.ForTaskRes (Task.liftAsync5 create, p1, p2, p3, p4, p5)

  member this.ForTask (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Task<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
    this.ForTaskRes (TaskResult.liftTask5 create, p1, p2, p3, p4, p5)

  member this.ForAsync (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Async<'a>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
    this.ForTask (Task.liftAsync5 create, p1, p2, p3, p4, p5)
  
  member this.ForRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> Result<'a, Error list>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
    this.ForTaskRes (Task.lift5 create, p1, p2, p3, p4, p5)
  
  member this.For (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'a, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>) =
    this.ForTaskRes (TaskResult.lift5 create, p1, p2, p3, p4, p5)

  // Arity 6
  
  member _.ForTaskRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>, p6: RequestGetter<'ctx, 'p6>) =
    let consumedFields =
      [|
          p1.FieldName
          p2.FieldName
          p3.FieldName
          p4.FieldName
          p5.FieldName
          p6.FieldName
      |]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleFields

    let consumedQueryParams =
      [|
        p1.QueryParamName
        p2.QueryParamName
        p3.QueryParamName
        p4.QueryParamName
        p5.QueryParamName
        p6.QueryParamName
      |]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleParams

    RequestParser<'ctx, 'a>.Create(
      consumedFields,
      consumedQueryParams,
      includedTypeAndId,
      ctx,
      req,
      fun c r ->
        create
        <!> p1.Get(c, r, includedTypeAndId)
        <*> p2.Get(c, r, includedTypeAndId)
        <*> p3.Get(c, r, includedTypeAndId)
        <*> p4.Get(c, r, includedTypeAndId)
        <*> p5.Get(c, r, includedTypeAndId)
        <*> p6.Get(c, r, includedTypeAndId)
        |> TaskResult.bind id)
  
  member this.ForAsyncRes
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> Async<Result<'a, Error list>>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 -> create p1 p2 p3 p4 p5 p6 |> Task.fromAsync),
      p1, p2, p3, p4, p5, p6
    )

  member this.ForTask
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> Task<'a>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>
        ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 -> create p1 p2 p3 p4 p5 p6 |> Task.map Ok),
      p1, p2, p3, p4, p5, p6
    )

  member this.ForAsync
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> Async<'a>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>
      ) =
    this.ForTask(
      (fun p1 p2 p3 p4 p5 p6 -> create p1 p2 p3 p4 p5 p6 |> Task.fromAsync),
      p1, p2, p3, p4, p5, p6
    )
  
  member this.ForRes
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> Result<'a, Error list>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 -> create p1 p2 p3 p4 p5 p6 |> Task.result),
      p1, p2, p3, p4, p5, p6
    )
  
  member this.For
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'a,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 -> create p1 p2 p3 p4 p5 p6 |> Ok |> Task.result),
      p1, p2, p3, p4, p5, p6
    )

  // Arity 7
  
  member _.ForTaskRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>, p6: RequestGetter<'ctx, 'p6>, p7: RequestGetter<'ctx, 'p7>) =
    let consumedFields =
      [|
          p1.FieldName
          p2.FieldName
          p3.FieldName
          p4.FieldName
          p5.FieldName
          p6.FieldName
          p7.FieldName
      |]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleFields

    let consumedQueryParams =
      [|
        p1.QueryParamName
        p2.QueryParamName
        p3.QueryParamName
        p4.QueryParamName
        p5.QueryParamName
        p6.QueryParamName
        p7.QueryParamName
      |]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleParams

    RequestParser<'ctx, 'a>.Create(
      consumedFields,
      consumedQueryParams,
      includedTypeAndId,
      ctx,
      req,
      fun c r ->
        create
        <!> p1.Get(c, r, includedTypeAndId)
        <*> p2.Get(c, r, includedTypeAndId)
        <*> p3.Get(c, r, includedTypeAndId)
        <*> p4.Get(c, r, includedTypeAndId)
        <*> p5.Get(c, r, includedTypeAndId)
        <*> p6.Get(c, r, includedTypeAndId)
        <*> p7.Get(c, r, includedTypeAndId)
        |> TaskResult.bind id)
  
  member this.ForAsyncRes
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> Async<Result<'a, Error list>>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 -> create p1 p2 p3 p4 p5 p6 p7 |> Task.fromAsync),
      p1, p2, p3, p4, p5, p6, p7
    )

  member this.ForTask
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> Task<'a>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>
        ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 -> create p1 p2 p3 p4 p5 p6 p7 |> Task.map Ok),
      p1, p2, p3, p4, p5, p6, p7
    )

  member this.ForAsync
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> Async<'a>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>
      ) =
    this.ForTask(
      (fun p1 p2 p3 p4 p5 p6 p7 -> create p1 p2 p3 p4 p5 p6 p7 |> Task.fromAsync),
      p1, p2, p3, p4, p5, p6, p7
    )
  
  member this.ForRes
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> Result<'a, Error list>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 -> create p1 p2 p3 p4 p5 p6 p7 |> Task.result),
      p1, p2, p3, p4, p5, p6, p7
    )
  
  member this.For
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> 'a,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 -> create p1 p2 p3 p4 p5 p6 p7 |> Ok |> Task.result),
      p1, p2, p3, p4, p5, p6, p7
    )

  // Arity 8
  
  member _.ForTaskRes (create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> 'p8 -> Task<Result<'a, Error list>>, p1: RequestGetter<'ctx, 'p1>, p2: RequestGetter<'ctx, 'p2>, p3: RequestGetter<'ctx, 'p3>, p4: RequestGetter<'ctx, 'p4>, p5: RequestGetter<'ctx, 'p5>, p6: RequestGetter<'ctx, 'p6>, p7: RequestGetter<'ctx, 'p7>, p8: RequestGetter<'ctx, 'p8>) =
    let consumedFields =
      [|
          p1.FieldName
          p2.FieldName
          p3.FieldName
          p4.FieldName
          p5.FieldName
          p6.FieldName
          p7.FieldName
          p8.FieldName
      |]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleFields

    let consumedQueryParams =
      [|
        p1.QueryParamName
        p2.QueryParamName
        p3.QueryParamName
        p4.QueryParamName
        p5.QueryParamName
        p6.QueryParamName
        p7.QueryParamName
        p8.QueryParamName
      |]
      |> Array.choose id
      |> Set.ofArray
      |> Set.union consumedSingleParams

    RequestParser<'ctx, 'a>.Create(
      consumedFields,
      consumedQueryParams,
      includedTypeAndId,
      ctx,
      req,
      fun c r ->
        create
        <!> p1.Get(c, r, includedTypeAndId)
        <*> p2.Get(c, r, includedTypeAndId)
        <*> p3.Get(c, r, includedTypeAndId)
        <*> p4.Get(c, r, includedTypeAndId)
        <*> p5.Get(c, r, includedTypeAndId)
        <*> p6.Get(c, r, includedTypeAndId)
        <*> p7.Get(c, r, includedTypeAndId)
        <*> p8.Get(c, r, includedTypeAndId)
        |> TaskResult.bind id)
  
  member this.ForAsyncRes
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> 'p8 -> Async<Result<'a, Error list>>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>,
        p8: RequestGetter<'ctx, 'p8>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 p8 -> create p1 p2 p3 p4 p5 p6 p7 p8 |> Task.fromAsync),
      p1, p2, p3, p4, p5, p6, p7, p8
    )

  member this.ForTask
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> 'p8 -> Task<'a>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>,
        p8: RequestGetter<'ctx, 'p8>
        ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 p8 -> create p1 p2 p3 p4 p5 p6 p7 p8 |> Task.map Ok),
      p1, p2, p3, p4, p5, p6, p7, p8
    )

  member this.ForAsync
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> 'p8 -> Async<'a>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>,
        p8: RequestGetter<'ctx, 'p8>
      ) =
    this.ForTask(
      (fun p1 p2 p3 p4 p5 p6 p7 p8 -> create p1 p2 p3 p4 p5 p6 p7 p8 |> Task.fromAsync),
      p1, p2, p3, p4, p5, p6, p7, p8
    )
  
  member this.ForRes
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> 'p8 -> Result<'a, Error list>,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>,
        p8: RequestGetter<'ctx, 'p8>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 p8 -> create p1 p2 p3 p4 p5 p6 p7 p8 |> Task.result),
      p1, p2, p3, p4, p5, p6, p7, p8
    )
  
  member this.For
      ( create: 'p1 -> 'p2 -> 'p3 -> 'p4 -> 'p5 -> 'p6 -> 'p7 -> 'p8 -> 'a,
        p1: RequestGetter<'ctx, 'p1>,
        p2: RequestGetter<'ctx, 'p2>,
        p3: RequestGetter<'ctx, 'p3>,
        p4: RequestGetter<'ctx, 'p4>,
        p5: RequestGetter<'ctx, 'p5>,
        p6: RequestGetter<'ctx, 'p6>,
        p7: RequestGetter<'ctx, 'p7>,
        p8: RequestGetter<'ctx, 'p8>
      ) =
    this.ForTaskRes(
      (fun p1 p2 p3 p4 p5 p6 p7 p8 -> create p1 p2 p3 p4 p5 p6 p7 p8 |> Ok |> Task.result),
      p1, p2, p3, p4, p5, p6, p7, p8
    )


  // TODO: Higher arities
