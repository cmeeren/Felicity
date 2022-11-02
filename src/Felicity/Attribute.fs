namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json.Serialization
open System.Threading.Tasks
open Errors


type internal Attribute<'ctx> =
  abstract Name: AttributeName
  abstract BoxedGetSerialized: ('ctx -> BoxedEntity -> Task<BoxedSerializedField Skippable>) option
  abstract RequiresExplicitInclude: bool


type internal ConstrainedField<'ctx> =
  abstract Name: FieldName
  abstract HasConstraints: bool
  abstract BoxedGetConstraints: 'ctx -> BoxedEntity -> Task<(string * obj) list>
  abstract RequiresExplicitInclude: bool


type FieldSetter<'ctx> =
  abstract Names: Set<FieldName>
  abstract SetOrder: int
  abstract Set: 'ctx -> Request -> BoxedEntity -> Map<FieldName, int> -> Task<Result<BoxedEntity * bool, Error list>>


type FieldQueryParser<'ctx, 'entity, 'attr, 'serialized> =
  abstract Name: string
  abstract ToDomain: 'ctx -> ('serialized -> ParsedValueInfo) -> 'serialized -> Task<Result<'attr, Error list>>



type NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> = internal {
  name: string
  setOrder: int
  mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>
  fromDomain: 'attr -> 'serialized
  toDomain: 'ctx -> ('serialized -> ParsedValueInfo) -> 'serialized -> Task<Result<'attr, Error list>>
  get: ('ctx -> 'entity -> Task<'attr Skippable>) option
  set: ('setCtx -> 'attr -> 'entity -> Task<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>
  requiresExplicitInclude: bool
} with

  static member internal Create(name: string, mapSetCtx, fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> ('serialized -> ParsedValueInfo) -> 'serialized -> Task<Result<'attr, Error list>>) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    {
      name = name
      setOrder = 0
      mapSetCtx = mapSetCtx
      fromDomain = fromDomain
      toDomain = toDomain
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> Task.result []
      requiresExplicitInclude = false
    }

  interface FieldSetter<'ctx> with
    member this.Names = Set.singleton this.name
    member this.SetOrder = this.setOrder
    member this.Set ctx req entity numSetters =
      task {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { attributes = Include attrVals } }) ->
            match this.set, attrVals.TryGetValue this.name with
            | _, (false, _) -> return Ok (entity, false)  // not provided in request
            | None, (true, _) ->
                if numSetters[this.name] > 1 then
                  // Provided in request and no setter, but there exists another setter, so ignore
                  return Ok (entity, false)
                else
                  return Error [setAttrReadOnly this.name ("/data/attributes/" + this.name)]
            | Some set, (true, attrValue) ->
                match! this.mapSetCtx ctx (unbox<'entity> entity) with
                | Error errs -> return Error errs
                | Ok setCtx ->
                    let getValueInfo value = FromBodyAttribute { Name = this.name; StringValue = string<'serialized> (unbox<'serialized> value) }
                    return!
                      this.toDomain ctx getValueInfo (unbox<'serialized> attrValue)
                      |> TaskResult.bind (fun domain -> set setCtx domain (unbox<'entity> entity))
                      |> TaskResult.mapError (List.map (Error.setSourcePointer ("/data/attributes/" + this.name)))
                      |> TaskResult.map (fun e -> box e, true)
        | _ -> return Ok (entity, false)  // no attributes provided
      }

  interface Attribute<'ctx> with

    member this.Name = this.name

    member this.BoxedGetSerialized =
      this.get
      |> Option.map (fun get ->
          fun ctx res ->
            get ctx (unbox<'entity> res) |> Task.map (Skippable.map (this.fromDomain >> box))
      )
    member this.RequiresExplicitInclude = this.requiresExplicitInclude


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)
    member this.RequiresExplicitInclude = this.requiresExplicitInclude


  member this.Optional =
    { new RequestGetter<'ctx, 'attr option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getAttrAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? 'serialized as attr) ->
                  let getValueInfo value = FromBodyAttribute { Name = this.name; StringValue = string<'serialized> value }
                  this.toDomain ctx getValueInfo attr
                  |> TaskResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                  |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected attribute '%s{this.name}' to be deserialized to %s{typeof<'serialized>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result
    }

  interface OptionalRequestGetter<'ctx, 'attr> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'attr> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingAttr includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredAttr this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { attributes = Include attrVals } }) when attrVals.ContainsKey this.name ->
          let pointer = Request.pointerForMissingAttr includedTypeAndId req + "/" + this.name
          [reqParserProhibitedAttr this.name pointer]
      | _ -> []

  interface FieldQueryParser<'ctx, 'entity, 'attr, 'serialized> with
    member this.Name = this.name
    member this.ToDomain ctx getInfo serialized =
      this.toDomain ctx getInfo serialized


  member this.Name = this.name

  /// Specify the order in which this field will be set relative to other fields during
  /// POST collection and PATCH resource requests. By default, all fields have SetOrder =
  /// 0. Negative numbers are allowed. The order of fields with identical SetOrder is
  /// unspecified.
  member this.SetOrder (i: int) =
    { this with setOrder = i }

  member this.GetTaskSkip (get: Func<'ctx, 'entity, Task<'attr Skippable>>) =
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetTaskSkip (get: Func<'entity, Task<'attr Skippable>>) =
    this.GetTaskSkip(fun _ e -> get.Invoke(e))

  member this.GetAsyncSkip (get: Func<'ctx, 'entity, Async<'attr Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetAsyncSkip (get: Func<'entity, Async<'attr Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc get)

  member this.GetTask (get: Func<'ctx, 'entity, Task<'attr>>) =
    this.GetTaskSkip (fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetTask (get: Func<'entity, Task<'attr>>) =
    this.GetTaskSkip (fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'attr>>) =
    this.GetTask (Task.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'attr>>) =
    this.GetTask (Task.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, 'attr Skippable>) =
    this.GetTaskSkip (Task.liftFunc2 get)

  member this.GetSkip (get: Func<'entity, 'attr Skippable>) =
    this.GetTaskSkip (Task.liftFunc get)

  member this.Get (get: Func<'ctx, 'entity, 'attr>) =
    this.GetTaskSkip (fun ctx r -> get.Invoke(ctx, r) |> Include |> Task.result)

  member this.Get (get: Func<'entity, 'attr>) =
    this.GetTaskSkip (fun _ r -> get.Invoke r |> Include |> Task.result)

  member this.SetTaskRes (set: 'setCtx -> 'attr -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with set = Some set }

  member this.SetTaskRes (set: 'attr -> 'entity -> Task<Result<'entity, Error list>>) =
    this.SetTaskRes (fun _ x e -> set x e)

  member this.SetAsyncRes (set: 'setCtx -> 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetTaskRes(Task.liftAsync3 set)

  member this.SetAsyncRes (set: 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetTaskRes (Task.liftAsync2 set)

  member this.SetTask (set: 'setCtx -> 'attr -> 'entity -> Task<'entity>) =
    this.SetTaskRes (fun ctx x e -> set ctx x e |> Task.map Ok)

  member this.SetTask (set: 'attr -> 'entity -> Task<'entity>) =
    this.SetTaskRes (fun _ x e -> set x e |> Task.map Ok)

  member this.SetAsync (set: 'setCtx -> 'attr -> 'entity -> Async<'entity>) =
    this.SetTask (Task.liftAsync3 set)

  member this.SetAsync (set: 'attr -> 'entity -> Async<'entity>) =
    this.SetTask (Task.liftAsync2 set)

  member this.SetRes (set: 'setCtx -> 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetTaskRes (Task.lift3 set)

  member this.SetRes (set: 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetTaskRes (Task.lift2 set)

  member this.Set (set: 'setCtx -> 'attr -> 'entity -> 'entity) =
    this.SetTaskRes (TaskResult.lift3 set)

  member this.Set (set: 'attr -> 'entity -> 'entity) =
    this.SetTaskRes (TaskResult.lift2 set)

  member this.AddConstraintsTask(getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            task {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsTask(Task.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsTask(Task.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsTask(fun ctx e -> [name, box (getValue ctx e)] |> Task.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  /// If the 'requireExplicitInclude' parameter is true (the default when calling this method), the attribute will only
  /// be present in the response if explicitly specified using sparse fieldsets.
  member this.RequireExplicitInclude(?requireExplicitInclude) =
    { this with requiresExplicitInclude = defaultArg requireExplicitInclude true }



[<AutoOpen>]
module NonNullableAttributeExtensions =

  type NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> with

    member this.AddConstraint (name: string, value: 'a) =
      this.AddConstraint(name, fun _ -> value)




type NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> = internal {
  name: string
  setOrder: int
  mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>
  fromDomain: 'attr -> 'serialized
  toDomain: 'ctx -> ('serialized -> ParsedValueInfo) -> 'serialized -> Task<Result<'attr, Error list>>
  get: ('ctx -> 'entity -> Task<'attr option Skippable>) option
  set: ('setCtx -> 'attr option -> 'entity -> Task<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>
  requiresExplicitInclude: bool
} with

  static member internal Create(name: string, mapSetCtx, fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> ('serialized -> ParsedValueInfo) -> 'serialized -> Task<Result<'attr, Error list>>) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    {
      name = name
      setOrder = 0
      mapSetCtx = mapSetCtx
      fromDomain = fromDomain
      toDomain = toDomain
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> Task.result []
      requiresExplicitInclude = false
    }

  member internal this.nullableFromDomain =
    Option.map this.fromDomain

  member internal this.nullableToDomain =
    fun ctx i -> Option.traverseTaskResult (this.toDomain ctx i)

  interface FieldSetter<'ctx> with
    member this.Names = Set.singleton this.name
    member this.SetOrder = this.setOrder
    member this.Set ctx req entity numSetters =
      task {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { attributes = Include attrVals } }) ->
            match this.set, attrVals.TryGetValue this.name with
            | _, (false, _) -> return Ok (entity, false)  // not provided in request
            | None, (true, _) ->
                if numSetters[this.name] > 1 then
                  // Provided in request and no setter, but there exists another setter, so ignore
                  return Ok (entity, false)
                else
                  return Error [setAttrReadOnly this.name ("/data/attributes/" + this.name)]
            | Some set, (true, attrValue) ->
                match! this.mapSetCtx ctx (unbox<'entity> entity) with
                | Error errs -> return Error errs
                | Ok setCtx ->
                    let getValueInfo value = FromBodyAttribute { Name = this.name; StringValue = string<'serialized> value }
                    return!
                      this.nullableToDomain ctx getValueInfo (unbox<'serialized option> attrValue)
                      |> TaskResult.bind (fun domain -> set setCtx domain (unbox<'entity> entity))
                      |> TaskResult.mapError (List.map (Error.setSourcePointer ("/data/attributes/" + this.name)))
                      |> TaskResult.map (fun e -> box e, true)
        | _ -> return Ok (entity, false)  // no attributes provided
      }

  interface Attribute<'ctx> with

    member this.Name = this.name

    member this.BoxedGetSerialized =
      this.get
      |> Option.map (fun get ->
          fun ctx res ->
            get ctx (unbox<'entity> res) |> Task.map (Skippable.map (this.nullableFromDomain >> box))
      )
    member this.RequiresExplicitInclude = this.requiresExplicitInclude


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)
    member this.RequiresExplicitInclude = this.requiresExplicitInclude


  member this.Optional =
    { new RequestGetter<'ctx, 'attr option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getAttrAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? ('serialized option) as attr) ->
                  let getValueInfo value = FromBodyAttribute { Name = this.name; StringValue = string<'serialized> value }
                  attr
                  |> Option.traverseTaskResult (this.toDomain ctx getValueInfo)
                  |> TaskResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                  |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected attribute '%s{this.name}' to be deserialized to %s{typeof<'serialized option>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result
    }

  member this.AsNonNullableOptional =
    { new RequestGetter<'ctx, 'attr option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getAttrAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? ('serialized option) as attr) ->
                  match attr with
                  | None -> Error [ setAttrNullNotAllowed this.name |> Error.setSourcePointer (attrsPointer + "/" + this.name) ] |> Task.result
                  | Some attr ->
                      let getValueInfo value = FromBodyAttribute { Name = this.name; StringValue = string<'serialized> value }
                      attr
                      |> this.toDomain ctx getValueInfo
                      |> TaskResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                      |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected attribute '%s{this.name}' to be deserialized to %s{typeof<'serialized option>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result
    }

  member this.AsNonNullable =
    { new RequestGetter<'ctx, 'attr> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let pointer = Request.pointerForMissingAttr includedTypeAndId req
          this.AsNonNullableOptional.Get(ctx, req, includedTypeAndId)
          |> TaskResult.requireSome [reqParserMissingRequiredAttr this.name pointer]
    }

  interface OptionalRequestGetter<'ctx, 'attr option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'attr option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingAttr includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredAttr this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { attributes = Include attrVals } }) when attrVals.ContainsKey this.name ->
          let pointer = Request.pointerForMissingAttr includedTypeAndId req + "/" + this.name
          [reqParserProhibitedAttr this.name pointer]
      | _ -> []

  interface FieldQueryParser<'ctx, 'entity, 'attr, 'serialized> with
    member this.Name = this.name
    member this.ToDomain ctx info serialized =
      this.toDomain ctx info serialized


  member this.Name = this.name

  /// Specify the order in which this field will be set relative to other fields during
  /// POST collection and PATCH resource requests. By default, all fields have SetOrder =
  /// 0. Negative numbers are allowed. The order of fields with identical SetOrder is
  /// unspecified.
  member this.SetOrder (i: int) =
    { this with setOrder = i }

  member this.GetTaskSkip (get: Func<'ctx, 'entity, Task<'attr option Skippable>>) =
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetTaskSkip (get: Func<'entity, Task<'attr option Skippable>>) =
    this.GetTaskSkip(fun _ e -> get.Invoke(e))

  member this.GetAsyncSkip (get: Func<'ctx, 'entity, Async<'attr option Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetAsyncSkip (get: Func<'entity, Async<'attr option Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc get)

  member this.GetTask (get: Func<'ctx, 'entity, Task<'attr option>>) =
    this.GetTaskSkip (fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetTask (get: Func<'entity, Task<'attr option>>) =
    this.GetTaskSkip (fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'attr option>>) =
    this.GetTask (Task.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'attr option>>) =
    this.GetTask (Task.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, 'attr option Skippable>) =
    this.GetTaskSkip (Task.liftFunc2 get)

  member this.GetSkip (get: Func<'entity, 'attr option Skippable>) =
    this.GetTaskSkip (Task.liftFunc get)

  member this.Get (get: Func<'ctx, 'entity, 'attr option>) =
    this.GetTaskSkip (fun ctx r -> get.Invoke(ctx, r) |> Include |> Task.result)

  member this.Get (get: Func<'entity, 'attr option>) =
    this.GetTaskSkip (fun _ r -> get.Invoke r |> Include |> Task.result)

  member this.SetTaskRes (set: 'setCtx -> 'attr option -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with set = Some set }

  member this.SetTaskRes (set: 'attr option -> 'entity -> Task<Result<'entity, Error list>>) =
    this.SetTaskRes (fun _ x e -> set x e)

  member this.SetAsyncRes (set: 'setCtx -> 'attr option -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetTaskRes (Task.liftAsync3 set)

  member this.SetAsyncRes (set: 'attr option -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetTaskRes (Task.liftAsync2 set)

  member this.SetTask (set: 'setCtx -> 'attr option -> 'entity -> Task<'entity>) =
    this.SetTaskRes (TaskResult.liftTask3 set)

  member this.SetTask (set: 'attr option -> 'entity -> Task<'entity>) =
    this.SetTaskRes (TaskResult.liftTask2 set)

  member this.SetAsync (set: 'setCtx -> 'attr option -> 'entity -> Async<'entity>) =
    this.SetTask (Task.liftAsync3 set)

  member this.SetAsync (set: 'attr option -> 'entity -> Async<'entity>) =
    this.SetTask (Task.liftAsync2 set)

  member this.SetRes (set: 'setCtx -> 'attr option -> 'entity -> Result<'entity, Error list>) =
    this.SetTaskRes (Task.lift3 set)

  member this.SetRes (set: 'attr option -> 'entity -> Result<'entity, Error list>) =
    this.SetTaskRes (Task.lift2 set)

  member this.Set (set: 'setCtx -> 'attr option -> 'entity -> 'entity) =
    this.SetTaskRes (TaskResult.lift3 set)

  member this.Set (set: 'attr option -> 'entity -> 'entity) =
    this.SetTaskRes (TaskResult.lift2 set)

  member this.SetNonNullTaskRes (set: 'setCtx -> 'attr -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with
        set = Some (fun ctx attr e ->
          attr
          |> Result.requireSome [setAttrNullNotAllowed this.name]
          |> Task.result
          |> TaskResult.bind (fun a -> set ctx a e))
    }

  member this.SetNonNullTaskRes (set: 'attr -> 'entity -> Task<Result<'entity, Error list>>) =
    this.SetNonNullTaskRes (fun _ x e -> set x e)

  member this.SetNonNullAsyncRes (set: 'setCtx -> 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetNonNullTaskRes (Task.liftAsync3 set)

  member this.SetNonNullAsyncRes (set: 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetNonNullTaskRes (Task.liftAsync2 set)

  member this.SetNonNullTask (set: 'setCtx -> 'attr -> 'entity -> Task<'entity>) =
    this.SetNonNullTaskRes (fun ctx x e -> set ctx x e |> Task.map Ok)

  member this.SetNonNullTask (set: 'attr -> 'entity -> Task<'entity>) =
    this.SetNonNullTaskRes (fun _ x e -> set x e |> Task.map Ok)

  member this.SetNonNullAsync (set: 'setCtx -> 'attr -> 'entity -> Async<'entity>) =
    this.SetNonNullTask (Task.liftAsync3 set)

  member this.SetNonNullAsync (set: 'attr -> 'entity -> Async<'entity>) =
    this.SetNonNullTask (Task.liftAsync2 set)

  member this.SetNonNullRes (set: 'setCtx -> 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetNonNullTaskRes (Task.lift3 set)

  member this.SetNonNullRes (set: 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetNonNullTaskRes (Task.lift2 set)

  member this.SetNonNull (set: 'setCtx -> 'attr -> 'entity -> 'entity) =
    this.SetNonNullTaskRes (TaskResult.lift3 set)

  member this.SetNonNull (set: 'attr -> 'entity -> 'entity) =
    this.SetNonNullTaskRes (TaskResult.lift2 set)

  member this.AddConstraintsTask(getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            task {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsTask(Task.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsTask(Task.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsTask(fun ctx e -> [name, box (getValue ctx e)] |> Task.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  /// If the 'requireExplicitInclude' parameter is true (the default when calling this method), the attribute will only
  /// be present in the response if explicitly specified using sparse fieldsets.
  member this.RequireExplicitInclude(?requireExplicitInclude) =
    { this with requiresExplicitInclude = defaultArg requireExplicitInclude true }



[<AutoOpen>]
module NullableAttributeExtensions =

  type NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> with

    member this.AddConstraint (name: string, value: 'a) =
      this.AddConstraint(name, fun _ -> value)



[<AutoOpen>]
module private AttributeParsers =

  open System.Text.Json
  open System.Text.RegularExpressions

  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTimeOffset.
  let parseDateTimeOffset =
    let r = Regex("(?>Z|(?>\+|-)\d\d:\d\d)$", RegexOptions.Compiled)
    let errMsg = "The value must be a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
    fun (value: string) ->
      try
        let res = JsonSerializer.Deserialize<DateTimeOffset> ("\"" + value + "\"")
        if r.IsMatch value then Ok res
        else Error errMsg
      with _ -> Error errMsg

  let stringifyDateTimeOffset =
    JsonSerializer.Serialize<DateTimeOffset>
    >> fun s -> s.Trim('"')



type NullableAttributeHelper<'ctx, 'setCtx, 'entity> internal (mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>) =

  member _.MapSetContextTaskRes (mapSetCtx: 'ctx -> 'entity -> Task<Result<'mappedSetCtx, Error list>>) : NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity>(mapSetCtx)

  member this.MapSetContextAsyncRes (mapSetCtx: 'ctx -> 'entity -> Async<Result<'mappedSetCtx, Error list>>) : NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextTask (mapSetCtx: 'ctx -> 'entity -> Task<'mappedSetCtx>) : NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (fun ctx e -> mapSetCtx ctx e |> Task.map Ok)

  member this.MapSetContextAsync (mapSetCtx: 'ctx -> 'entity -> Async<'mappedSetCtx>) : NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTask (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextRes (mapSetCtx: 'ctx -> 'entity -> Result<'mappedSetCtx, Error list>) : NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (Task.lift2 mapSetCtx)

  member this.MapSetContext (mapSetCtx: 'ctx -> 'entity -> 'mappedSetCtx) : NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (TaskResult.lift2 mapSetCtx)

  member _.SimpleUnsafe([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'serialized, 'serialized> =
    NullableAttribute<'ctx, 'setCtx, 'entity, 'serialized, 'serialized>.Create(
      name, mapSetCtx, id, fun _ _ -> Ok >> Task.result)

  member this.SimpleBool([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, bool, bool> =
    this.SimpleUnsafe(name)

  member this.SimpleByte([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, byte, byte> =
    this.SimpleUnsafe(name)

  member this.SimpleInt([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, int, int> =
    this.SimpleUnsafe(name)

  member this.SimpleInt64([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, int64, int64> =
    this.SimpleUnsafe(name)

  member this.SimpleDecimal([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, decimal, decimal> =
    this.SimpleUnsafe(name)

  member this.SimpleFloat([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, float, float> =
    this.SimpleUnsafe(name)

  member this.SimpleString([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, string, string> =
    this.SimpleUnsafe(name)

  member this.SimpleChar([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, string, string> =
    this.SimpleUnsafe(name)

  member this.SimpleDateTime([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, DateTime, DateTime> =
    this.SimpleUnsafe(name)

  member this.SimpleDateTimeOffsetAllowMissingOffset([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, DateTimeOffset, DateTimeOffset> =
    this.SimpleUnsafe(name)

  member _.SimpleDateTimeOffset([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, DateTimeOffset, string> =
    NullableAttribute<'ctx, 'setCtx, 'entity, DateTimeOffset, string>.Create(
      name, mapSetCtx, stringifyDateTimeOffset, (fun _ getInfo v -> v |> parseDateTimeOffset |> Result.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton) |> Task.result))

  member this.SimpleGuid([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, Guid, Guid> =
    this.SimpleUnsafe(name)

  member this.SimpleUri([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, Uri, Uri> =
    this.SimpleUnsafe(name)

  member private _.ParsedTaskRes'(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> ('serialized -> ParsedValueInfo) -> 'serialized -> Task<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized>.Create(name, mapSetCtx, fromDomain, toDomain)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx _ -> toDomain ctx), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ _ -> toDomain), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> TaskResult.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton)), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> TaskResult.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton)), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> TaskResult.mapError (List.map (invalidParsedErrMsg (getInfo v)))), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> TaskResult.mapError (List.map (invalidParsedErrMsg (getInfo v)))), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedTaskOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Task.map (Result.requireSome [invalidParsedNone (getInfo v)])), name)

  member this.ParsedTaskOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Task.map (Result.requireSome [invalidParsedNone (getInfo v)])), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskOpt(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskOpt(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedTask(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx _ -> toDomain ctx >> Task.map Ok), name)

  member this.ParsedTask(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ _ -> toDomain >> Task.map Ok), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTask(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTask(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.lift2 toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.lift toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Result.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton) |> Task.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Result.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton) |> Task.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Result.mapError (List.map (invalidParsedErrMsg (getInfo v))) |> Task.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Result.mapError (List.map (invalidParsedErrMsg (getInfo v))) |> Task.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Result.requireSome [invalidParsedNone (getInfo v)] |> Task.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Result.requireSome [invalidParsedNone (getInfo v)] |> Task.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx _ -> toDomain ctx >> Ok >> Task.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ _ -> toDomain >> Ok >> Task.result), name)

  member _.Enum(fromDomain: 'attr -> string, toDomainMap: (string * 'attr) list, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, string> =
    let d = dict toDomainMap
    let allowed = toDomainMap |> List.map fst |> List.distinct
    let toDomain getInfo serialized =
      serialized
      |> d.TryGetValue
      |> function
          | false, _ -> Error [invalidEnum (getInfo serialized) allowed]
          | true, attr -> Ok attr
    NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, string>.Create(
      name, mapSetCtx, fromDomain, fun _ getInfo v -> v |> toDomain getInfo |> Task.result)




type AttributeHelper<'ctx, 'setCtx, 'entity> internal (mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>) =

  member _.Nullable = NullableAttributeHelper<'ctx, 'setCtx, 'entity>(mapSetCtx)

  member _.MapSetContextTaskRes (mapSetCtx: 'ctx -> 'entity -> Task<Result<'mappedSetCtx, Error list>>) =
    AttributeHelper<'ctx, 'mappedSetCtx, 'entity>(mapSetCtx)

  member this.MapSetContextAsyncRes (mapSetCtx: 'ctx -> 'entity -> Async<Result<'mappedSetCtx, Error list>>) : AttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextTask (mapSetCtx: 'ctx -> 'entity -> Task<'mappedSetCtx>) : AttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (fun ctx e -> mapSetCtx ctx e |> Task.map Ok)

  member this.MapSetContextAsync (mapSetCtx: 'ctx -> 'entity -> Async<'mappedSetCtx>) : AttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTask (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextRes (mapSetCtx: 'ctx -> 'entity -> Result<'mappedSetCtx, Error list>) : AttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (Task.lift2 mapSetCtx)

  member this.MapSetContext (mapSetCtx: 'ctx -> 'entity -> 'mappedSetCtx) : AttributeHelper<'ctx, 'mappedSetCtx, 'entity> =
    this.MapSetContextTaskRes (TaskResult.lift2 mapSetCtx)

  member _.SimpleUnsafe([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'serialized, 'serialized> =
    NonNullableAttribute<'ctx, 'setCtx, 'entity, 'serialized, 'serialized>.Create(
      name, mapSetCtx, id, fun _ _ -> Ok >> Task.result)

  member this.SimpleBool([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, bool, bool> =
    this.SimpleUnsafe(name)

  member this.SimpleByte([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, byte, byte> =
    this.SimpleUnsafe(name)

  member this.SimpleInt([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, int, int> =
    this.SimpleUnsafe(name)

  member this.SimpleInt64([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, int64, int64> =
    this.SimpleUnsafe(name)

  member this.SimpleDecimal([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, decimal, decimal> =
    this.SimpleUnsafe(name)

  member this.SimpleFloat([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, float, float> =
    this.SimpleUnsafe(name)

  member this.SimpleString([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, string, string> =
    this.SimpleUnsafe(name)

  member this.SimpleChar([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, string, string> =
    this.SimpleUnsafe(name)

  member this.SimpleDateTime([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, DateTime, DateTime> =
    this.SimpleUnsafe(name)

  member this.SimpleDateTimeOffsetAllowMissingOffset([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, DateTimeOffset, DateTimeOffset> =
    this.SimpleUnsafe(name)

  member _.SimpleDateTimeOffset([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, DateTimeOffset, string> =
    NonNullableAttribute<'ctx, 'setCtx, 'entity, DateTimeOffset, string>.Create(
      name, mapSetCtx, stringifyDateTimeOffset, (fun _ getInfo v -> v |> parseDateTimeOffset |> Result.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton) |> Task.result))

  member this.SimpleGuid([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, Guid, Guid> =
    this.SimpleUnsafe(name)

  member this.SimpleUri([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, Uri, Uri> =
    this.SimpleUnsafe(name)

  member private _.ParsedTaskRes'(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> ('serialized -> ParsedValueInfo) -> 'serialized -> Task<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized>.Create(name, mapSetCtx, fromDomain, toDomain)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx _ -> toDomain ctx), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ _ -> toDomain), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> TaskResult.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton)), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> TaskResult.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton)), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> TaskResult.mapError (List.map (invalidParsedErrMsg (getInfo v)))), name)

  member this.ParsedTaskRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> TaskResult.mapError (List.map (invalidParsedErrMsg (getInfo v)))), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedTaskOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Task.map (Result.requireSome [invalidParsedNone (getInfo v)])), name)

  member this.ParsedTaskOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Task.map (Result.requireSome [invalidParsedNone (getInfo v)])), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskOpt(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskOpt(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedTask(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Task<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx _ -> toDomain ctx >> Task.map Ok), name)

  member this.ParsedTask(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Task<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ _ -> toDomain >> Task.map Ok), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTask(fromDomain, Task.liftAsync2 toDomain, name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTask(fromDomain, Task.liftAsync toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.lift2 toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes(fromDomain, Task.lift toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Result.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton) |> Task.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Result.mapError (invalidParsedErrMsg (getInfo v) >> List.singleton) |> Task.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Result.mapError (List.map (invalidParsedErrMsg (getInfo v))) |> Task.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Result.mapError (List.map (invalidParsedErrMsg (getInfo v))) |> Task.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx getInfo v -> v |> toDomain ctx |> Result.requireSome [invalidParsedNone (getInfo v)] |> Task.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ getInfo v -> v |> toDomain |> Result.requireSome [invalidParsedNone (getInfo v)] |> Task.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun ctx _ -> toDomain ctx >> Ok >> Task.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    this.ParsedTaskRes'(fromDomain, (fun _ _ -> toDomain >> Ok >> Task.result), name)

  member _.Enum(fromDomain: 'attr -> string, toDomainMap: (string * 'attr) list, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, string> =
    let d = dict toDomainMap
    let allowed = toDomainMap |> List.map fst |> List.distinct
    let toDomain getInfo serialized =
      match d.TryGetValue serialized with
      | false, _ -> Error [invalidEnum (getInfo serialized) allowed]
      | true, attr -> Ok attr
    NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, string>.Create(
      name, mapSetCtx, fromDomain, fun _ getInfo v -> v |> toDomain getInfo |> Task.result)
