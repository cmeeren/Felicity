namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json.Serialization
open Hopac
open Errors


type internal Attribute<'ctx> =
  abstract Name: AttributeName
  abstract BoxedGetSerialized: ('ctx -> BoxedEntity -> Job<BoxedSerializedField Skippable>) option


type internal ConstrainedField<'ctx> =
  abstract Name: FieldName
  abstract HasConstraints: bool
  abstract BoxedGetConstraints: 'ctx -> BoxedEntity -> Job<(string * obj) list>


type FieldSetter<'ctx> =
  abstract Names: Set<FieldName>
  abstract SetOrder: int
  abstract Set: 'ctx -> Request -> BoxedEntity -> Map<FieldName, int> -> Job<Result<BoxedEntity, Error list>>


type FieldQueryParser<'ctx, 'entity, 'attr, 'serialized> =
  abstract Name: string
  abstract ToDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>



type NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> = internal {
  name: string
  setOrder: int
  mapSetCtx: 'ctx -> Job<Result<'setCtx, Error list>>
  fromDomain: 'attr -> 'serialized
  toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>
  get: ('ctx -> 'entity -> Job<'attr Skippable>) option
  set: ('setCtx -> 'attr -> 'entity -> Job<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>
} with

  static member internal Create(name: string, mapSetCtx, fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>) : NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    {
      name = name
      setOrder = 0
      mapSetCtx = mapSetCtx
      fromDomain = fromDomain
      toDomain = toDomain
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> Job.result []
    }

  interface FieldSetter<'ctx> with
    member this.Names = Set.singleton this.name
    member this.SetOrder = this.setOrder
    member this.Set ctx req entity numSetters =
      job {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { attributes = Include attrVals } }) ->
            match this.set, attrVals.TryFind this.name with
            | _, None -> return Ok entity  // not provided in request
            | None, Some _ ->
                if numSetters[this.name] > 1 then
                  // Provided in request and no setter, but there exists another setter, so ignore
                  return Ok entity
                else
                  return Error [setAttrReadOnly this.name ("/data/attributes/" + this.name)]
            | Some set, Some attrValue ->
                match! this.mapSetCtx ctx with
                | Error errs -> return Error errs
                | Ok setCtx ->
                    return!
                      this.toDomain ctx (unbox<'serialized> attrValue)
                      |> JobResult.bind (fun domain -> set setCtx domain (unbox<'entity> entity))
                      |> JobResult.mapError (List.map (Error.setSourcePointer ("/data/attributes/" + this.name)))
                      |> JobResult.map box
        | _ -> return Ok entity  // no attributes provided
      }

  interface Attribute<'ctx> with

    member this.Name = this.name

    member this.BoxedGetSerialized =
      this.get
      |> Option.map (fun get ->
          fun ctx res ->
            get ctx (unbox<'entity> res) |> Job.map (Skippable.map (this.fromDomain >> box))
      )


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)


  member this.Optional =
    { new RequestGetter<'ctx, 'attr option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getAttrAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? 'serialized as attr) ->
                  this.toDomain ctx attr
                  |> JobResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                  |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected attribute '%s' to be deserialized to %s, but was %s" this.name typeof<'serialized>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result
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
      |> JobResult.requireSome [reqParserMissingRequiredAttr this.name pointer]

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
    member this.ToDomain ctx serialized =
      this.toDomain ctx serialized


  member this.Name = this.name

  /// Specify the order in which this field will be set relative to other fields during
  /// POST collection and PATCH resource requests. By default, all fields have SetOrder =
  /// 0. Negative numbers are allowed. The order of fields with identical SetOrder is
  /// unspecified.
  member this.SetOrder (i: int) =
    { this with setOrder = i }

  member this.GetJobSkip (get: Func<'ctx, 'entity, Job<'attr Skippable>>) =
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsyncSkip (get: Func<'ctx, 'entity, Async<'attr Skippable>>) =
    this.GetJobSkip(Job.liftAsyncFunc2 get)

  member this.GetJob (get: Func<'ctx, 'entity, Job<'attr>>) =
    this.GetJobSkip (fun ctx r -> get.Invoke(ctx, r) |> Job.map Include)

  member this.GetJob (get: Func<'entity, Job<'attr>>) =
    this.GetJobSkip (fun _ r -> get.Invoke r |> Job.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'attr>>) =
    this.GetJob (Job.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'attr>>) =
    this.GetJob (Job.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, 'attr Skippable>) =
    this.GetJobSkip (Job.liftFunc2 get)

  member this.Get (get: Func<'ctx, 'entity, 'attr>) =
    this.GetJobSkip (fun ctx r -> get.Invoke(ctx, r) |> Include |> Job.result)

  member this.Get (get: Func<'entity, 'attr>) =
    this.GetJobSkip (fun _ r -> get.Invoke r |> Include |> Job.result)

  member this.SetJobRes (set: 'setCtx -> 'attr -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with set = Some set }

  member this.SetJobRes (set: 'attr -> 'entity -> Job<Result<'entity, Error list>>) =
    this.SetJobRes (fun _ x e -> set x e)

  member this.SetAsyncRes (set: 'setCtx -> 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetJobRes(Job.liftAsync3 set)

  member this.SetAsyncRes (set: 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetJobRes (Job.liftAsync2 set)

  member this.SetJob (set: 'setCtx -> 'attr -> 'entity -> Job<'entity>) =
    this.SetJobRes (fun ctx x e -> set ctx x e |> Job.map Ok)

  member this.SetJob (set: 'attr -> 'entity -> Job<'entity>) =
    this.SetJobRes (fun _ x e -> set x e |> Job.map Ok)

  member this.SetAsync (set: 'setCtx -> 'attr -> 'entity -> Async<'entity>) =
    this.SetJob (Job.liftAsync3 set)

  member this.SetAsync (set: 'attr -> 'entity -> Async<'entity>) =
    this.SetJob (Job.liftAsync2 set)

  member this.SetRes (set: 'setCtx -> 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetJobRes (Job.lift3 set)

  member this.SetRes (set: 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetJobRes (Job.lift2 set)

  member this.Set (set: 'setCtx -> 'attr -> 'entity -> 'entity) =
    this.SetJobRes (JobResult.lift3 set)

  member this.Set (set: 'attr -> 'entity -> 'entity) =
    this.SetJobRes (JobResult.lift2 set)

  member this.AddConstraintsJob(getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            job {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsJob(Job.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsJob(Job.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsJob(fun ctx e -> [name, box (getValue ctx e)] |> Job.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)



[<AutoOpen>]
module NonNullableAttributeExtensions =

  type NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> with

    member this.AddConstraint (name: string, value: 'a) =
      this.AddConstraint(name, fun _ -> value)




type NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> = internal {
  name: string
  setOrder: int
  mapSetCtx: 'ctx -> Job<Result<'setCtx, Error list>>
  fromDomain: 'attr -> 'serialized
  toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>
  get: ('ctx -> 'entity -> Job<'attr option Skippable>) option
  set: ('setCtx -> 'attr option -> 'entity -> Job<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>
} with

  static member internal Create(name: string, mapSetCtx, fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>) : NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized> =
    {
      name = name
      setOrder = 0
      mapSetCtx = mapSetCtx
      fromDomain = fromDomain
      toDomain = toDomain
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> Job.result []
    }

  member internal this.nullableFromDomain =
    Option.map this.fromDomain

  member internal this.nullableToDomain =
    fun ctx -> Option.traverseJobResult (this.toDomain ctx)

  interface FieldSetter<'ctx> with
    member this.Names = Set.singleton this.name
    member this.SetOrder = this.setOrder
    member this.Set ctx req entity numSetters =
      job {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { attributes = Include attrVals } }) ->
            match this.set, attrVals.TryFind this.name with
            | _, None -> return Ok entity  // not provided in request
            | None, Some _ ->
                if numSetters[this.name] > 1 then
                  // Provided in request and no setter, but there exists another setter, so ignore
                  return Ok entity
                else
                  return Error [setAttrReadOnly this.name ("/data/attributes/" + this.name)]
            | Some set, Some attrValue ->
                match! this.mapSetCtx ctx with
                | Error errs -> return Error errs
                | Ok setCtx ->
                    return!
                      this.nullableToDomain ctx (unbox<'serialized option> attrValue)
                      |> JobResult.bind (fun domain -> set setCtx domain (unbox<'entity> entity))
                      |> JobResult.mapError (List.map (Error.setSourcePointer ("/data/attributes/" + this.name)))
                      |> JobResult.map box
        | _ -> return Ok entity  // no attributes provided
      }

  interface Attribute<'ctx> with

    member this.Name = this.name

    member this.BoxedGetSerialized =
      this.get
      |> Option.map (fun get ->
          fun ctx res ->
            get ctx (unbox<'entity> res) |> Job.map (Skippable.map (this.nullableFromDomain >> box))
      )


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)


  member this.Optional =
    { new RequestGetter<'ctx, 'attr option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getAttrAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? ('serialized option) as attr) ->
                  attr
                  |> Option.traverseJobResult (this.toDomain ctx)
                  |> JobResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                  |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected attribute '%s' to be deserialized to %s, but was %s" this.name typeof<'serialized option>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result
    }

  member this.AsNonNullableOptional =
    { new RequestGetter<'ctx, 'attr option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getAttrAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? ('serialized option) as attr) ->
                  match attr with
                  | None -> Error [ setAttrNullNotAllowed this.name |> Error.setSourcePointer (attrsPointer + "/" + this.name) ] |> Job.result
                  | Some attr ->
                      attr
                      |> this.toDomain ctx
                      |> JobResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                      |> JobResult.map Some
              | true, x -> failwithf "Framework bug: Expected attribute '%s' to be deserialized to %s, but was %s" this.name typeof<'serialized option>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> Job.result
    }

  member this.AsNonNullable =
    { new RequestGetter<'ctx, 'attr> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let pointer = Request.pointerForMissingAttr includedTypeAndId req
          this.AsNonNullableOptional.Get(ctx, req, includedTypeAndId)
          |> JobResult.requireSome [reqParserMissingRequiredAttr this.name pointer]
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
      |> JobResult.requireSome [reqParserMissingRequiredAttr this.name pointer]

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
    member this.ToDomain ctx serialized =
      this.toDomain ctx serialized


  member this.Name = this.name

  /// Specify the order in which this field will be set relative to other fields during
  /// POST collection and PATCH resource requests. By default, all fields have SetOrder =
  /// 0. Negative numbers are allowed. The order of fields with identical SetOrder is
  /// unspecified.
  member this.SetOrder (i: int) =
    { this with setOrder = i }

  member this.GetJobSkip (get: Func<'ctx, 'entity, Job<'attr option Skippable>>) =
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsyncSkip (get: Func<'ctx, 'entity, Async<'attr option Skippable>>) =
    this.GetJobSkip(Job.liftAsyncFunc2 get)

  member this.GetJob (get: Func<'ctx, 'entity, Job<'attr option>>) =
    this.GetJobSkip (fun ctx r -> get.Invoke(ctx, r) |> Job.map Include)

  member this.GetJob (get: Func<'entity, Job<'attr option>>) =
    this.GetJobSkip (fun _ r -> get.Invoke r |> Job.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'attr option>>) =
    this.GetJob (Job.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'attr option>>) =
    this.GetJob (Job.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, 'attr option Skippable>) =
    this.GetJobSkip (Job.liftFunc2 get)

  member this.Get (get: Func<'ctx, 'entity, 'attr option>) =
    this.GetJobSkip (fun ctx r -> get.Invoke(ctx, r) |> Include |> Job.result)

  member this.Get (get: Func<'entity, 'attr option>) =
    this.GetJobSkip (fun _ r -> get.Invoke r |> Include |> Job.result)

  member this.SetJobRes (set: 'setCtx -> 'attr option -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with set = Some set }

  member this.SetJobRes (set: 'attr option -> 'entity -> Job<Result<'entity, Error list>>) =
    this.SetJobRes (fun _ x e -> set x e)

  member this.SetAsyncRes (set: 'setCtx -> 'attr option -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetJobRes (Job.liftAsync3 set)

  member this.SetAsyncRes (set: 'attr option -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetJobRes (Job.liftAsync2 set)

  member this.SetJob (set: 'setCtx -> 'attr option -> 'entity -> Job<'entity>) =
    this.SetJobRes (JobResult.liftJob3 set)

  member this.SetJob (set: 'attr option -> 'entity -> Job<'entity>) =
    this.SetJobRes (JobResult.liftJob2 set)

  member this.SetAsync (set: 'setCtx -> 'attr option -> 'entity -> Async<'entity>) =
    this.SetJob (Job.liftAsync3 set)

  member this.SetAsync (set: 'attr option -> 'entity -> Async<'entity>) =
    this.SetJob (Job.liftAsync2 set)

  member this.SetRes (set: 'setCtx -> 'attr option -> 'entity -> Result<'entity, Error list>) =
    this.SetJobRes (Job.lift3 set)

  member this.SetRes (set: 'attr option -> 'entity -> Result<'entity, Error list>) =
    this.SetJobRes (Job.lift2 set)

  member this.Set (set: 'setCtx -> 'attr option -> 'entity -> 'entity) =
    this.SetJobRes (JobResult.lift3 set)

  member this.Set (set: 'attr option -> 'entity -> 'entity) =
    this.SetJobRes (JobResult.lift2 set)

  member this.SetNonNullJobRes (set: 'setCtx -> 'attr -> 'entity -> Job<Result<'entity, Error list>>) =
    { this with
        set = Some (fun ctx attr e ->
          attr
          |> Result.requireSome [setAttrNullNotAllowed this.name]
          |> Job.result
          |> JobResult.bind (fun a -> set ctx a e))
    }

  member this.SetNonNullJobRes (set: 'attr -> 'entity -> Job<Result<'entity, Error list>>) =
    this.SetNonNullJobRes (fun _ x e -> set x e)

  member this.SetNonNullAsyncRes (set: 'setCtx -> 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetNonNullJobRes (Job.liftAsync3 set)

  member this.SetNonNullAsyncRes (set: 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetNonNullJobRes (Job.liftAsync2 set)

  member this.SetNonNullJob (set: 'setCtx -> 'attr -> 'entity -> Job<'entity>) =
    this.SetNonNullJobRes (fun ctx x e -> set ctx x e |> Job.map Ok)

  member this.SetNonNullJob (set: 'attr -> 'entity -> Job<'entity>) =
    this.SetNonNullJobRes (fun _ x e -> set x e |> Job.map Ok)

  member this.SetNonNullAsync (set: 'setCtx -> 'attr -> 'entity -> Async<'entity>) =
    this.SetNonNullJob (Job.liftAsync3 set)

  member this.SetNonNullAsync (set: 'attr -> 'entity -> Async<'entity>) =
    this.SetNonNullJob (Job.liftAsync2 set)

  member this.SetNonNullRes (set: 'setCtx -> 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetNonNullJobRes (Job.lift3 set)

  member this.SetNonNullRes (set: 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetNonNullJobRes (Job.lift2 set)

  member this.SetNonNull (set: 'setCtx -> 'attr -> 'entity -> 'entity) =
    this.SetNonNullJobRes (JobResult.lift3 set)

  member this.SetNonNull (set: 'attr -> 'entity -> 'entity) =
    this.SetNonNullJobRes (JobResult.lift2 set)

  member this.AddConstraintsJob(getConstraints: 'ctx -> 'entity -> Job<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            job {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsJob(Job.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsJob(Job.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsJob(fun ctx e -> [name, box (getValue ctx e)] |> Job.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)



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
    fun (str: string) ->
      try
        let res = JsonSerializer.Deserialize<DateTimeOffset> ("\"" + str + "\"")
        if r.IsMatch str then Ok res
        else Error "Missing offset (e.g. 'Z' or '+01:00')"
      with _ -> Error "Invalid ISO 8601-1:2019 date-time"

  let stringifyDateTimeOffset =
    JsonSerializer.Serialize<DateTimeOffset>
    >> fun s -> s.Trim('"')



type NullableAttributeHelper<'ctx, 'setCtx, 'entity> internal (mapSetCtx: 'ctx -> Job<Result<'setCtx, Error list>>) =

  member _.MapSetContextJobRes (mapSetCtx: 'ctx -> Job<Result<'mappedSetCtx, Error list>>) =
    NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity>(mapSetCtx)
  
  member this.MapSetContextAsyncRes (mapSetCtx: 'ctx -> Async<Result<'mappedSetCtx, Error list>>) =
    this.MapSetContextJobRes (Job.liftAsync mapSetCtx)
  
  member this.MapSetContextJob (mapSetCtx: 'ctx -> Job<'mappedSetCtx>) =
    this.MapSetContextJobRes (mapSetCtx >> Job.map Ok)
  
  member this.MapSetContextAsync (mapSetCtx: 'ctx -> Async<'mappedSetCtx>) =
    this.MapSetContextJob (Job.liftAsync mapSetCtx)
  
  member this.MapSetContextRes (mapSetCtx: 'ctx -> Result<'mappedSetCtx, Error list>) =
    this.MapSetContextJobRes (Job.lift mapSetCtx)
  
  member this.MapSetContext (mapSetCtx: 'ctx -> 'mappedSetCtx) =
    this.MapSetContextJobRes (JobResult.lift mapSetCtx)

  member _.SimpleUnsafe([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NullableAttribute<'ctx, 'setCtx, 'entity, 'serialized, 'serialized>.Create(
      name, mapSetCtx, id, fun _ -> Ok >> Job.result)

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
      name, mapSetCtx, stringifyDateTimeOffset, (fun _ -> parseDateTimeOffset >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> Job.result))

  member this.SimpleGuid([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, Guid, Guid> =
    this.SimpleUnsafe(name)

  member this.SimpleUri([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NullableAttribute<'ctx, 'setCtx, 'entity, Uri, Uri> =
    this.SimpleUnsafe(name)

  member private _.ParsedJobRes'(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized>.Create(name, mapSetCtx, fromDomain, toDomain)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, toDomain, name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> JobResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> JobResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> JobResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> JobResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedJobOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Job.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedJobOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Job.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobOpt(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobOpt(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedJob(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Job.map Ok), name)

  member this.ParsedJob(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Job.map Ok), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJob(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJob(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.lift2 toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.lift toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> Job.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> Job.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> Job.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> Job.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.requireSome [attrInvalidParsedNone name] >> Job.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Result.requireSome [attrInvalidParsedNone name] >> Job.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Ok >> Job.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Ok >> Job.result), name)

  member _.Enum(fromDomain: 'attr -> string, toDomainMap: (string * 'attr) list, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let d = dict toDomainMap
    let allowed = toDomainMap |> List.map fst |> List.distinct
    let toDomain serialized =
      serialized
      |> d.TryGetValue
      |> function
          | false, _ -> Error [attrInvalidEnum name serialized allowed]
          | true, attr -> Ok attr
    NullableAttribute<'ctx, 'setCtx, 'entity, 'attr, string>.Create(
      name, mapSetCtx, fromDomain, fun _ -> toDomain >> Job.result)




type AttributeHelper<'ctx, 'setCtx, 'entity> internal (mapSetCtx: 'ctx -> Job<Result<'setCtx, Error list>>) =

  member _.Nullable = NullableAttributeHelper<'ctx, 'setCtx, 'entity>(mapSetCtx)

  member _.MapSetContextJobRes (mapSetCtx: 'ctx -> Job<Result<'mappedSetCtx, Error list>>) =
    AttributeHelper<'ctx, 'mappedSetCtx, 'entity>(mapSetCtx)
  
  member this.MapSetContextAsyncRes (mapSetCtx: 'ctx -> Async<Result<'mappedSetCtx, Error list>>) =
    this.MapSetContextJobRes (Job.liftAsync mapSetCtx)
  
  member this.MapSetContextJob (mapSetCtx: 'ctx -> Job<'mappedSetCtx>) =
    this.MapSetContextJobRes (mapSetCtx >> Job.map Ok)
  
  member this.MapSetContextAsync (mapSetCtx: 'ctx -> Async<'mappedSetCtx>) =
    this.MapSetContextJob (Job.liftAsync mapSetCtx)
  
  member this.MapSetContextRes (mapSetCtx: 'ctx -> Result<'mappedSetCtx, Error list>) =
    this.MapSetContextJobRes (Job.lift mapSetCtx)
  
  member this.MapSetContext (mapSetCtx: 'ctx -> 'mappedSetCtx) =
    this.MapSetContextJobRes (JobResult.lift mapSetCtx)

  member _.SimpleUnsafe([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NonNullableAttribute<'ctx, 'setCtx, 'entity, 'serialized, 'serialized>.Create(
      name, mapSetCtx, id, fun _ -> Ok >> Job.result)

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
      name, mapSetCtx, stringifyDateTimeOffset, (fun _ -> parseDateTimeOffset >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> Job.result))

  member this.SimpleGuid([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, Guid, Guid> =
    this.SimpleUnsafe(name)

  member this.SimpleUri([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) : NonNullableAttribute<'ctx, 'setCtx, 'entity, Uri, Uri> =
    this.SimpleUnsafe(name)

  member private _.ParsedJobRes'(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, 'serialized>.Create(name, mapSetCtx, fromDomain, toDomain)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, toDomain, name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> JobResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> JobResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> JobResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedJobRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> JobResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedJobOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Job.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedJobOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Job.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobOpt(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobOpt(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedJob(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Job<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Job.map Ok), name)

  member this.ParsedJob(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Job<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Job.map Ok), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJob(fromDomain, Job.liftAsync2 toDomain, name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJob(fromDomain, Job.liftAsync toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.lift2 toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes(fromDomain, Job.lift toDomain, name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> Job.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> Job.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> Job.result), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> Job.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.requireSome [attrInvalidParsedNone name] >> Job.result), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Result.requireSome [attrInvalidParsedNone name] >> Job.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun ctx -> toDomain ctx >> Ok >> Job.result), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedJobRes'(fromDomain, (fun _ -> toDomain >> Ok >> Job.result), name)

  member _.Enum(fromDomain: 'attr -> string, toDomainMap: (string * 'attr) list, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let d = dict toDomainMap
    let allowed = toDomainMap |> List.map fst |> List.distinct
    let toDomain serialized =
      match d.TryGetValue serialized with
      | false, _ -> Error [attrInvalidEnum name serialized allowed]
      | true, attr -> Ok attr
    NonNullableAttribute<'ctx, 'setCtx, 'entity, 'attr, string>.Create(
      name, mapSetCtx, fromDomain, fun _ -> toDomain >> Job.result)
