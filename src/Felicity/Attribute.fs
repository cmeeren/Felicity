namespace Felicity

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json.Serialization
open Errors


type internal Attribute<'ctx> =
  abstract Name: AttributeName
  abstract BoxedGetSerialized: ('ctx -> BoxedEntity -> Async<BoxedSerializedField Skippable>) option


type internal ConstrainedField<'ctx> =
  abstract Name: FieldName
  abstract HasConstraints: bool
  abstract BoxedGetConstraints: 'ctx -> BoxedEntity -> Async<(string * obj) list>


type internal FieldSetter<'ctx> =
  abstract Name: FieldName
  abstract Set: 'ctx -> Request -> BoxedEntity -> Async<Result<BoxedEntity, Error list>>


type NonNullableAttribute<'ctx, 'entity, 'attr, 'serialized> = internal {
  name: string
  fromDomain: 'attr -> 'serialized
  toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>
  get: ('ctx -> 'entity -> Async<'attr Skippable>) option
  set: ('ctx -> 'attr -> 'entity -> Async<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>
} with

  static member internal Create(name: string, fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>) : NonNullableAttribute<'ctx, 'entity, 'attr, 'serialized> =
    {
      name = name
      fromDomain = fromDomain
      toDomain = toDomain
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> async.Return []
    }

  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      async {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { attributes = Include attrVals } }) ->
            match this.set, attrVals.TryFind this.name with
            | _, None -> return Ok entity  // not provided in request
            | None, Some _ -> return Error [setAttrReadOnly this.name ("/data/attributes/" + this.name)]
            | Some set, Some attrValue ->
                return!
                  this.toDomain ctx (unbox<'serialized> attrValue)
                  |> AsyncResult.bind (fun domain -> set ctx domain (unbox<'entity> entity))
                  |> AsyncResult.mapError (List.map (Error.setSourcePointer ("/data/attributes/" + this.name)))
                  |> AsyncResult.map box
        | _ -> return Ok entity  // no attributes provided
      }

  interface Attribute<'ctx> with

    member this.Name = this.name

    member this.BoxedGetSerialized =
      this.get
      |> Option.map (fun get ->
          fun ctx res ->
            get ctx (unbox<'entity> res) |> Async.map (Skippable.map (this.fromDomain >> box))
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
          | Error errs -> Error errs |> async.Return
          | Ok None -> None |> Ok |> async.Return
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? 'serialized as attr) ->
                  this.toDomain ctx attr
                  |> AsyncResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                  |> AsyncResult.map Some
              | true, x -> failwithf "Framework bug: Expected attribute '%s' to be deserialized to %s, but was %s" this.name typeof<'serialized>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> async.Return
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
      |> AsyncResult.requireSome [reqParserMissingRequiredAttr this.name pointer]

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


  member this.Name = this.name

  // Use Func to help type inference: https://github.com/dotnet/fsharp/issues/8110
  member this.GetAsyncSkip (get: Func<'ctx, 'entity, Async<'attr Skippable>>) =
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'attr>>) =
    this.GetAsyncSkip (fun ctx r -> get.Invoke(ctx, r) |> Async.map Include)

  member this.GetAsync (get: Func<'entity, Async<'attr>>) =
    this.GetAsyncSkip (fun _ r -> get.Invoke r |> Async.map Include)

  member this.GetSkip (get: Func<'ctx, 'entity, 'attr Skippable>) =
    this.GetAsyncSkip (fun ctx r -> get.Invoke(ctx, r) |> async.Return)

  member this.Get (get: Func<'ctx, 'entity, 'attr>) =
    this.GetAsyncSkip (fun ctx r -> get.Invoke(ctx, r) |> Include |> async.Return)

  member this.Get (get: Func<'entity, 'attr>) =
    this.GetAsyncSkip (fun _ r -> get.Invoke r |> Include |> async.Return)

  member this.SetAsyncRes (set: 'ctx -> 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    { this with set = Some set }

  member this.SetAsyncRes (set: 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetAsyncRes (fun _ x e -> set x e)

  member this.SetAsync (set: 'ctx -> 'attr -> 'entity -> Async<'entity>) =
    this.SetAsyncRes (fun ctx x e -> set ctx x e |> Async.map Ok)

  member this.SetAsync (set: 'attr -> 'entity -> Async<'entity>) =
    this.SetAsyncRes (fun _ x e -> set x e |> Async.map Ok)

  member this.SetRes (set: 'ctx -> 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetAsyncRes (fun ctx x e -> set ctx x e |> async.Return)

  member this.SetRes (set: 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetAsyncRes (fun _ x e -> set x e |> async.Return)

  member this.Set (set: 'ctx -> 'attr -> 'entity -> 'entity) =
    this.SetAsyncRes (fun ctx x e -> set ctx x e |> Ok |> async.Return)

  member this.Set (set: 'attr -> 'entity -> 'entity) =
    this.SetAsyncRes (fun _ x e -> set x e |> Ok |> async.Return)

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            async {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsAsync(fun ctx e -> getConstraints ctx e |> async.Return)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsAsync(fun ctx e -> [name, box (getValue ctx e)] |> async.Return)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.AddConstraint (name: string, value: 'a) =
    this.AddConstraint(name, fun _ -> value)



type NullableAttribute<'ctx, 'entity, 'attr, 'serialized> = internal {
  name: string
  fromDomain: 'attr -> 'serialized
  toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>
  get: ('ctx -> 'entity -> Async<'attr option Skippable>) option
  set: ('ctx -> 'attr option -> 'entity -> Async<Result<'entity, Error list>>) option
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>
} with

  static member internal Create(name: string, fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>) : NullableAttribute<'ctx, 'entity, 'attr, 'serialized> =
    {
      name = name
      fromDomain = fromDomain
      toDomain = toDomain
      get = None
      set = None
      hasConstraints = false
      getConstraints = fun _ _ -> async.Return []
    }

  member internal this.nullableFromDomain =
    Option.map this.fromDomain

  member internal this.nullableToDomain =
    fun ctx -> Option.traverseAsyncResult (this.toDomain ctx)

  interface FieldSetter<'ctx> with
    member this.Name = this.name
    member this.Set ctx req entity =
      async {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { attributes = Include attrVals } }) ->
            match this.set, attrVals.TryFind this.name with
            | _, None -> return Ok entity  // not provided in request
            | None, Some _ -> return Error [setAttrReadOnly this.name ("/data/attributes/" + this.name)]
            | Some set, Some attrValue ->
                return!
                  this.nullableToDomain ctx (unbox<'serialized option> attrValue)
                  |> AsyncResult.bind (fun domain -> set ctx domain (unbox<'entity> entity))
                  |> AsyncResult.mapError (List.map (Error.setSourcePointer ("/data/attributes/" + this.name)))
                  |> AsyncResult.map box
        | _ -> return Ok entity  // no attributes provided
      }

  interface Attribute<'ctx> with

    member this.Name = this.name

    member this.BoxedGetSerialized =
      this.get
      |> Option.map (fun get ->
          fun ctx res ->
            get ctx (unbox<'entity> res) |> Async.map (Skippable.map (this.nullableFromDomain >> box))
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
          | Error errs -> Error errs |> async.Return
          | Ok None -> None |> Ok |> async.Return
          | Ok (Some (attrVals, attrsPointer)) ->
              match attrVals.TryGetValue this.name with
              | true, (:? 'serialized as attr) ->
                  this.toDomain ctx attr
                  |> AsyncResult.mapError (List.map (Error.setSourcePointer (attrsPointer + "/" + this.name)))
                  |> AsyncResult.map Some
              | true, x -> failwithf "Framework bug: Expected attribute '%s' to be deserialized to %s, but was %s" this.name typeof<'serialized>.FullName (x.GetType().FullName)
              | false, _ -> None |> Ok |> async.Return
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
      |> AsyncResult.requireSome [reqParserMissingRequiredAttr this.name pointer]

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


  member this.Name = this.name

  // Use Func to help type inference: https://github.com/dotnet/fsharp/issues/8110
  member this.GetAsyncSkip (get: Func<'ctx, 'entity, Async<'attr option Skippable>>) =
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'attr option>>) =
    this.GetAsyncSkip (fun ctx r -> get.Invoke(ctx, r) |> Async.map Include)

  member this.GetAsync (get: Func<'entity, Async<'attr option>>) =
    this.GetAsyncSkip (fun _ r -> get.Invoke r |> Async.map Include)

  member this.GetSkip (get: Func<'ctx, 'entity, 'attr option Skippable>) =
    this.GetAsyncSkip (fun ctx r -> get.Invoke(ctx, r) |> async.Return)

  member this.Get (get: Func<'ctx, 'entity, 'attr option>) =
    this.GetAsyncSkip (fun ctx r -> get.Invoke(ctx, r) |> Include |> async.Return)

  member this.Get (get: Func<'entity, 'attr option>) =
    this.GetAsyncSkip (fun _ r -> get.Invoke r |> Include |> async.Return)

  member this.SetAsyncRes (set: 'ctx -> 'attr option -> 'entity -> Async<Result<'entity, Error list>>) =
    { this with set = Some set }

  member this.SetAsyncRes (set: 'attr option -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetAsyncRes (fun _ x e -> set x e)

  member this.SetAsync (set: 'ctx -> 'attr option -> 'entity -> Async<'entity>) =
    this.SetAsyncRes (fun ctx x e -> set ctx x e |> Async.map Ok)

  member this.SetAsync (set: 'attr option -> 'entity -> Async<'entity>) =
    this.SetAsyncRes (fun _ x e -> set x e |> Async.map Ok)

  member this.SetRes (set: 'ctx -> 'attr option -> 'entity -> Result<'entity, Error list>) =
    this.SetAsyncRes (fun ctx x e -> set ctx x e |> async.Return)

  member this.SetRes (set: 'attr option -> 'entity -> Result<'entity, Error list>) =
    this.SetAsyncRes (fun _ x e -> set x e |> async.Return)

  member this.Set (set: 'ctx -> 'attr option -> 'entity -> 'entity) =
    this.SetAsyncRes (fun ctx x e -> set ctx x e |> Ok |> async.Return)

  member this.Set (set: 'attr option -> 'entity -> 'entity) =
    this.SetAsyncRes (fun _ x e -> set x e |> Ok |> async.Return)

  member this.SetNonNullAsyncRes (set: 'ctx -> 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    { this with
        set = Some (fun ctx attr e ->
          attr
          |> Result.requireSome [setAttrNullNotAllowed this.name]
          |> async.Return
          |> AsyncResult.bind (fun a -> set ctx a e))
    }

  member this.SetNonNullAsyncRes (set: 'attr -> 'entity -> Async<Result<'entity, Error list>>) =
    this.SetNonNullAsyncRes (fun _ x e -> set x e)

  member this.SetNonNullAsync (set: 'ctx -> 'attr -> 'entity -> Async<'entity>) =
    this.SetNonNullAsyncRes (fun ctx x e -> set ctx x e |> Async.map Ok)

  member this.SetNonNullAsync (set: 'attr -> 'entity -> Async<'entity>) =
    this.SetNonNullAsyncRes (fun _ x e -> set x e |> Async.map Ok)

  member this.SetNonNullRes (set: 'ctx -> 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetNonNullAsyncRes (fun ctx x e -> set ctx x e |> async.Return)

  member this.SetNonNullRes (set: 'attr -> 'entity -> Result<'entity, Error list>) =
    this.SetNonNullAsyncRes (fun _ x e -> set x e |> async.Return)

  member this.SetNonNull (set: 'ctx -> 'attr -> 'entity -> 'entity) =
    this.SetNonNullAsyncRes (fun ctx x e -> set ctx x e |> Ok |> async.Return)

  member this.SetNonNull (set: 'attr -> 'entity -> 'entity) =
    this.SetNonNullAsyncRes (fun _ x e -> set x e |> Ok |> async.Return)

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            async {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsAsync(fun ctx e -> getConstraints ctx e |> async.Return)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsAsync(fun ctx e -> [name, box (getValue ctx e)] |> async.Return)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.AddConstraint (name: string, value: 'a) =
    this.AddConstraint(name, fun _ -> value)



type NullableAttributeHelper<'ctx, 'entity> internal () =

  member _.Simple([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NullableAttribute<'ctx, 'entity, 'serialized, 'serialized>.Create(
      name, id, fun _ -> Ok >> async.Return)

  member private _.ParsedAsyncRes'(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NullableAttribute<'ctx, 'entity, 'attr, 'serialized>.Create(name, fromDomain, toDomain)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> AsyncResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> AsyncResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> AsyncResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> AsyncResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Async.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Async.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Async.map Ok), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Async.map Ok), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> async.Return), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.requireSome [attrInvalidParsedNone name] >> async.Return), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Result.requireSome [attrInvalidParsedNone name] >> async.Return), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Ok >> async.Return), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Ok >> async.Return), name)

  member _.Enum(fromDomain: 'attr -> string, toDomainMap: (string * 'attr) list, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let d = dict toDomainMap
    let allowed = toDomainMap |> List.map fst |> List.distinct
    let toDomain serialized =
      serialized
      |> d.TryGetValue
      |> function
          | false, _ -> Error [attrInvalidEnum name serialized allowed]
          | true, attr -> Ok attr
    NullableAttribute<'ctx, 'entity, 'attr, string>.Create(
      name, fromDomain, fun _ -> toDomain >> async.Return)




type AttributeHelper<'ctx, 'entity> internal () =

  member _.Nullable = NullableAttributeHelper<'ctx, 'entity>()

  member _.Simple([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NonNullableAttribute<'ctx, 'entity, 'serialized, 'serialized>.Create(
      name, id, fun _ -> Ok >> async.Return)

  member private _.ParsedAsyncRes'(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    NonNullableAttribute<'ctx, 'entity, 'attr, 'serialized>.Create(name, fromDomain, toDomain)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, toDomain, name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, Error list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> AsyncResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> AsyncResult.mapError (attrInvalidParsedErrMsg name >> List.singleton)), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> AsyncResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedAsyncRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<Result<'attr, string list>>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> AsyncResult.mapError (List.map (attrInvalidParsedErrMsg name))), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Async.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedAsyncOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr option>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Async.map (Result.requireSome [attrInvalidParsedNone name])), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Async.map Ok), name)

  member this.ParsedAsync(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Async<'attr>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Async.map Ok), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, Error list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (attrInvalidParsedErrMsg name >> List.singleton) >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> async.Return), name)

  member this.ParsedRes(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> Result<'attr, string list>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Result.mapError (List.map (attrInvalidParsedErrMsg name)) >> async.Return), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Result.requireSome [attrInvalidParsedNone name] >> async.Return), name)

  member this.ParsedOpt(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr option, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Result.requireSome [attrInvalidParsedNone name] >> async.Return), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'ctx -> 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun ctx -> toDomain ctx >> Ok >> async.Return), name)

  member this.Parsed(fromDomain: 'attr -> 'serialized, toDomain: 'serialized -> 'attr, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    this.ParsedAsyncRes'(fromDomain, (fun _ -> toDomain >> Ok >> async.Return), name)

  member _.Enum(fromDomain: 'attr -> string, toDomainMap: (string * 'attr) list, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let d = dict toDomainMap
    let allowed = toDomainMap |> List.map fst |> List.distinct
    let toDomain serialized =
      match d.TryGetValue serialized with
      | false, _ -> Error [attrInvalidEnum name serialized allowed]
      | true, attr -> Ok attr
    NonNullableAttribute<'ctx, 'entity, 'attr, string>.Create(
      name, fromDomain, fun _ -> toDomain >> async.Return)
