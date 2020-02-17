namespace Felicity

open Errors


type Id<'ctx, 'entity, 'id> = internal {
  fromDomain: 'id -> string
  toDomain: 'ctx -> ResourceId -> Async<Result<'id, Error list>>
  getId: 'entity -> 'id
} with

  static member internal Create(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Async<Result<'id, Error list>>, getId: 'entity -> 'id) : Id<'ctx, 'entity, 'id> =
    {
      fromDomain = fromDomain
      toDomain = toDomain
      getId = getId
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'id option> with
        member _.FieldName = Some "id"
        member _.QueryParamName = None
        member _.Get(ctx, req) =
          match req.Document.Value with
          | Error errs -> Error errs |> async.Return
          | Ok (Some { data = Some { id = Include id } }) ->
              this.toDomain ctx id
              |> AsyncResult.mapError (List.map (Error.setSourcePointer "/data/id"))
              |> AsyncResult.map Some
          | _ -> None |> Ok |> async.Return
    }

  interface OptionalRequestGetter<'ctx, 'id> with
    member _.FieldName = Some "id"
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      this.Optional.Get(ctx, req)

  interface RequestGetter<'ctx, 'id> with
    member _.FieldName = Some "id"
    member this.QueryParamName = None
    member this.Get(ctx, req) =
      let pointer =
        match req.Document.Value with
        | Error _ -> ""  // Won't be used
        | Ok None -> ""
        | Ok (Some { data = None }) -> ""
        | Ok (Some { data = Some { id = Skip } }) -> "/data"
        | Ok (Some { data = Some { id = Include _ } }) -> ""  // Won't be used
      this.Optional.Get(ctx, req)
      |> AsyncResult.requireSome [reqParserMissingRequiredId pointer]

      
type IdHelper<'ctx, 'entity, 'id> internal () =

  member _.Simple(getId: 'entity -> string) =
    Id<'ctx, 'entity, string>.Create(
      id, (fun ctx -> Ok >> async.Return), getId)

  member _.ParsedAsyncRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Async<Result<'id, Error list>>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, toDomain, getId)

  member _.ParsedAsyncOpt(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Async<'id option>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain ctx >> Async.map (Result.requireSome [idInvalidParsedNone])), getId)

  member _.ParsedAsyncOpt(fromDomain: 'id -> string, toDomain: string -> Async<'id option>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain >> Async.map (Result.requireSome [idInvalidParsedNone])), getId)

  member _.ParsedAsync(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Async<'id>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain ctx >> Async.map Ok), getId)

  member _.ParsedRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Result<'id, Error list>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain ctx >> async.Return), getId)

  member _.ParsedRes(fromDomain: 'id -> string, toDomain: string -> Result<'id, Error list>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain >> async.Return), getId)

  member _.ParsedRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Result<'id, string>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (idInvalidErrMsg >> List.singleton) >> async.Return), getId)

  member _.ParsedRes(fromDomain: 'id -> string, toDomain: string -> Result<'id, string>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain >> Result.mapError (idInvalidErrMsg >> List.singleton) >> async.Return), getId)

  member _.ParsedRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Result<'id, string list>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (List.map idInvalidErrMsg) >> async.Return), getId)

  member _.ParsedRes(fromDomain: 'id -> string, toDomain: string -> Result<'id, string list>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain >> Result.mapError (List.map idInvalidErrMsg) >> async.Return), getId)

  member _.ParsedOpt(fromDomain: 'id -> string, toDomain: 'ctx -> string -> 'id option, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain ctx >> Result.requireSome [idInvalidParsedNone] >> async.Return), getId)

  member _.ParsedOpt(fromDomain: 'id -> string, toDomain: string -> 'id option, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain >> Result.requireSome [idInvalidParsedNone] >> async.Return), getId)

  member _.Parsed(fromDomain: 'id -> string, toDomain: 'ctx -> string -> 'id, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain ctx >> Ok >> async.Return), getId)

  member _.Parsed(fromDomain: 'id -> string, toDomain: string -> 'id, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(
      fromDomain, (fun ctx -> toDomain >> Ok >> async.Return), getId)
