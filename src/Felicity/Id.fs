namespace Felicity

open Hopac
open Errors


type Id<'ctx, 'entity, 'id> = internal {
  fromDomain: 'id -> string
  toDomain: 'ctx -> ResourceId -> Job<Result<'id, Error list>>
  getId: 'entity -> 'id
} with

  static member internal Create(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Job<Result<'id, Error list>>, getId: 'entity -> 'id) : Id<'ctx, 'entity, 'id> =
    {
      fromDomain = fromDomain
      toDomain = toDomain
      getId = getId
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'id option> with
        member _.FieldName = Some "id"
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getIdAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Job.result
          | Ok None -> None |> Ok |> Job.result
          | Ok (Some (resId, pointer)) ->
              this.toDomain ctx resId
              |> JobResult.mapError (List.map (Error.setSourcePointer pointer))
              |> JobResult.map Some
    }

  interface OptionalRequestGetter<'ctx, 'id> with
    member _.FieldName = Some "id"
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'id> with
    member _.FieldName = Some "id"
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingId includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredId pointer]

      
type IdHelper<'ctx, 'entity, 'id> internal () =

  member _.Simple(getId: 'entity -> string) =
    Id<'ctx, 'entity, string>.Create(id, (fun _ -> Ok >> Job.result), getId)

  member _.ParsedJobRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Job<Result<'id, Error list>>, getId: 'entity -> 'id) =
    Id<'ctx, 'entity, 'id>.Create(fromDomain, toDomain, getId)

  member this.ParsedAsyncRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Async<Result<'id, Error list>>, getId: 'entity -> 'id) =
    this.ParsedJobRes(fromDomain, Job.liftAsync2 toDomain, getId)

  member this.ParsedJobOpt(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Job<'id option>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Job.map (Result.requireSome [idInvalidParsedNone])), getId)

  member this.ParsedJobOpt(fromDomain: 'id -> string, toDomain: string -> Job<'id option>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain >> Job.map (Result.requireSome [idInvalidParsedNone])), getId)

  member this.ParsedAsyncOpt(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Async<'id option>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Job.fromAsync >> Job.map (Result.requireSome [idInvalidParsedNone])), getId)

  member this.ParsedAsyncOpt(fromDomain: 'id -> string, toDomain: string -> Async<'id option>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain >> Job.fromAsync >> Job.map (Result.requireSome [idInvalidParsedNone])), getId)

  member this.ParsedJob(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Job<'id>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Job.map Ok), getId)

  member this.ParsedAsync(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Async<'id>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Job.fromAsync >> Job.map Ok), getId)

  member this.ParsedRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Result<'id, Error list>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Job.result), getId)

  member this.ParsedRes(fromDomain: 'id -> string, toDomain: string -> Result<'id, Error list>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain >> Job.result), getId)

  member this.ParsedRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Result<'id, string>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (idInvalidErrMsg >> List.singleton) >> Job.result), getId)

  member this.ParsedRes(fromDomain: 'id -> string, toDomain: string -> Result<'id, string>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain >> Result.mapError (idInvalidErrMsg >> List.singleton) >> Job.result), getId)

  member this.ParsedRes(fromDomain: 'id -> string, toDomain: 'ctx -> string -> Result<'id, string list>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Result.mapError (List.map idInvalidErrMsg) >> Job.result), getId)

  member this.ParsedRes(fromDomain: 'id -> string, toDomain: string -> Result<'id, string list>, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain >> Result.mapError (List.map idInvalidErrMsg) >> Job.result), getId)

  member this.ParsedOpt(fromDomain: 'id -> string, toDomain: 'ctx -> string -> 'id option, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Result.requireSome [idInvalidParsedNone] >> Job.result), getId)

  member this.ParsedOpt(fromDomain: 'id -> string, toDomain: string -> 'id option, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain >> Result.requireSome [idInvalidParsedNone] >> Job.result), getId)

  member this.Parsed(fromDomain: 'id -> string, toDomain: 'ctx -> string -> 'id, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain ctx >> Ok >> Job.result), getId)

  member this.Parsed(fromDomain: 'id -> string, toDomain: string -> 'id, getId: 'entity -> 'id) =
    this.ParsedJobRes(
      fromDomain, (fun ctx -> toDomain >> Ok >> Job.result), getId)
