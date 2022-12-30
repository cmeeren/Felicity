namespace Felicity

open System.Threading.Tasks
open Errors


type Id<'ctx, 'entity, 'id> =
    internal
        {
            fromDomain: 'id -> string
            toDomain: 'ctx -> ResourceId -> Task<Result<'id, (ParsedValueInfo -> Error) list>>
            getId: 'entity -> 'id
        }

    static member internal Create
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Task<Result<'id, (ParsedValueInfo -> Error) list>>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
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
                | Error errs -> Error errs |> Task.result
                | Ok None -> None |> Ok |> Task.result
                | Ok(Some(resId, pointer)) ->
                    let valueInfo = FromBodyId { Value = resId }

                    this.toDomain ctx resId
                    |> TaskResult.mapError (List.map (fun getErr -> getErr valueInfo |> Error.setSourcePointer pointer))
                    |> TaskResult.map Some
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
            |> TaskResult.requireSome [ reqParserMissingRequiredId pointer ]


type IdHelper<'ctx, 'entity, 'id> internal () =

    member _.Simple(getId: 'entity -> string) : Id<'ctx, 'entity, string> =
        Id<'ctx, 'entity, string>.Create (id, (fun _ -> Ok >> Task.result), getId)

    member private _.ParsedTaskRes'
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Task<Result<'id, (ParsedValueInfo -> Error) list>>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        Id<'ctx, 'entity, 'id>.Create (fromDomain, toDomain, getId)

    member _.ParsedTaskRes
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Task<Result<'id, Error list>>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        Id<'ctx, 'entity, 'id>.Create
            (fromDomain, (fun ctx v -> toDomain ctx v |> TaskResult.mapError (List.map (fun err _ -> err))), getId)

    member this.ParsedAsyncRes
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Async<Result<'id, Error list>>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes(fromDomain, Task.liftAsync2 toDomain, getId)

    member this.ParsedTaskOpt
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Task<'id option>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun ctx -> toDomain ctx >> Task.map (Result.requireSome [ invalidParsedNone ])),
            getId
        )

    member this.ParsedTaskOpt
        (
            fromDomain: 'id -> string,
            toDomain: string -> Task<'id option>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun _ctx -> toDomain >> Task.map (Result.requireSome [ invalidParsedNone ])),
            getId
        )

    member this.ParsedAsyncOpt
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Async<'id option>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun ctx ->
                toDomain ctx
                >> Task.fromAsync
                >> Task.map (Result.requireSome [ invalidParsedNone ])),
            getId
        )

    member this.ParsedAsyncOpt
        (
            fromDomain: 'id -> string,
            toDomain: string -> Async<'id option>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun _ctx ->
                toDomain
                >> Task.fromAsync
                >> Task.map (Result.requireSome [ invalidParsedNone ])),
            getId
        )

    member this.ParsedTask
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Task<'id>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(fromDomain, (fun ctx -> toDomain ctx >> Task.map Ok), getId)

    member this.ParsedAsync
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Async<'id>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(fromDomain, (fun ctx -> toDomain ctx >> Task.fromAsync >> Task.map Ok), getId)

    member this.ParsedRes
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Result<'id, Error list>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes(fromDomain, (fun ctx -> toDomain ctx >> Task.result), getId)

    member this.ParsedRes
        (
            fromDomain: 'id -> string,
            toDomain: string -> Result<'id, Error list>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes(fromDomain, (fun _ctx -> toDomain >> Task.result), getId)

    member this.ParsedRes
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Result<'id, string>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun ctx ->
                toDomain ctx
                >> Result.mapError (flip invalidParsedErrMsg >> List.singleton)
                >> Task.result),
            getId
        )

    member this.ParsedRes
        (
            fromDomain: 'id -> string,
            toDomain: string -> Result<'id, string>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun _ctx ->
                toDomain
                >> Result.mapError (flip invalidParsedErrMsg >> List.singleton)
                >> Task.result),
            getId
        )

    member this.ParsedRes
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> Result<'id, string list>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun ctx ->
                toDomain ctx
                >> Result.mapError (List.map (flip invalidParsedErrMsg))
                >> Task.result),
            getId
        )

    member this.ParsedRes
        (
            fromDomain: 'id -> string,
            toDomain: string -> Result<'id, string list>,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun _ctx -> toDomain >> Result.mapError (List.map (flip invalidParsedErrMsg)) >> Task.result),
            getId
        )

    member this.ParsedOpt
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> 'id option,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun ctx -> toDomain ctx >> Result.requireSome [ invalidParsedNone ] >> Task.result),
            getId
        )

    member this.ParsedOpt
        (
            fromDomain: 'id -> string,
            toDomain: string -> 'id option,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(
            fromDomain,
            (fun _ctx -> toDomain >> Result.requireSome [ invalidParsedNone ] >> Task.result),
            getId
        )

    member this.Parsed
        (
            fromDomain: 'id -> string,
            toDomain: 'ctx -> string -> 'id,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(fromDomain, (fun ctx -> toDomain ctx >> Ok >> Task.result), getId)

    member this.Parsed
        (
            fromDomain: 'id -> string,
            toDomain: string -> 'id,
            getId: 'entity -> 'id
        ) : Id<'ctx, 'entity, 'id> =
        this.ParsedTaskRes'(fromDomain, (fun _ctx -> toDomain >> Ok >> Task.result), getId)
