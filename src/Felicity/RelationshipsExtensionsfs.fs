[<AutoOpen>]
module Felicity.RelationshipExtensions

open Hopac


type PolymorphicRelationshipHelper<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> with

  member this.MapSetContextJobRes (mapSetCtx: 'ctx -> Job<Result<'mappedSetCtx, Error list>>) =
    {
      mapSetCtx = fun ctx _ -> mapSetCtx ctx
      resolveEntity = this.resolveEntity
      resolveId = this.resolveId
      idParsers = this.idParsers
    }

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



type RelationshipHelper<'ctx, 'setCtx', 'entity> with

  member _.MapSetContextJobRes (mapSetCtx: 'ctx -> Job<Result<'mappedSetCtx, Error list>>) =
    RelationshipHelper<'ctx, 'mappedSetCtx, 'entity>(fun ctx _ -> mapSetCtx ctx)

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
