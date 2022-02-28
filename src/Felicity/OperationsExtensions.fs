[<AutoOpen>]
module Felicity.OperationsExtensions

open Hopac
open Errors


type OperationHelper<'originalCtx, 'ctx, 'entity, 'id> with

  member _.ForContextJobRes (mapCtx: 'originalCtx -> Job<Result<'mappedCtx, Error list>>) =
    OperationHelper<'originalCtx, 'mappedCtx, 'entity, 'id>(mapCtx)

  member this.ForContextAsyncRes (mapCtx: 'originalCtx -> Async<Result<'mappedCtx, Error list>>) =
    this.ForContextJobRes(Job.liftAsync mapCtx)

  member this.ForContextJobOpt (mapCtx: 'originalCtx -> Job<'mappedCtx option>) =
    this.ForContextJobRes(mapCtx >> Job.map (Result.requireSome [opMapCtxFailedNone ()]))

  member this.ForContextAsyncOpt (mapCtx: 'originalCtx -> Async<'mappedCtx option>) =
    this.ForContextJobOpt(Job.liftAsync mapCtx)

  member this.ForContextJob (mapCtx: 'originalCtx -> Job<'mappedCtx>) =
    this.ForContextJobRes(mapCtx >> Job.map Ok)

  member this.ForContextAsync (mapCtx: 'originalCtx -> Async<'mappedCtx>) =
    this.ForContextJob(Job.liftAsync mapCtx)

  member this.ForContextRes (mapCtx: 'originalCtx -> Result<'mappedCtx, Error list>) =
    this.ForContextJobRes(Job.lift mapCtx)

  member this.ForContextOpt (mapCtx: 'originalCtx -> 'mappedCtx option) =
    this.ForContextJobOpt(Job.lift mapCtx)

  member this.ForContext (mapCtx: 'originalCtx -> 'mappedCtx) =
    this.ForContextJobRes(JobResult.lift mapCtx)
