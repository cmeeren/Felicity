[<AutoOpen>]
module Felicity.OperationsExtensions

open System.Threading.Tasks
open Errors


type OperationHelper<'originalCtx, 'ctx, 'entity, 'id> with

    member _.ForContextTaskRes(mapCtx: 'originalCtx -> Task<Result<'mappedCtx, Error list>>) =
        OperationHelper<'originalCtx, 'mappedCtx, 'entity, 'id>(mapCtx)

    member this.ForContextAsyncRes(mapCtx: 'originalCtx -> Async<Result<'mappedCtx, Error list>>) =
        this.ForContextTaskRes(Task.liftAsync mapCtx)

    member this.ForContextTaskOpt(mapCtx: 'originalCtx -> Task<'mappedCtx option>) =
        this.ForContextTaskRes(mapCtx >> Task.map (Result.requireSome [ opMapCtxFailedNone () ]))

    member this.ForContextAsyncOpt(mapCtx: 'originalCtx -> Async<'mappedCtx option>) =
        this.ForContextTaskOpt(Task.liftAsync mapCtx)

    member this.ForContextTask(mapCtx: 'originalCtx -> Task<'mappedCtx>) =
        this.ForContextTaskRes(mapCtx >> Task.map Ok)

    member this.ForContextAsync(mapCtx: 'originalCtx -> Async<'mappedCtx>) =
        this.ForContextTask(Task.liftAsync mapCtx)

    member this.ForContextRes(mapCtx: 'originalCtx -> Result<'mappedCtx, Error list>) =
        this.ForContextTaskRes(Task.lift mapCtx)

    member this.ForContextOpt(mapCtx: 'originalCtx -> 'mappedCtx option) =
        this.ForContextTaskOpt(Task.lift mapCtx)

    member this.ForContext(mapCtx: 'originalCtx -> 'mappedCtx) =
        this.ForContextTaskRes(TaskResult.lift mapCtx)
