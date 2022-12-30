[<AutoOpen>]
module Felicity.AttributeExtensions

open System.Threading.Tasks


type AttributeHelper<'ctx, 'setCtx, 'entity> with

    member _.MapSetContextTaskRes(mapSetCtx: 'ctx -> Task<Result<'mappedSetCtx, Error list>>) =
        AttributeHelper<'ctx, 'mappedSetCtx, 'entity>(fun ctx _ -> mapSetCtx ctx)

    member this.MapSetContextAsyncRes(mapSetCtx: 'ctx -> Async<Result<'mappedSetCtx, Error list>>) =
        this.MapSetContextTaskRes(Task.liftAsync mapSetCtx)

    member this.MapSetContextTask(mapSetCtx: 'ctx -> Task<'mappedSetCtx>) =
        this.MapSetContextTaskRes(mapSetCtx >> Task.map Ok)

    member this.MapSetContextAsync(mapSetCtx: 'ctx -> Async<'mappedSetCtx>) =
        this.MapSetContextTask(Task.liftAsync mapSetCtx)

    member this.MapSetContextRes(mapSetCtx: 'ctx -> Result<'mappedSetCtx, Error list>) =
        this.MapSetContextTaskRes(Task.lift mapSetCtx)

    member this.MapSetContext(mapSetCtx: 'ctx -> 'mappedSetCtx) =
        this.MapSetContextTaskRes(TaskResult.lift mapSetCtx)


type NullableAttributeHelper<'ctx, 'setCtx, 'entity> with

    member _.MapSetContextTaskRes(mapSetCtx: 'ctx -> Task<Result<'mappedSetCtx, Error list>>) =
        NullableAttributeHelper<'ctx, 'mappedSetCtx, 'entity>(fun ctx _ -> mapSetCtx ctx)

    member this.MapSetContextAsyncRes(mapSetCtx: 'ctx -> Async<Result<'mappedSetCtx, Error list>>) =
        this.MapSetContextTaskRes(Task.liftAsync mapSetCtx)

    member this.MapSetContextTask(mapSetCtx: 'ctx -> Task<'mappedSetCtx>) =
        this.MapSetContextTaskRes(mapSetCtx >> Task.map Ok)

    member this.MapSetContextAsync(mapSetCtx: 'ctx -> Async<'mappedSetCtx>) =
        this.MapSetContextTask(Task.liftAsync mapSetCtx)

    member this.MapSetContextRes(mapSetCtx: 'ctx -> Result<'mappedSetCtx, Error list>) =
        this.MapSetContextTaskRes(Task.lift mapSetCtx)

    member this.MapSetContext(mapSetCtx: 'ctx -> 'mappedSetCtx) =
        this.MapSetContextTaskRes(TaskResult.lift mapSetCtx)
