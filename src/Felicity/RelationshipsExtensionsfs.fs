[<AutoOpen>]
module Felicity.RelationshipExtensions

open System.Threading.Tasks



type PolymorphicRelationshipHelper<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> with

    member this.MapSetContextTaskRes(mapSetCtx: 'ctx -> Task<Result<'mappedSetCtx, Error list>>) = {
        mapSetCtx = fun ctx _ -> mapSetCtx ctx
        resolveEntity = this.resolveEntity
        resolveId = this.resolveId
        idParsers = this.idParsers
    }

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



type RelationshipHelper<'ctx, 'setCtx', 'entity> with

    member _.MapSetContextTaskRes(mapSetCtx: 'ctx -> Task<Result<'mappedSetCtx, Error list>>) =
        RelationshipHelper<'ctx, 'mappedSetCtx, 'entity>(fun ctx _ -> mapSetCtx ctx)

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
