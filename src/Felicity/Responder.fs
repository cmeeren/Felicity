namespace Felicity

open Hopac
open Giraffe


type Responder<'ctx> internal (builder: ResponseBuilder<'ctx>, ctx, req) =

  member _.WithEntity(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entity: 'entity) : HttpHandler =
    fun next httpCtx ->
      job {
        let! doc = builder.Write ctx req (upcast resourceDef, entity)
        return! jsonApiWithETag doc next httpCtx
      }
      |> Job.startAsTask

  member _.WithEntities(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entities: 'entity list) : HttpHandler =
    fun next httpCtx ->
      job {
        let! doc = builder.WriteList ctx req (entities |> List.map (fun e -> upcast resourceDef, e))
        return! jsonApiWithETag doc next httpCtx
      }
      |> Job.startAsTask

  member _.WithPolymorphicEntities(polyBuilders: PolymorphicBuilder<'ctx> list) : HttpHandler =
    fun next httpCtx ->
      job {
        let! doc = builder.WriteList ctx req (polyBuilders |> List.map (fun b -> b.resourceDef, b.entity))
        return! jsonApiWithETag doc next httpCtx
      }
      |> Job.startAsTask

  member _.WithOptEntity(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entity: 'entity option) : HttpHandler =
    fun next httpCtx ->
      job {
        let! doc = builder.WriteOpt ctx req (entity |> Option.map (fun e -> upcast resourceDef, e))
        return! jsonApiWithETag doc next httpCtx
      }
      |> Job.startAsTask
