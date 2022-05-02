namespace Felicity

open Giraffe


type Responder<'ctx> internal (builder: ResponseBuilder<'ctx>, ctx, req) =

  member _.WithEntity(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entity: 'entity) : HttpHandler =
    fun next httpCtx ->
      task {
        let! doc = builder.Write httpCtx ctx req (upcast resourceDef, entity)
        return! jsonApiWithETag<'ctx> doc next httpCtx
      }

  member _.WithEntities(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entities: 'entity list) : HttpHandler =
    fun next httpCtx ->
      task {
        let! doc = builder.WriteList httpCtx ctx req (entities |> List.map (fun e -> upcast resourceDef, e))
        return! jsonApiWithETag<'ctx> doc next httpCtx
      }

  member _.WithPolymorphicEntities(polyBuilders: PolymorphicBuilder<'ctx> list) : HttpHandler =
    fun next httpCtx ->
      task {
        let! doc = builder.WriteList httpCtx ctx req (polyBuilders |> List.map (fun b -> b.resourceDef, b.entity))
        return! jsonApiWithETag<'ctx> doc next httpCtx
      }

  member _.WithOptEntity(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entity: 'entity option) : HttpHandler =
    fun next httpCtx ->
      task {
        let! doc = builder.WriteOpt httpCtx ctx req (entity |> Option.map (fun e -> upcast resourceDef, e))
        return! jsonApiWithETag<'ctx> doc next httpCtx
      }
