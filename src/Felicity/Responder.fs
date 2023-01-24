namespace Felicity

open Microsoft.Extensions.DependencyInjection
open Giraffe


type Responder<'ctx> internal (builder: ResponseBuilder<'ctx>, ctx, req) =

    let mutable primaryResourceTypesForFieldTracking = None

    // If you use field tracking (i.e., have called TrackFieldUsage when configuring Felicity) and this responder can be
    // used to return multiple resource types, call this method for each resource type before calling any 'With...'
    // method. It is not necessary to call this method if the responder is only used to return a single resource type.
    member this.RegisterResourceType(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>) =
        match primaryResourceTypesForFieldTracking with
        | None -> primaryResourceTypesForFieldTracking <- [ resourceDef.name ] |> Some
        | Some xs -> primaryResourceTypesForFieldTracking <- resourceDef.name :: xs |> List.distinct |> Some

        this

    member _.WithEntity(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entity: 'entity) : HttpHandler =
        fun next httpCtx -> task {
            let! doc = builder.Write httpCtx ctx req (upcast resourceDef, entity)

            let primaryResourceTypes =
                primaryResourceTypesForFieldTracking |> Option.defaultValue [ resourceDef.name ]

            let! fieldTrackerHandler =
                httpCtx.RequestServices
                    .GetRequiredService<FieldTracker<'ctx>>()
                    .TrackFields(primaryResourceTypes, ctx, req)

            return! (fieldTrackerHandler >=> jsonApiWithETag<'ctx> doc) next httpCtx
        }

    member _.WithEntities(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entities: 'entity list) : HttpHandler =
        fun next httpCtx -> task {
            let! doc = builder.WriteList httpCtx ctx req (entities |> List.map (fun e -> upcast resourceDef, e))

            let primaryResourceTypes =
                primaryResourceTypesForFieldTracking |> Option.defaultValue [ resourceDef.name ]

            let! fieldTrackerHandler =
                httpCtx.RequestServices
                    .GetRequiredService<FieldTracker<'ctx>>()
                    .TrackFields(primaryResourceTypes, ctx, req)

            return! (fieldTrackerHandler >=> jsonApiWithETag<'ctx> doc) next httpCtx
        }

    member _.WithPolymorphicEntities(polyBuilders: PolymorphicBuilder<'ctx> list) : HttpHandler =
        fun next httpCtx -> task {
            let! doc = builder.WriteList httpCtx ctx req (polyBuilders |> List.map (fun b -> b.resourceDef, b.entity))

            let primaryResourceTypes =
                primaryResourceTypesForFieldTracking
                |> Option.defaultValue (polyBuilders |> List.map (fun b -> b.resourceDef.TypeName) |> List.distinct)

            let! fieldTrackerHandler =
                httpCtx.RequestServices
                    .GetRequiredService<FieldTracker<'ctx>>()
                    .TrackFields(primaryResourceTypes, ctx, req)

            return! (fieldTrackerHandler >=> jsonApiWithETag<'ctx> doc) next httpCtx
        }

    member _.WithOptEntity(resourceDef: ResourceDefinition<'ctx, 'entity, 'id>, entity: 'entity option) : HttpHandler =
        fun next httpCtx -> task {
            let! doc = builder.WriteOpt httpCtx ctx req (entity |> Option.map (fun e -> upcast resourceDef, e))

            let primaryResourceTypes =
                primaryResourceTypesForFieldTracking |> Option.defaultValue [ resourceDef.name ]

            let! fieldTrackerHandler =
                httpCtx.RequestServices
                    .GetRequiredService<FieldTracker<'ctx>>()
                    .TrackFields(primaryResourceTypes, ctx, req)

            return! (fieldTrackerHandler >=> jsonApiWithETag<'ctx> doc) next httpCtx
        }
