namespace Felicity

open System
open Microsoft.AspNetCore.Http
open Hopac
open Giraffe
open Errors


type ResourceDefinition<'ctx> =
  abstract TypeName: ResourceTypeName
  abstract CollectionName: CollectionName option
  abstract GetIdBoxed: BoxedEntity -> ResourceId
  abstract ParseIdBoxed: 'ctx -> ResourceId -> Job<Result<BoxedDomainId, Error list>>



type internal FelicityLockSpec<'ctx> = {
  CollName: CollectionName
  Timeout: TimeSpan
}


type internal CustomLockSpec<'ctx> = {
  CustomLock: 'ctx -> ResourceId -> Job<IDisposable option>
}


type internal OtherLockSpec<'ctx> = {
  GetOtherIdFromThisId: 'ctx -> ResourceId -> Job<ResourceId option>
  GetOtherIdFromRelationship: 'ctx -> Request -> Job<ResourceId option>
  OtherLockSpec: LockSpecification<'ctx>
}


and internal LockSpecification<'ctx> =
  | Felicity of FelicityLockSpec<'ctx>
  | Custom of CustomLockSpec<'ctx>
  | Other of OtherLockSpec<'ctx>


module internal LockSpecification =


  let rec lock (httpCtx: HttpContext) ctx req (resId: ResourceId option) (lockSpec: LockSpecification<'ctx>) =
    job {
      match lockSpec, resId with
      | Felicity _, None ->
          return Ok None
      | Felicity lockSpec, Some resId ->
          let queueFactory = httpCtx.GetService<SemaphoreQueueFactory<'ctx>>()
          let queue = queueFactory.GetFor(lockSpec.CollName, resId)
          match! queue.Lock lockSpec.Timeout with
          | Some lock -> return lock |> Some |> Ok
          | None -> return Error [lockTimeout]
      | Custom _, None ->
          return Ok None
      | Custom lockSpec, Some resId ->
          match! lockSpec.CustomLock ctx resId with
          | Some lock -> return lock |> Some |> Ok
          | None -> return Error [lockTimeout]
      | Other lockSpec, None ->
          let! otherId = lockSpec.GetOtherIdFromRelationship ctx req
          return! lock httpCtx ctx req otherId lockSpec.OtherLockSpec
      | Other lockSpec, Some resId ->
          let! otherId = lockSpec.GetOtherIdFromThisId ctx resId
          return! lock httpCtx ctx req otherId lockSpec.OtherLockSpec
    }




type internal ResourceDefinitionLockSpec<'ctx> =
  abstract CollName: CollectionName option
  abstract LockSpec: LockSpecification<'ctx> option



type ResourceDefinition<'ctx, 'id> =
  abstract TypeName: ResourceTypeName
  abstract ParseId: 'ctx -> ResourceId -> Job<Result<'id, Error list>>


[<Struct>]
type PolymorphicBuilder<'ctx> = internal {
  resourceDef: ResourceDefinition<'ctx>
  entity: obj
}


type ResourceDefinition<'ctx, 'entity, 'id> = internal {
  name: string
  collectionName: string option
  id: Id<'ctx, 'entity, 'id>
  lockSpec: LockSpecification<'ctx> option
} with
  static member internal Create(name: string, id: Id<'ctx, 'entity, 'id>) : ResourceDefinition<'ctx, 'entity, 'id> =
    {
      name = name
      collectionName = None
      id = id
      lockSpec = None
    }

  interface ResourceDefinition<'ctx> with
    member this.TypeName = this.name
    member this.CollectionName = this.collectionName
    member this.GetIdBoxed entity = unbox entity |> this.id.getId |> this.id.fromDomain
    member this.ParseIdBoxed ctx rawId =
      this.id.toDomain ctx rawId
      |> JobResult.map box

  interface ResourceDefinitionLockSpec<'ctx> with
    member this.CollName = this.collectionName
    member this.LockSpec = this.lockSpec

  interface ResourceDefinition<'ctx, 'id> with
    member this.TypeName = this.name
    member this.ParseId ctx rawId =
      this.id.toDomain ctx rawId


  member this.CollectionName (collectionName) =
    { this with collectionName = Some collectionName }

  member this.PolymorphicFor (entity: 'entity) =
    { resourceDef = this; entity = box entity }

  member private this.CreateFelicityLockSpec(?timeout) =
    Felicity {
      CollName = this.collectionName |> Option.defaultValue ("/" + this.name)
      Timeout = defaultArg timeout (TimeSpan.FromSeconds 10.)
    }

  member private this.CreateCustomLockSpec(getLock: 'ctx -> 'id -> Job<IDisposable option>) =
    Custom {
      CustomLock =
        fun ctx rawId ->
          job {
            match! this.id.toDomain ctx rawId with
            | Error _ -> return None  // Request will fail anyway
            | Ok id -> return! getLock ctx id
          }
    }

  member private this.CreateOtherLockSpec(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> Job<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    Other {
      GetOtherIdFromThisId =
        fun ctx resId ->
          job {
            match! this.id.toDomain ctx resId with
            | Error _ -> return None  // Request will fail anyway
            | Ok thisId ->
                let getId = getOtherIdForResourceOperation |> Option.defaultValue (fun _ _ -> None |> Job.result)
                let! otherId = getId ctx thisId
                return otherId |> Option.map resDef.id.fromDomain
          }
      GetOtherIdFromRelationship =
        fun ctx req ->
          job {
            match relationshipForPostOperation with
            | None -> return None
            | Some rel ->
                match! rel.Get(ctx, req, None) with
                | Error _ -> return None  
                | Ok otherId -> return otherId |> Option.map resDef.id.fromDomain
          }
      OtherLockSpec =
        resDef.lockSpec
        |> Option.defaultWith (fun () -> failwithf "Resource type '%s' can not lock other resource '%s' because the other resource does not have a lock specification" this.name resDef.name)
    }

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates. If a lock can not be acquired after the specified timeout
  /// (default 10 seconds), an error will be returned.
  member this.Lock(?timeout) =
    { this with lockSpec = this.CreateFelicityLockSpec(?timeout=timeout) |> Some }

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates, using an external lock mechanism. The getLock function
  /// is passed the resource ID, and should return None if the lock times out, or Some
  /// with an IDisposable that releases the lock when disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> Job<IDisposable option>) =
    { this with lockSpec = this.CreateCustomLockSpec(getLock) |> Some }

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates, using an external lock mechanism. The getLock function
  /// is passed the resource ID, and should return None if the lock times out, or Some
  /// with an IDisposable that releases the lock when disposed.
  member this.CustomLock(getLock: 'id -> Job<IDisposable option>) =
    this.CustomLock(fun _ id -> getLock id)

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates, using an external lock mechanism. The getLock function
  /// is passed the resource ID, and should return None if the lock times out, or Some
  /// with an IDisposable that releases the lock when disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> Async<IDisposable option>) =
    this.CustomLock(Job.liftAsync2 getLock)

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates, using an external lock mechanism. The getLock function
  /// is passed the resource ID, and should return None if the lock times out, or Some
  /// with an IDisposable that releases the lock when disposed.
  member this.CustomLock(getLock: 'id -> Async<IDisposable option>) =
    this.CustomLock(Job.liftAsync getLock)

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates, using an external lock mechanism. The getLock function
  /// is passed the resource ID, and should return None if the lock times out, or Some
  /// with an IDisposable that releases the lock when disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> IDisposable option) =
    this.CustomLock(Job.lift2 getLock)

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates, using an external lock mechanism. The getLock function
  /// is passed the resource ID, and should return None if the lock times out, or Some
  /// with an IDisposable that releases the lock when disposed.
  member this.CustomLock(getLock: 'id -> IDisposable option) =
    this.CustomLock(Job.lift getLock)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified. If a lock can not be acquired after the specified timeout (default 10
  /// seconds), an error will be returned.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> Job<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    if getOtherIdForResourceOperation.IsNone && relationshipForPostOperation.IsNone then
      failwithf "At least one of 'getOtherIdForResourceOperation' and 'relationshipForPostOperation' must be specified in call to LockOther for resource '%s'" this.name
    { this with lockSpec = this.CreateOtherLockSpec(resDef, ?getOtherIdForResourceOperation = getOtherIdForResourceOperation, ?relationshipForPostOperation = relationshipForPostOperation) |> Some }

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified. If a lock can not be acquired after the specified timeout (default 10
  /// seconds), an error will be returned.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'id -> Job<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> fun _ -> getId)),
      ?relationshipForPostOperation=relationshipForPostOperation
    )

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified. If a lock can not be acquired after the specified timeout (default 10
  /// seconds), an error will be returned.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'id -> Async<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.liftAsync2 (fun _ -> getId))),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified. If a lock can not be acquired after the specified timeout (default 10
  /// seconds), an error will be returned.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> Async<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.liftAsync2 getId)),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified. If a lock can not be acquired after the specified timeout (default 10
  /// seconds), an error will be returned.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> 'otherId option, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.lift2 getId)),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified. If a lock can not be acquired after the specified timeout (default 10
  /// seconds), an error will be returned.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> 'otherId, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.lift2 (fun ctx id -> getId ctx id |> Some))),
      ?relationshipForPostOperation=relationshipForPostOperation)
