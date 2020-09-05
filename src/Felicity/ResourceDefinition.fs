namespace Felicity

open System
open System.Threading.Tasks
open Hopac


type ResourceDefinition<'ctx> =
  abstract TypeName: ResourceTypeName
  abstract CollectionName: CollectionName option
  abstract GetIdBoxed: BoxedEntity -> ResourceId
  abstract ParseIdBoxed: 'ctx -> ResourceId -> Job<Result<BoxedDomainId, Error list>>


type internal LockSpecification<'ctx> = {
  Timeout: TimeSpan
  CollName: CollectionName
  GetId: 'ctx -> Request -> ResourceId option -> Job<ResourceId option>
  CustomLock: ('ctx -> ResourceId -> Job<IDisposable option>) option
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

  member private this.CreateLockSpec(?timeout) =
    {
      Timeout = defaultArg timeout (TimeSpan.FromSeconds 10.)
      CollName =
        this.collectionName
        |> Option.defaultWith (fun () -> failwithf "Resource type %s does not have a collection name and can therefore not be used for locking" this.name)
      GetId = fun _ _ resId -> resId |> Job.result
      CustomLock = None
    }

  member private this.CreateOtherLockSpec(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'ctx -> 'id -> Job<'otherId option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>, ?timeout) =
    {
      Timeout = defaultArg timeout (TimeSpan.FromSeconds 10.)
      CollName =
        resDef.collectionName
        |> Option.defaultWith (fun () -> failwithf "Resource type %s does not have a collection name and can therefore not be used for locking" resDef.name)
      GetId =
        fun ctx req thisIdRaw ->
          job {
            match thisIdRaw with
            | Some thisIdRaw ->
                // We have a raw ID from a resource-specific endpoint
                match! this.id.toDomain ctx thisIdRaw with
                | Error _ -> return None
                | Ok thisIdDomain ->
                    match! getId ctx thisIdDomain with
                    | None -> return None
                    | Some otherIdDomain ->
                        return Some (resDef.id.fromDomain otherIdDomain)
            | None ->
                // POST collection request, so we must fetch the ID from the request body if the
                // relationship is specified
                match relationship with
                | None -> return None
                | Some rel ->
                    return!
                      rel.Get(ctx, req, None)
                      |> Job.map (Option.fromResult >> Option.bind id >> Option.map resDef.id.fromDomain)
          }
      CustomLock = None
    }

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates. If a lock can not be acquired after the specified timeout
  /// (default 10 seconds), an error will be returned.
  member this.Lock(?timeout) =
    let lockSpec = this.CreateLockSpec(?timeout=timeout)
    { this with lockSpec = Some lockSpec }

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates, using an external lock mechanism. The getLock function
  /// is passed the resource ID, and should return None if the lock times out, or Some
  /// with an IDisposable that releases the lock when disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> Job<IDisposable option>) =
    let lockSpec = this.CreateLockSpec()
    let getLock ctx rawId =
      job {
        match! this.id.toDomain ctx rawId with
        | Error _ -> return None  // Request will fail anyway
        | Ok id -> return! getLock ctx id
      }
    { this with lockSpec = Some { lockSpec with CustomLock = Some getLock } }

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
  /// If a lock can not be acquired after the specified timeout (default 10 seconds), an
  /// error will be returned. The optional relationship must be specified in order to
  /// lock POST collection requests for this resource.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'ctx -> 'id -> Job<'otherId option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>, ?timeout) =
    let lockSpec = this.CreateOtherLockSpec(resDef, getId, ?relationship=relationship, ?timeout=timeout)
    { this with lockSpec = Some lockSpec }

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// If a lock can not be acquired after the specified timeout (default 10 seconds), an
  /// error will be returned. The optional relationship must be specified in order to
   /// lock POST collection requests for this resource.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'id -> Job<'otherId option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>, ?timeout) =
    this.LockOther(resDef, (fun _ -> getId), ?relationship=relationship, ?timeout=timeout)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// If a lock can not be acquired after the specified timeout (default 10 seconds), an
  /// error will be returned. The optional relationship must be specified in order to
   /// lock POST collection requests for this resource.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'id -> Async<'otherId option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>, ?timeout) =
    this.LockOther(resDef, Job.liftAsync2 (fun _ -> getId), ?relationship=relationship, ?timeout=timeout)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// If a lock can not be acquired after the specified timeout (default 10 seconds), an
  /// error will be returned. The optional relationship must be specified in order to
  /// lock POST collection requests for this resource. 
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'ctx -> 'id -> Async<'otherId option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>, ?timeout) =
    this.LockOther(resDef, Job.liftAsync2 getId, ?relationship=relationship, ?timeout=timeout)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource,
  /// using an external lock mechanism. The getLock function is passed the resource ID,
  /// and should return None if the lock times out, or Some with an IDisposable that
  /// releases the lock when disposed. The optional relationship must be specified in
  /// order to lock POST collection requests for this resource.
  member this.CustomLockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'ctx -> 'id -> Job<'otherId option>, getLock: 'ctx -> 'otherId -> Job<IDisposable option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>) =
    let lockSpec = this.CreateOtherLockSpec(resDef, getId, ?relationship=relationship)
    let getLock ctx rawId =
      job {
        match! resDef.id.toDomain ctx rawId with
        | Error _ -> return None  // Request will fail anyway
        | Ok id -> return! getLock ctx id
      }
    { this with lockSpec = Some { lockSpec with CustomLock = Some getLock } }

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource,
  /// using an external lock mechanism. The getLock function is passed the resource ID,
  /// and should return None if the lock times out, or Some with an IDisposable that
  /// releases the lock when disposed. The optional relationship must be specified in
  /// order to lock POST collection requests for this resource.
  member this.CustomLockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'ctx -> 'id -> Async<'otherId option>, getLock: 'ctx -> 'otherId -> Async<IDisposable option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>) =
    this.CustomLockOther(resDef, Job.liftAsync2 getId, Job.liftAsync2 getLock, ?relationship=relationship)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource,
  /// using an external lock mechanism. The getLock function is passed the resource ID,
  /// and should return None if the lock times out, or Some with an IDisposable that
  /// releases the lock when disposed. The optional relationship must be specified in
  /// order to lock POST collection requests for this resource.
  member this.CustomLockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'id -> Job<'otherId option>, getLock: 'otherId -> Job<IDisposable option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>) =
    this.CustomLockOther(resDef, (fun _ id -> getId id), (fun _ id -> getLock id), ?relationship=relationship)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource,
  /// using an external lock mechanism. The getLock function is passed the resource ID,
  /// and should return None if the lock times out, or Some with an IDisposable that
  /// releases the lock when disposed. The optional relationship must be specified in
  /// order to lock POST collection requests for this resource.
  member this.CustomLockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'id -> Async<'otherId option>, getLock: 'otherId -> Async<IDisposable option>, ?relationship: OptionalRequestGetter<'ctx, 'otherId>) =
    this.CustomLockOther(resDef, Job.liftAsync2 (fun _ id -> getId id), Job.liftAsync2 (fun _ id -> getLock id), ?relationship=relationship)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource,
  /// using an external lock mechanism. The getLock function is passed the resource ID,
  /// and should return None if the lock times out, or Some with an IDisposable that
  /// releases the lock when disposed. The optional relationship must be specified in
  /// order to lock POST collection requests for this resource.
  member this.CustomLockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'id -> Job<'otherId option>, getLock: 'otherId -> IDisposable option, ?relationship: OptionalRequestGetter<'ctx, 'otherId>) =
    this.CustomLockOther(resDef, (fun _ id -> getId id), Job.lift2 (fun _ id -> getLock id), ?relationship=relationship)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource,
  /// using an external lock mechanism. The getLock function is passed the resource ID,
  /// and should return None if the lock times out, or Some with an IDisposable that
  /// releases the lock when disposed. The optional relationship must be specified in
  /// order to lock POST collection requests for this resource.
  member this.CustomLockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, getId: 'id -> Async<'otherId option>, getLock: 'otherId -> IDisposable option, ?relationship: OptionalRequestGetter<'ctx, 'otherId>) =
    this.CustomLockOther(resDef, Job.liftAsync2 (fun _ id -> getId id), Job.lift2 (fun _ id -> getLock id), ?relationship=relationship)
