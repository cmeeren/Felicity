namespace Felicity

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
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
  CustomLock: 'ctx -> ResourceId -> Task<IDisposable option>
}


type internal CustomResourceCreationLockSpec<'ctx> = {
  CustomLock: 'ctx -> Task<IDisposable option>
}


type internal OtherLockSpec<'ctx> = {
  GetOtherIdFromThisId: 'ctx -> ResourceId -> Job<ResourceId option>
  GetOtherIdFromRelationship: 'ctx -> Request -> Job<ResourceId option>
  OtherLockSpecs: LockSpecification<'ctx> list
}


and internal LockSpecification<'ctx> =
  | Felicity of FelicityLockSpec<'ctx>
  | Custom of CustomLockSpec<'ctx>
  | CustomResourceCreation of CustomResourceCreationLockSpec<'ctx>
  | Other of OtherLockSpec<'ctx>


module internal LockSpecification =


  type MultiLockState () =

    let disps = ResizeArray<IDisposable>()

    let locker = obj ()

    let mutable disposed = false
    let mutable timedOut = false

    member _.TimedOut = timedOut

    member _.AddLock(disp: IDisposable) =
      lock locker (fun () ->
        if disposed then
          disp.Dispose ()
        else
          disps.Add disp
      )

    member this.SetTimedOut() =
      lock locker (fun () ->
        if not timedOut then
          timedOut <- true
          (this :> IDisposable).Dispose ()
      )

    interface IDisposable with
      member _.Dispose () =
        lock locker (fun () ->
          if not disposed then
            disposed <- true
            for disp in disps do
              try disp.Dispose ()
              with _ -> ()
        )


  let rec private lock
      (state: MultiLockState)
      (httpCtx: HttpContext)
      ctx
      req
      (resId: ResourceId option)
      (lockSpec: LockSpecification<'ctx>)
      =
    task {
      match lockSpec, resId with
      | Felicity _, None -> ()
      | Felicity lockSpec, Some resId ->
          if not state.TimedOut then
            let queueFactory = httpCtx.GetService<SemaphoreQueueFactory<'ctx>>()
            let queue = queueFactory.GetFor(lockSpec.CollName, resId)
            match! queue.Lock lockSpec.Timeout with
            | Some lock -> state.AddLock lock
            | None -> state.SetTimedOut()
      | Custom _, None -> ()
      | Custom lockSpec, Some resId ->
          if not state.TimedOut then
            match! lockSpec.CustomLock ctx resId with
            | Some lock -> state.AddLock lock
            | None -> state.SetTimedOut ()
      | CustomResourceCreation lockSpec, None ->
          if not state.TimedOut then
            match! lockSpec.CustomLock ctx with
            | Some lock -> state.AddLock lock
            | None -> state.SetTimedOut ()
      | CustomResourceCreation _, Some _ -> ()
      | Other lockSpec, resId ->
          if not state.TimedOut then
            let! otherId =
              match resId with
              | None -> lockSpec.GetOtherIdFromRelationship ctx req |> Job.startAsTask
              | Some resId -> lockSpec.GetOtherIdFromThisId ctx resId |> Job.startAsTask
            // Locks must be taken in sequence, not parallel, to avoid deadlocks.
            for lockSpec in lockSpec.OtherLockSpecs do
              if not state.TimedOut then
                do! lock state httpCtx ctx req otherId lockSpec
    }


  let lockAll httpCtx ctx req (totalTimeout: TimeSpan option) resId lockSpecs =
    task {
      let state = new MultiLockState()
      let timeoutTask =
        totalTimeout |> Option.map (fun ts ->
          task {
            do! Task.Delay ts
            state.SetTimedOut ()
          }
        )
      let lockTask =
        task {
          // Locks must be taken in sequence, not parallel, to avoid deadlocks.
          for lockSpec in lockSpecs do
            if not state.TimedOut then
              do! lock state httpCtx ctx req resId lockSpec
        }
      try
        let! completedTask = Task.WhenAny(lockTask :: Option.toList timeoutTask)
        do! completedTask  // TODO: Is this needed?
      with ex ->
        (state :> IDisposable).Dispose ()
        return ex.Reraise ()
      return if state.TimedOut then Error [lockTimeout ()] else Ok state
    }



type internal ResourceDefinitionLockSpec<'ctx> =
  abstract CollName: CollectionName option
  abstract LockSpecs: LockSpecification<'ctx> list option
  abstract TotalTimeout: TimeSpan option



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
  lockSpecs: LockSpecification<'ctx> list
  lockTotalTimeout: TimeSpan option
} with
  static member internal Create(name: string, id: Id<'ctx, 'entity, 'id>) : ResourceDefinition<'ctx, 'entity, 'id> =
    {
      name = name
      collectionName = None
      id = id
      lockSpecs = []
      lockTotalTimeout = None
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
    member this.LockSpecs = this.lockSpecs |> Some |> Option.filter (not << List.isEmpty)
    member this.TotalTimeout = this.lockTotalTimeout

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

  member private this.CreateCustomLockSpec(getLock: 'ctx -> 'id -> Task<IDisposable option>) =
    Custom {
      CustomLock =
        fun ctx rawId ->
          task {
            match! this.id.toDomain ctx rawId |> Job.startAsTask with
            | Error _ -> return None  // Request will fail anyway
            | Ok id -> return! getLock ctx id
          }
    }

  member private _.CreateCustomResourceCreationLockSpec(getLock: 'ctx -> Task<IDisposable option>) =
    CustomResourceCreation { CustomLock = getLock }

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
      OtherLockSpecs =
        match resDef.lockSpecs with
        | [] -> failwithf "Resource type '%s' can not lock other resource '%s' because the other resource does not have a lock specification" this.name resDef.name
        | xs -> xs
    }

  /// Lock this resource for the entirety of all modification operations to ensure there
  /// are no concurrent updates. If a lock can not be acquired after the specified timeout
  /// (default 10 seconds), an error will be returned.
  member this.Lock(?timeout) =
    let hasFelicityLockSpec = this.lockSpecs |> List.exists (function Felicity _ -> true | _ -> false)
    if hasFelicityLockSpec then failwith "Cannot call multiple built-in locks using Lock()"
    { this with lockSpecs = this.lockSpecs @ [this.CreateFelicityLockSpec(?timeout=timeout)] }

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> Job<IDisposable option>) =
    { this with lockSpecs = this.lockSpecs @ [this.CreateCustomLockSpec(fun ctx id -> getLock ctx id |> Job.startAsTask)] }

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'id -> Job<IDisposable option>) =
    this.CustomLock(fun _ id -> getLock id)

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> Task<IDisposable option>) =
    { this with lockSpecs = this.lockSpecs @ [this.CreateCustomLockSpec(getLock)] }

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'id -> Task<IDisposable option>) =
    this.CustomLock(fun _ id -> getLock id)

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> Async<IDisposable option>) =
    this.CustomLock(fun ctx id -> getLock ctx id |> Async.StartAsTask)

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'id -> Async<IDisposable option>) =
    this.CustomLock(fun _ id -> getLock id |> Async.StartAsTask)

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'ctx -> 'id -> IDisposable option) =
    this.CustomLock(fun ctx id -> getLock ctx id |> Task.FromResult)

  /// Lock this resource for the entirety of all modification operations except resource
  /// creation (POST) to ensure there are no concurrent updates, using an external lock
  /// mechanism. The getLock function is passed the resource ID, and should return None if
  /// the lock times out, or Some with an IDisposable that releases the lock when
  /// disposed.
  member this.CustomLock(getLock: 'id -> IDisposable option) =
    this.CustomLock(fun _ id -> getLock id |> Task.FromResult)

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: 'ctx -> Job<IDisposable option>) =
    { this with lockSpecs = this.lockSpecs @ [this.CreateCustomResourceCreationLockSpec(fun ctx -> getLock ctx |> Job.startAsTask)] }

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: unit -> Job<IDisposable option>) =
    this.CustomResourceCreationLock(fun (_: 'ctx) -> getLock () |> Job.startAsTask)

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: 'ctx -> Task<IDisposable option>) =
    { this with lockSpecs = this.lockSpecs @ [this.CreateCustomResourceCreationLockSpec(getLock)] }

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: unit -> Task<IDisposable option>) =
    this.CustomResourceCreationLock(fun (_: 'ctx) -> getLock ())

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: 'ctx -> Async<IDisposable option>) =
    this.CustomResourceCreationLock(fun ctx -> getLock ctx |> Async.StartAsTask)

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: unit -> Async<IDisposable option>) =
    this.CustomResourceCreationLock(fun (_: 'ctx) -> getLock () |> Async.StartAsTask)

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: 'ctx -> IDisposable option) =
    this.CustomResourceCreationLock(fun ctx -> getLock ctx |> Task.FromResult)

  /// Locks for the entirety of resource creation (POST) operations to ensure there are no
  /// concurrent operations, using an external lock mechanism. The getLock function should
  /// return None if the lock times out, or Some with an IDisposable that releases the
  /// lock when disposed.
  member this.CustomResourceCreationLock(getLock: unit -> IDisposable option) =
    this.CustomResourceCreationLock(fun (_: 'ctx) -> getLock () |> Task.FromResult)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> Job<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    if getOtherIdForResourceOperation.IsNone && relationshipForPostOperation.IsNone then
      failwithf "At least one of 'getOtherIdForResourceOperation' and 'relationshipForPostOperation' must be specified in call to LockOther for resource '%s'" this.name
    { this with
        lockSpecs =
          this.lockSpecs
          @ [this.CreateOtherLockSpec(resDef, ?getOtherIdForResourceOperation = getOtherIdForResourceOperation, ?relationshipForPostOperation = relationshipForPostOperation)]
    }

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified.
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
  /// specified.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'id -> Async<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.liftAsync2 (fun _ -> getId))),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> Async<'otherId option>, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.liftAsync2 getId)),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> 'otherId option, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.lift2 getId)),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'id -> 'otherId option, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.lift2 (fun _ -> getId))),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'ctx -> 'id -> 'otherId, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.lift2 (fun ctx id -> getId ctx id |> Some))),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// Lock another (e.g. parent) resource for the entirety of all modification operations
  /// on this resource to ensure there are no concurrent updates to the other resource.
  /// POST operations will only be locked if relationshipForPostOperation is specified.
  /// Any other operation will only be locked if getOtherIdForResourceOperation is
  /// specified.
  member this.LockOther(resDef: ResourceDefinition<'ctx, 'otherEntity, 'otherId>, ?getOtherIdForResourceOperation: 'id -> 'otherId, ?relationshipForPostOperation: OptionalRequestGetter<'ctx, 'otherId>) =
    this.LockOther(
      resDef,
      ?getOtherIdForResourceOperation = (getOtherIdForResourceOperation |> Option.map (fun getId -> Job.lift2 (fun _ id -> getId id |> Some))),
      ?relationshipForPostOperation=relationshipForPostOperation)

  /// When multiple locks are taken and this timeout is reached, Felicity will 1) return
  /// an error to the client, 2) dispose any locks that were successfully taken (including
  /// in-progress locks that complete after the timeout), and 3) not attempt to obtain any
  /// additional further locks.
  ///
  /// Throws if less than two locks have been specified for this resource.
  member this.MultiLockTotalTimeout(timeout: TimeSpan) =
    if this.lockSpecs.Length < 2 then failwithf "At least two locks must be present in order to call MultiLockTotalTimeout for resource '%s'" this.name
    { this with lockTotalTimeout = Some timeout }
