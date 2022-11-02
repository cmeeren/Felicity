namespace Felicity

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Giraffe
open Errors



[<AutoOpen>]
module private RelationshipHelpers =

  let getIncludedForSelfUrl httpCtx ctx (req: Request) (resp: ResponseBuilder<'ctx>) relName parentResDef parentEntity =
    // Hack: The included resources are the same as those for the parent resource document
    // when using only the include paths that start with this relationship.
    let reqForIncluded = { req with Includes = req.Includes |> List.filter (List.tryHead >> (=) (Some relName)) }
    resp.Write httpCtx ctx reqForIncluded (parentResDef, parentEntity)
    |> Task.map (fun doc -> doc.included)



type Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId> =
  abstract Name: string


type internal RelationshipHandlers<'ctx> =
  abstract Name: string
  abstract IsToMany: bool
  abstract IsSettable: bool
  abstract IsSettableButNotGettable: bool
  abstract IsSettableWithoutPersist: bool
  abstract GetRelated: ('ctx -> Request -> BoxedEntity -> ResourceDefinition<'ctx> -> ResponseBuilder<'ctx> -> HttpHandler) option
  abstract GetSelf: ('ctx -> Request -> BoxedEntity -> ResourceDefinition<'ctx> -> ResponseBuilder<'ctx> -> HttpHandler) option
  abstract PostSelf: ('ctx -> Request -> Preconditions<'ctx> -> BoxedEntity -> ResourceDefinition<'ctx> -> ResponseBuilder<'ctx> -> HttpHandler) option
  abstract PatchSelf: ('ctx -> Request -> ParentResourceTypeName -> Preconditions<'ctx> -> BoxedEntity -> ResourceDefinition<'ctx> -> ResponseBuilder<'ctx> -> HttpHandler) option
  abstract DeleteSelf: ('ctx -> Request -> Preconditions<'ctx> -> BoxedEntity -> ResourceDefinition<'ctx> -> ResponseBuilder<'ctx> -> HttpHandler) option


type internal ToOneRelationship<'ctx> =
  abstract Name: RelationshipName
  abstract SelfLink: bool
  abstract RelatedLink: bool
  abstract AllowedTypes: ICollection<ResourceTypeName> option
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Task<Skippable<ResourceDefinition<'ctx> * BoxedEntity>>) option
  abstract GetLinkageIfNotIncluded: 'ctx -> BoxedEntity -> Task<Skippable<ResourceIdentifier>>
  abstract SkipRelationship: 'ctx -> BoxedEntity -> bool


type ToOneRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  idGetter: RequestGetter<'ctx, ('relatedId * ResourceIdentifier) option>
  getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>
} with

  static member internal Create(name, idGetter: RequestGetter<'ctx, ('relatedId * ResourceIdentifier) option>, getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) : ToOneRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      idGetter = idGetter
      getRelated = getRelated
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          this.idGetter.Get(ctx, req, includedTypeAndId)
          |> TaskResult.bind (
              Option.traverseTaskResult (fun (resId, identifier) ->
                this.getRelated.GetById ctx resId
                |> TaskResult.mapError (fun _ -> [relatedResourceNotFound identifier.``type`` identifier.id ("/data/relationships/" + this.name + "/data")])
                |> TaskResult.requireSome [relatedResourceNotFound identifier.``type`` identifier.id ("/data/relationships/" + this.name + "/data")]
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]


type ToOneRelationshipIncludedGetter<'ctx, 'relatedEntity> = internal {
  name: string
  getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>
  allowedTypes: ResourceTypeName list
} with

  static member internal Create(name, getParser, allowedTypes) : ToOneRelationshipIncludedGetter<'ctx, 'relatedEntity> =
    {
      name = name
      getParser = getParser
      allowedTypes = allowedTypes
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOne as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Task.result
                  | { data = Include identifier } when not (this.allowedTypes |> List.contains identifier.``type``) ->
                      let pointer = relsPointer + "/" + this.name + "/data/type"
                      Error [relInvalidType this.name identifier.``type`` this.allowedTypes pointer] |> Task.result
                  | { data = Include identifier } ->
                      RequestParserHelper<'ctx>(ctx, req, (identifier.``type``, identifier.id))
                      |> this.getParser
                      |> fun p -> p.ParseTask()
                      |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToOne>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result

    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]


type ToOneRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  setOrder: int
  mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  resolveId: ('relatedId -> ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Task<Result<'relatedId, (ParsedValueInfo -> Error) list>>> option
  get: ('ctx -> 'entity -> Task<'relatedEntity Skippable>) option
  set: ('ctx -> 'setCtx -> Pointer -> 'relatedId * ResourceIdentifier -> 'entity -> Task<Result<'entity, Error list>>) option
  getLinkageIfNotIncluded: 'ctx -> 'entity -> Task<ResourceIdentifier Skippable>
  skipRelationship: 'ctx -> 'entity -> bool
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>
  beforeModifySelf: 'setCtx -> 'entity -> Task<Result<'entity, Error list>>
  afterModifySelf: ('setCtx -> 'entity -> 'entity -> Task<Result<'entity, Error list>>) option
  modifyGetRelatedResponse: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler
  modifyGetSelfResponse: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler
  modifyPatchSelfOkResponse: 'setCtx -> 'entity -> 'relatedEntity -> HttpHandler
  modifyPatchSelfAcceptedResponse: 'setCtx -> 'entity -> HttpHandler
  patchSelfReturn202Accepted: bool
} with

  static member internal Create (name: string, mapSetCtx, resolveEntity, resolveId, idParsers) : ToOneRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      setOrder = 0
      mapSetCtx = mapSetCtx
      resolveEntity = resolveEntity
      resolveId = resolveId
      idParsers = idParsers
      get = None
      set = None
      getLinkageIfNotIncluded = fun _ _ -> Task.result Skip
      skipRelationship = fun _ _ -> false
      hasConstraints = false
      getConstraints = fun _ _ -> Task.result []
      beforeModifySelf = fun _ e -> Ok e |> Task.result
      afterModifySelf = None
      modifyGetRelatedResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyGetSelfResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      patchSelfReturn202Accepted = false
    }

  member private _.toIdSetter (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>) entitySetter =
    fun ctx setCtx (dataPointer: Pointer) (relatedId, identifier: ResourceIdentifier) entity ->
      getRelated.GetById ctx relatedId
      |> TaskResult.mapError (List.map (Error.setSourcePointer dataPointer))
      |> TaskResult.requireSome [relatedResourceNotFound identifier.``type`` identifier.id dataPointer]
      |> TaskResult.bind (fun r ->
          entitySetter setCtx r entity
          |> TaskResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member private this.OptionalWithIdentifier =
    { new RequestGetter<'ctx, ('relatedId * ResourceIdentifier) option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwith $"Attempted to parse resource ID for polymorphic relationship '%s{this.name}', but no ID parsers have been specified.")
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOne as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Task.result
                  | { data = Include identifier } ->
                      match idParsers.TryGetValue identifier.``type`` with
                      | false, _ ->
                          let allowedTypes = idParsers |> Map.toList |> List.map fst
                          let pointer = relsPointer + "/" + this.name + "/data/type"
                          Error [relInvalidType this.name identifier.``type`` allowedTypes pointer] |> Task.result
                      | true, parseId ->
                          parseId ctx identifier.id
                          // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                          // this just means that the resource does not exist, which is a more helpful result.
                          |> TaskResult.mapError (fun _ -> [relatedResourceNotFound identifier.``type`` identifier.id (relsPointer + "/" + this.name + "/data")])
                          |> TaskResult.map (fun x -> Some (x, identifier))
              | true, x -> failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToOne>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result
    }


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId option> with
        member _.FieldName = this.OptionalWithIdentifier.FieldName
        member _.QueryParamName = this.OptionalWithIdentifier.QueryParamName
        member _.Get(ctx, req, includedTypeAndId) =
          this.OptionalWithIdentifier.Get(ctx, req, includedTypeAndId)
          |> AsyncResult.map (Option.map fst)
    }


  interface OptionalRequestGetter<'ctx, 'relatedId> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedId> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          let pointer = Request.pointerForMissingRel includedTypeAndId req + "/" + this.name
          [reqParserProhibitedRel this.name pointer]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Names = Set.singleton this.name
    member this.SetOrder = this.setOrder
    member this.Set ctx req entity numSetters =
      task {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { relationships = Include rels } }) ->
            match this.set, rels.TryGetValue this.name with
            | _, (false, _) -> return Ok (entity, false) // not provided in request
            | None, (true, _) ->
                if numSetters[this.name] > 1 then
                  // Provided in request and no setter, but there exists another setter, so ignore
                  return Ok (entity, false)
                else
                  return Error [setRelReadOnly this.name ("/data/relationships/" + this.name)]
            | Some set, (true, (:? ToOne as rel)) ->
                match! this.mapSetCtx ctx (unbox<'entity> entity) with
                | Error errs -> return Error errs
                | Ok setCtx ->
                    let idParsers =
                      this.idParsers
                      |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
                    match rel.data with
                    | Skip -> return Error [relMissingData this.name ("/data/relationships/" + this.name)]
                    | Include identifier ->
                        match idParsers.TryGetValue identifier.``type`` with
                        | false, _ ->
                            let allowedTypes = idParsers |> Map.toList |> List.map fst
                            let pointer = "/data/relationships/" + this.name + "/data/type"
                            return Error [relInvalidType this.name identifier.``type`` allowedTypes pointer]
                        | true, parseId ->
                            return!
                              parseId ctx identifier.id
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> TaskResult.mapError (fun _ -> [relatedResourceNotFound identifier.``type`` identifier.id ("/data/relationships/" + this.name + "/data")])
                              |> TaskResult.bind (fun domain ->
                                  set ctx setCtx ("/data/relationships/" + this.name + "/data") (domain, identifier) (unbox<'entity> entity))
                              |> TaskResult.map (fun e -> box<'entity> e, true)
            | Some _, (true, rel) -> return failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToOne>.FullName}, but was %s{rel.GetType().FullName}"
        | _ -> return Ok (entity, false)  // no relationships provided
      }


  interface ToOneRelationship<'ctx> with
    member this.Name = this.name
    member this.SelfLink = this.get.IsSome
    member this.RelatedLink = this.get.IsSome
    member this.AllowedTypes = this.idParsers |> Option.map Map.keys
    member this.BoxedGetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx entity ->
          getRelated ctx (unbox<'entity> entity) |> Task.map (Skippable.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          ))
      )
    member this.GetLinkageIfNotIncluded ctx entity =
      this.getLinkageIfNotIncluded ctx (unbox<'entity> entity)
    member this.SkipRelationship ctx entity =
      this.skipRelationship ctx (unbox<'entity> entity)

  
  member this.Related (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>) =
    ToOneRelationshipRelatedGetter<'ctx, 'entity, 'lookupType, 'relatedId>.Create(this.name, this.OptionalWithIdentifier, getRelated)


  member this.Included (getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>) =
    ToOneRelationshipIncludedGetter.Create(this.name, getParser, this.idParsers |> Option.map (Map.toList >> List.map fst) |> Option.defaultValue [])


  interface Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId> with
    member this.Name = this.name

  interface FieldQueryParser<'ctx, 'entity, 'relatedId, string> with
    member this.Name = this.name
    member this.ToDomain ctx getInfo str =
      match this.idParsers |> Option.defaultValue Map.empty |> Map.toList with
      | [] -> failwith $"Relationship '%s{this.name}' does not contain any ID parsers and may not be used to parse query IDs"
      | [_, parseId] -> parseId ctx str |> TaskResult.mapError (List.map (fun getErr -> getErr (getInfo str)))
      | _::_::_ -> failwith $"Relationship '%s{this.name}' contains ID parsers for several types and may therefore not be used to parse query IDs"


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)
    member this.RequiresExplicitInclude = false


  interface RelationshipHandlers<'ctx> with

    member this.Name = this.name

    member _.IsToMany = false

    member this.IsSettable = this.set.IsSome

    member this.IsSettableButNotGettable = this.set.IsSome && this.get.IsNone

    member this.IsSettableWithoutPersist = this.set.IsSome && this.afterModifySelf.IsNone

    member this.GetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resDef resp ->
          fun next httpCtx ->
            task {
              match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok () ->
                  let entity = unbox<'entity> entity
                  match! getRelated ctx entity with
                  | Skip -> return! handleErrors [getRelWhileSkip ()] next httpCtx
                  | Include relatedEntity ->
                      let b = resolveEntity relatedEntity
                      let! doc = resp.Write httpCtx ctx req (b.resourceDef, b.entity)

                      let! fieldTrackerHandler =
                        match this.idParsers with
                        | None ->
                            logFieldTrackerPolymorphicRelTraversalWarning httpCtx resDef.TypeName this.name
                            Task.result (fun next ctx -> next ctx)
                        | Some parsers ->
                            let primaryResourceTypes = parsers.Keys |> Seq.toList
                            httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>().TrackFields(primaryResourceTypes, ctx, req, (resDef.TypeName, this.name))

                      let handler =
                        setStatusCode 200
                        >=> this.modifyGetRelatedResponse ctx entity relatedEntity
                        >=> fieldTrackerHandler
                        >=> jsonApiWithETag<'ctx> doc
                      return! handler next httpCtx
            }
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resDef resp ->
          fun next httpCtx ->
            task {
              match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok () ->
                  let entity = unbox<'entity> entity
                  match! getRelated ctx entity with
                  | Skip -> return! handleErrors [getRelWhileSkip ()] next httpCtx
                  | Include relatedEntity ->
                      let b = resolveEntity relatedEntity
                      let! included = getIncludedForSelfUrl httpCtx ctx req resp this.name resDef entity
                      let doc : ResourceIdentifierDocument = {
                        jsonapi = Skip
                        links = Skip
                        meta = Skip
                        data = Some { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                        included = included
                      }

                      let! fieldTrackerHandler =
                        httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>()
                          .TrackFields(
                            [resDef.TypeName],
                            ctx,
                            req,
                            (resDef.TypeName, this.name),
                            this.name
                          )

                      let handler =
                        setStatusCode 200
                        >=> this.modifyGetSelfResponse ctx entity relatedEntity
                        >=> fieldTrackerHandler
                        >=> jsonApiWithETag<'ctx> doc
                      return! handler next httpCtx
            }
      )

    member _.PostSelf = None

    member this.PatchSelf =
      (this.get, this.set) ||> Option.map2 (fun getRelated set ->
        let idParsers =
          this.idParsers
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        let afterModifySelf =
          this.afterModifySelf
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without AfterModifySelf. This should be caught at startup.")
        fun ctx req parentTypeName preconditions entity0 resDef resp ->
          fun next httpCtx ->
            task {
              match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok () ->
                  match! this.mapSetCtx ctx (unbox<'entity> entity0) with
                  | Error errs -> return! handleErrors errs next httpCtx
                  | Ok setCtx ->
                      match req.IdentifierDocument.Value with
                      | Error errs -> return! handleErrors errs next httpCtx
                      | Ok None -> return! handleErrors [modifyRelSelfMissingData ""] next httpCtx
                      | Ok (Some { data = None }) -> return! handleErrors [relInvalidNull parentTypeName this.name "/data"] next httpCtx
                      | Ok (Some { data = Some id }) ->
                          match preconditions.Validate httpCtx ctx entity0 with
                          | Error errors -> return! handleErrors errors next httpCtx
                          | Ok () ->
                              match! this.beforeModifySelf setCtx (unbox<'entity> entity0) with
                              | Error errors -> return! handleErrors errors next httpCtx
                              | Ok entity1 ->
                                  match idParsers.TryGetValue id.``type`` with
                                  | false, _ ->
                                      let allowedTypes = idParsers |> Map.toList |> List.map fst
                                      return! handleErrors [relInvalidTypeSelf id.``type`` allowedTypes "/data/type"] next httpCtx
                                  | true, parseId ->
                                      let! entity2Res =
                                        parseId ctx id.id
                                        // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                        // this just means that the resource does not exist, which is a more helpful result.
                                        |> TaskResult.mapError (fun _ -> [relatedResourceNotFound id.``type`` id.id "/data"])
                                        |> TaskResult.bind (fun domain ->
                                            set ctx setCtx "/data" (domain, id) (unbox<'entity> entity1)
                                        )
                                      match entity2Res with
                                      | Error errs -> return! handleErrors errs next httpCtx
                                      | Ok entity2 ->
                                          match! afterModifySelf setCtx (unbox<'entity> entity0) (unbox<'entity> entity2) with
                                          | Error errors -> return! handleErrors errors next httpCtx
                                          | Ok entity3 ->
                                              if this.patchSelfReturn202Accepted then
                                                let handler =
                                                  setStatusCode 202
                                                  >=> this.modifyPatchSelfAcceptedResponse setCtx (unbox<'entity> entity2)
                                                return! handler next httpCtx
                                              else
                                                match! getRelated ctx (unbox<'entity> entity3) with
                                                | Skip ->
                                                    let logger = httpCtx.GetLogger("Felicity.Relationships")
                                                    logger.LogError("Relationship {RelationshipName} was updated using a self URL, but no success response could be returned because the relationship getter returned Skip. This violates the JSON:API specification. Make sure that the relationship getter never returns Skip after an update.", this.name)
                                                    return! handleErrors [relModifySelfWhileSkip ()] next httpCtx
                                                | Include relatedEntity ->
                                                    let b = resolveEntity relatedEntity
                                                    let! included = getIncludedForSelfUrl httpCtx ctx req resp this.name resDef entity3
                                                    let doc : ResourceIdentifierDocument = {
                                                      jsonapi = Skip
                                                      links = Skip
                                                      meta = Skip
                                                      data = Some { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                                                      included = included
                                                    }

                                                    let! fieldTrackerHandler =
                                                      httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>()
                                                        .TrackFields(
                                                          [resDef.TypeName],
                                                          ctx,
                                                          req,
                                                          (resDef.TypeName, this.name),
                                                          this.name
                                                        )

                                                    let handler =
                                                      setStatusCode 200
                                                      >=> this.modifyPatchSelfOkResponse setCtx (unbox<'entity> entity3) relatedEntity
                                                      >=> fieldTrackerHandler
                                                      >=> jsonApiWithETag<'ctx> doc
                                                    return! handler next httpCtx
            }
      )

    member _.DeleteSelf = None


  member this.Name = this.name

  /// Specify the order in which this field will be set relative to other fields during
  /// POST collection and PATCH resource requests. By default, all fields have SetOrder =
  /// 0. Negative numbers are allowed. The order of fields with identical SetOrder is
  /// unspecified.
  member this.SetOrder (i: int) =
    { this with setOrder = i }

  member this.GetTaskSkip(get: Func<'ctx, 'entity, Task<'relatedEntity Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetTaskSkip(get: Func<'entity, Task<'relatedEntity Skippable>>) =
    this.GetTaskSkip(fun _ e -> get.Invoke(e))

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetAsyncSkip(get: Func<'entity, Async<'relatedEntity Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc get)

  member this.GetTask (get: Func<'ctx, 'entity, Task<'relatedEntity>>) =
    this.GetTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetTask (get: Func<'entity, Task<'relatedEntity>>) =
    this.GetTaskSkip(fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity>>) =
    this.GetTask(Task.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity>>) =
    this.GetTask(Task.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity>>) =
    this.GetTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.result)

  member this.GetSkip (get: Func<'entity, Skippable<'relatedEntity>>) =
    this.GetTaskSkip(fun _ r -> get.Invoke(r) |> Task.result)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity>) =
    this.GetTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> Task.result)

  member this.Get (get: Func<'entity, 'relatedEntity>) =
    this.GetTaskSkip(fun _ r -> get.Invoke r |> Include |> Task.result)

  member this.GetLinkageIfNotIncludedTaskSkip(get: Func<'ctx, 'entity, Task<'relatedId Skippable>>) =
    if this.resolveId.IsNone then
      failwithf "Can only add linkage getter if the polymorphic resource definition contains an ID resolver."
    { this with
        getLinkageIfNotIncluded =
          fun ctx e ->
            task {
              match! get.Invoke(ctx, e) with
              | Skip -> return Skip
              | Include relatedId ->
                  let resDef = this.resolveId.Value relatedId
                  return Include {
                    ``type`` = resDef.name
                    id = resDef.id.fromDomain relatedId
                  }
            }
    }

  member this.GetLinkageIfNotIncludedAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedId Skippable>>) =
    this.GetLinkageIfNotIncludedTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetLinkageIfNotIncludedTask (get: Func<'ctx, 'entity, Task<'relatedId>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetLinkageIfNotIncludedTask (get: Func<'entity, Task<'relatedId>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetLinkageIfNotIncludedAsync (get: Func<'ctx, 'entity, Async<'relatedId>>) =
    this.GetLinkageIfNotIncludedTask(Task.liftAsyncFunc2 get)

  member this.GetLinkageIfNotIncludedAsync (get: Func<'entity, Async<'relatedId>>) =
    this.GetLinkageIfNotIncludedTask(Task.liftAsyncFunc get)

  member this.GetLinkageIfNotIncludedSkip (get: Func<'ctx, 'entity, Skippable<'relatedId>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.result)

  member this.GetLinkageIfNotIncluded (get: Func<'ctx, 'entity, 'relatedId>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> Task.result)

  member this.GetLinkageIfNotIncluded (get: Func<'entity, 'relatedId>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun _ r -> get.Invoke r |> Include |> Task.result)

  /// Omits the entire relationship (links, data, and meta) in the returned resource if the predicate returns true. If
  /// using one of the Get...Skip methods, this method should also be called. Otherwise, relationship links will always
  /// be present in the response, but GET operations against them will return an error if the getter returns Skip.
  member this.SkipRelationshipIf(predicate: 'ctx -> 'entity -> bool) =
    { this with skipRelationship = predicate }

  /// Omits the entire relationship (links, data, and meta) in the returned resource if the predicate returns true. If
  /// using one of the Get...Skip methods, this method should also be called. Otherwise, relationship links will always
  /// be present in the response, but GET operations against them will return an error if the getter returns Skip.
  member this.SkipRelationshipIf(predicate: 'entity -> bool) =
    { this with skipRelationship = fun _ e -> predicate e }

  member private this.SetTaskRes (set: Func<'ctx, 'setCtx, Pointer, 'relatedId * ResourceIdentifier, 'entity, Task<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with set = Some (fun ctx setCtx ptr relIdWithIdentifier e -> set.Invoke(ctx, setCtx, ptr, relIdWithIdentifier, e)) }

  member this.SetTaskRes (set: Func<'setCtx, 'relatedId, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(fun _ ctx pointer relIdWithIdentifier e -> set.Invoke(ctx, fst relIdWithIdentifier, e) |> TaskResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetTaskRes (set: Func<'relatedId, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(fun _ id e -> set.Invoke(id, e))

  member this.SetTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(this.toIdSetter getRelated (fun ctx relId e -> set.Invoke(ctx, relId, e)))

  member this.SetTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(this.toIdSetter getRelated (fun _ id e -> set.Invoke(id, e)))

  member this.SetAsyncRes (set: Func<'setCtx, 'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(Task.liftAsyncFunc3 set)

  member this.SetAsyncRes (set: Func<'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(Task.liftAsyncFunc2 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(getRelated, Task.liftAsyncFunc3 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(getRelated, Task.liftAsyncFunc2 set)

  member this.SetTask (set: Func<'setCtx, 'relatedId, 'entity, Task<'entity>>) =
    this.SetTaskRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.SetTask (set: Func<'relatedId, 'entity, Task<'entity>>) =
    this.SetTaskRes(fun _ related entity -> set.Invoke(related, entity) |> Task.map Ok)

  member this.SetTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Task<'entity>>) =
    this.SetTaskRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Task.map Ok))

  member this.SetTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Task<'entity>>) =
    this.SetTaskRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Task.map Ok))

  member this.SetAsync (set: Func<'setCtx, 'relatedId, 'entity, Async<'entity>>) =
    this.SetTask(Task.liftAsyncFunc3 set)

  member this.SetAsync (set: Func<'relatedId, 'entity, Async<'entity>>) =
    this.SetTask(Task.liftAsyncFunc2 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Async<'entity>>) =
    this.SetTask(getRelated, Task.liftAsyncFunc3 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Async<'entity>>) =
    this.SetTask(getRelated, Task.liftAsyncFunc2 set)

  member this.SetRes (set: Func<'setCtx, 'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(Task.liftFunc3 set)

  member this.SetRes (set: Func<'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(Task.liftFunc2 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(getRelated, Task.liftFunc3 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(getRelated, Task.liftFunc2 set)

  member this.Set (set: Func<'setCtx, 'relatedId, 'entity, 'entity>) =
    this.SetTaskRes(TaskResult.liftFunc3 set)

  member this.Set (set: Func<'relatedId, 'entity, 'entity>) =
    this.SetTaskRes(TaskResult.liftFunc2 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, 'entity>) =
    this.SetTaskRes(getRelated, TaskResult.liftFunc3 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, 'entity>) =
    this.SetTaskRes(getRelated, TaskResult.liftFunc2 set)

  member this.AddConstraintsTask(getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            task {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsTask(Task.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsTask(Task.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsTask(fun ctx e -> [name, box (getValue ctx e)] |> Task.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.BeforeModifySelfTaskRes(f: Func<'setCtx, 'entity, Task<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfTaskRes(f: Func<'setCtx, 'entity, Task<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> TaskResult.map (fun () -> e))

  member this.BeforeModifySelfTaskRes(f: Func<'entity, Task<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfTaskRes(f: Func<'entity, Task<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'setCtx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'setCtx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc f)

  member this.BeforeModifySelfTask(f: Func<'setCtx, 'entity, Task<'entity>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'setCtx, 'entity, Task<unit>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'entity, Task<'entity>>) =
    this.BeforeModifySelfTaskRes(fun e -> f.Invoke e |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'entity, Task<unit>>) =
    this.BeforeModifySelfTaskRes(fun e -> f.Invoke e |> Task.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'setCtx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'setCtx, 'entity, Async<unit>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc f)

  member this.BeforeModifySelfRes(f: Func<'setCtx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'setCtx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc f)

  member this.BeforeModifySelf(f: Func<'setCtx, 'entity, 'entity>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'setCtx, 'entity, unit>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc f)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc f)

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some f }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> 'entity -> Task<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew |> TaskResult.map (fun () -> eNew)) }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx _ e -> f ctx e) }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> Task<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> TaskResult.map (fun () -> e))

  member this.AfterModifySelfTaskRes(f: 'entity -> 'entity -> Task<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew)

  member this.AfterModifySelfTaskRes(f: 'entity -> 'entity -> Task<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew |> TaskResult.map (fun () -> eNew))

  member this.AfterModifySelfTaskRes(f: 'entity -> Task<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ _ e -> f e)

  member this.AfterModifySelfTaskRes(f: 'entity -> Task<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ _ e -> f e |> TaskResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync f)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun ctx eOld eNew -> f ctx eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun ctx eOld eNew -> f ctx eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun e -> f e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun e -> f e |> Task.map Ok)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift3 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift3 f)

  member this.AfterModifySelfRes(f: 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift f)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift3 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift3 f)

  member this.AfterModifySelf(f: 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift f)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift f)

  member this.PatchSelfReturn202Accepted () =
    { this with patchSelfReturn202Accepted = true }

  member this.ModifyGetRelatedResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler) =
    { this with modifyGetRelatedResponse = getHandler }

  member this.ModifyGetRelatedResponse(f: 'ctx -> 'entity -> 'relatedEntity -> HttpContext -> unit) =
    this.ModifyGetRelatedResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetRelatedResponse(handler: HttpHandler) =
    this.ModifyGetRelatedResponse(fun _ _ _ -> handler)

  member this.ModifyGetSelfResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity -> HttpHandler) =
    { this with modifyGetSelfResponse = getHandler }

  member this.ModifyGetSelfResponse(f: 'ctx -> 'entity -> 'relatedEntity -> HttpContext -> unit) =
    this.ModifyGetSelfResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetSelfResponse(handler: HttpHandler) =
    this.ModifyGetSelfResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfOkResponse(getHandler: 'setCtx -> 'entity -> 'relatedEntity -> HttpHandler) =
    { this with modifyPatchSelfOkResponse = getHandler }

  member this.ModifyPatchSelfOkResponse(f: 'setCtx -> 'entity -> 'relatedEntity -> HttpContext -> unit) =
    this.ModifyPatchSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPatchSelfOkResponse(handler: HttpHandler) =
    this.ModifyPatchSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfAcceptedResponse(getHandler: 'setCtx -> 'entity -> HttpHandler) =
    { this with modifyPatchSelfAcceptedResponse = getHandler }

  member this.ModifyPatchSelfAcceptedResponse(f: 'setCtx -> 'entity -> HttpContext -> unit) =
    this.ModifyPatchSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPatchSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPatchSelfAcceptedResponse(fun _ _ -> handler)



[<AutoOpen>]
module ToOneRelationshipExtensions =

  type ToOneRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> with

    member this.AddConstraint (name: string, value: 'a) =
      this.AddConstraint(name, fun _ -> value)



type internal ToOneNullableRelationship<'ctx> =
  abstract Name: RelationshipName
  abstract SelfLink: bool
  abstract RelatedLink: bool
  abstract AllowedTypes: ICollection<ResourceTypeName> option
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Task<Skippable<(ResourceDefinition<'ctx> * BoxedEntity) option>>) option
  abstract GetLinkageIfNotIncluded: 'ctx -> BoxedEntity -> Task<Skippable<ResourceIdentifier option>>
  abstract SkipRelationship: 'ctx -> BoxedEntity -> bool



type ToOneNullableRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  idGetter: RequestGetter<'ctx, ('relatedId * ResourceIdentifier) option option>
  getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>
} with

  static member internal Create(name, idGetter: RequestGetter<'ctx, ('relatedId * ResourceIdentifier) option option>, getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) : ToOneNullableRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      idGetter = idGetter
      getRelated = getRelated
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          this.idGetter.Get(ctx, req, includedTypeAndId)
          |> TaskResult.bind (  // ID did not fail parsing, but may be missing or null
              Option.traverseTaskResult (  // ID was present, but may be null
                Option.traverseTaskResult (fun (resId, identifier) ->  // ID was not null
                  this.getRelated.GetById ctx resId
                  |> TaskResult.mapError (List.map (Error.setSourcePointer ("/data/relationships/" + this.name + "/data")))
                  |> TaskResult.requireSome [relatedResourceNotFound identifier.``type`` identifier.id ("/data/relationships/" + this.name + "/data")]
                )
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  member this.AsNonNullable =
    let nonNullIdGetter =
      { new RequestGetter<'ctx, ('relatedId * ResourceIdentifier) option> with
          member _.FieldName = Some this.name
          member _.QueryParamName = None
          member _.Get(ctx, req, includedTypeAndId) =
            task {
              match! this.idGetter.Get(ctx, req, includedTypeAndId) with
              | Error errs -> return Error errs
              | Ok None -> return Ok None
              | Ok (Some None) -> return Error [setRelNullNotAllowed this.name |> Error.setSourcePointer ("/data/relationships/" + this.name + "/data")]
              | Ok (Some (Some resId)) -> return Ok (Some resId)
            }
      }
    ToOneRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId>.Create(
      this.name, nonNullIdGetter, this.getRelated)


type ToOneNullableRelationshipIncludedGetter<'ctx, 'relatedEntity> = internal {
  name: string
  getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>
  allowedTypes: ResourceTypeName list
} with

  static member internal Create(name, getParser, allowedTypes) : ToOneNullableRelationshipIncludedGetter<'ctx, 'relatedEntity> =
    {
      name = name
      getParser = getParser
      allowedTypes = allowedTypes
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOneNullable as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Task.result
                  | { data = Include identifier } ->
                      identifier
                      |> Option.traverseTaskResult (fun id ->
                          if not (this.allowedTypes |> List.contains id.``type``) then
                            let pointer = relsPointer + "/" + this.name + "/data/type"
                            Error [relInvalidType this.name id.``type`` this.allowedTypes pointer] |> Task.result
                          else
                            RequestParserHelper<'ctx>(ctx, req, (id.``type``, id.id))
                            |> this.getParser
                            |> fun p -> p.ParseTask()
                      )
                      |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToOneNullable>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result

    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToOneNullableRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  setOrder: int
  mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  resolveId: ('relatedId -> ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Task<Result<'relatedId, (ParsedValueInfo -> Error) list>>> option
  get: ('ctx -> 'entity -> Task<'relatedEntity option Skippable>) option
  set: ('ctx -> 'setCtx -> Pointer -> ('relatedId * ResourceIdentifier) option -> 'entity -> Task<Result<'entity, Error list>>) option
  getLinkageIfNotIncluded: 'ctx -> 'entity -> Task<ResourceIdentifier option Skippable>
  skipRelationship: 'ctx -> 'entity -> bool
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>
  beforeModifySelf: 'setCtx -> 'entity -> Task<Result<'entity, Error list>>
  afterModifySelf: ('setCtx -> 'entity -> 'entity -> Task<Result<'entity, Error list>>) option
  modifyGetRelatedResponse: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler
  modifyGetSelfResponse: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler
  modifyPatchSelfOkResponse: 'setCtx -> 'entity -> 'relatedEntity option -> HttpHandler
  modifyPatchSelfAcceptedResponse: 'setCtx -> 'entity -> HttpHandler
  patchSelfReturn202Accepted: bool
} with

  static member internal Create (name: string, mapSetCtx, resolveEntity, resolveId, idParsers) : ToOneNullableRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      setOrder = 0
      mapSetCtx = mapSetCtx
      resolveEntity = resolveEntity
      resolveId = resolveId
      idParsers = idParsers
      get = None
      set = None
      getLinkageIfNotIncluded = fun _ _ -> Task.result Skip
      skipRelationship = fun _ _ -> false
      hasConstraints = false
      getConstraints = fun _ _ -> Task.result []
      beforeModifySelf = fun _ e -> Ok e |> Task.result
      afterModifySelf = None
      modifyGetRelatedResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyGetSelfResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      patchSelfReturn202Accepted = false
    }

  member private _.toIdSetter (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>) entitySetter =
    fun ctx setCtx (dataPointer: Pointer) relatedIdWithIdentifier entity ->
      relatedIdWithIdentifier
      |> Option.traverseTaskResult (fun (relId, identifier: ResourceIdentifier) ->
          getRelated.GetById ctx relId
          |> TaskResult.mapError (List.map (Error.setSourcePointer dataPointer))
          |> TaskResult.requireSome [relatedResourceNotFound identifier.``type`` identifier.id dataPointer]
      )
      |> TaskResult.bind (fun r ->
        entitySetter setCtx r entity
        |> TaskResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member private this.OptionalWithIdentifier =
    { new RequestGetter<'ctx, ('relatedId * ResourceIdentifier) option option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwith $"Attempted to parse resource ID for polymorphic relationship '%s{this.name}', but no ID parsers have been specified.")
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToOneNullable as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + this.name)] |> Task.result
                  | { data = Include identifier } ->
                      identifier
                      |> Option.traverseTaskResult (fun id ->
                          match idParsers.TryGetValue id.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = relsPointer + this.name + "/data/type"
                              Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> Task.result
                          | true, parseId ->
                              parseId ctx id.id
                              |> TaskResult.map (fun x -> x, id)
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> TaskResult.mapError (fun _ -> [relatedResourceNotFound id.``type`` id.id (relsPointer + this.name + "/data")])
                      )
                      |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToOneNullable>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result
    }


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId option option> with
        member _.FieldName = this.OptionalWithIdentifier.FieldName
        member _.QueryParamName = this.OptionalWithIdentifier.QueryParamName
        member _.Get(ctx, req, includedTypeAndId) =
          this.OptionalWithIdentifier.Get(ctx, req, includedTypeAndId)
          |> TaskResult.map (Option.map (Option.map fst))
    }


  interface OptionalRequestGetter<'ctx, 'relatedId option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedId option> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          let pointer = Request.pointerForMissingRel includedTypeAndId req + "/" + this.name
          [reqParserProhibitedRel this.name pointer]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Names = Set.singleton this.name
    member this.SetOrder = this.setOrder
    member this.Set ctx req entity numSetters =
      task {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { relationships = Include rels } }) ->
            match this.set, rels.TryGetValue this.name with
            | _, (false, _) -> return Ok (entity, false) // not provided in request
            | None, (true, _) ->
                if numSetters[this.name] > 1 then
                  // Provided in request and no setter, but there exists another setter, so ignore
                  return Ok (entity, false)
                else
                  return Error [setRelReadOnly this.name ("/data/relationships/" + this.name)]
            | Some set, (true, (:? ToOneNullable as rel)) ->
              match! this.mapSetCtx ctx (unbox<'entity> entity) with
              | Error errs -> return Error errs
              | Ok setCtx ->
                  let idParsers =
                    this.idParsers
                    |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
                  match rel.data with
                  | Skip -> return Error [relMissingData this.name ("/data/relationships/" + this.name)]
                  | Include identifier ->
                      return!
                        identifier
                        |> Option.traverseTaskResult (fun id ->
                            match idParsers.TryGetValue id.``type`` with
                            | false, _ ->
                                let allowedTypes = idParsers |> Map.toList |> List.map fst
                                let pointer = "/data/relationships/" + this.name + "/data/type"
                                Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> Task.result
                            | true, parseId ->
                              parseId ctx id.id
                              |> TaskResult.map (fun x -> x, id)
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> TaskResult.mapError (fun _ -> [relatedResourceNotFound id.``type`` id.id ("/data/relationships/" + this.name + "/data")])
                        )
                        |> TaskResult.bind (fun resIdWithIdentifier ->
                            set ctx setCtx ("/data/relationships/" + this.name + "/data") resIdWithIdentifier (unbox<'entity> entity))
                        |> TaskResult.map (fun e -> box<'entity> e, true)
            | Some _, (true, rel) -> return failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToOneNullable>.FullName}, but was %s{rel.GetType().FullName}"
        | _ -> return Ok (entity, false)  // no relationships provided
      }


  interface ToOneNullableRelationship<'ctx> with
    member this.Name = this.name
    member this.SelfLink = this.get.IsSome
    member this.RelatedLink = this.get.IsSome
    member this.AllowedTypes = this.idParsers |> Option.map Map.keys
    member this.BoxedGetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx entity ->
          getRelated ctx (unbox<'entity> entity) |> Task.map (Skippable.map (Option.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          )))
      )
    member this.GetLinkageIfNotIncluded ctx entity =
      this.getLinkageIfNotIncluded ctx (unbox<'entity> entity)
    member this.SkipRelationship ctx entity =
      this.skipRelationship ctx (unbox<'entity> entity)

  
  member this.Related (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>) =
    ToOneNullableRelationshipRelatedGetter<'ctx, 'entity, 'lookupType, 'relatedId>.Create(this.name, this.OptionalWithIdentifier, getRelated)


  member this.Included (getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>) =
    ToOneNullableRelationshipIncludedGetter.Create(this.name, getParser, this.idParsers |> Option.map (Map.toList >> List.map fst) |> Option.defaultValue [])


  interface Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId> with
    member this.Name = this.name

  interface FieldQueryParser<'ctx, 'entity, 'relatedId, string> with
    member this.Name = this.name
    member this.ToDomain ctx getInfo str =
      match this.idParsers |> Option.defaultValue Map.empty |> Map.toList with
      | [] -> failwith $"Relationship '%s{this.name}' does not contain any ID parsers and may not be used to parse query IDs"
      | [_, parseId] -> parseId ctx str |> TaskResult.mapError (List.map (fun getErr -> getErr (getInfo str)))
      | _::_::_ -> failwith $"Relationship '%s{this.name}' contains ID parsers for several types and may therefore not be used to parse query IDs"


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)
    member this.RequiresExplicitInclude = false


  interface RelationshipHandlers<'ctx> with

    member this.Name = this.name
    
    member _.IsToMany = false
    
    member this.IsSettable = this.set.IsSome

    member this.IsSettableButNotGettable = this.set.IsSome && this.get.IsNone

    member this.IsSettableWithoutPersist = this.set.IsSome && this.afterModifySelf.IsNone

    member this.GetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resDef resp ->
          fun next httpCtx ->
            task {
              match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok () ->
                  let entity = unbox<'entity> entity
                  match! getRelated ctx entity with
                  | Skip -> return! handleErrors [getRelWhileSkip ()] next httpCtx
                  | Include relatedEntity ->
                      let! doc = resp.WriteOpt httpCtx ctx req (relatedEntity |> Option.map (fun e ->
                        let b = resolveEntity e
                        b.resourceDef, b.entity
                      ))

                      let! fieldTrackerHandler =
                        match this.idParsers with
                        | None ->
                            logFieldTrackerPolymorphicRelTraversalWarning httpCtx resDef.TypeName this.name
                            Task.result (fun next ctx -> next ctx)
                        | Some parsers ->
                            let primaryResourceTypes = parsers.Keys |> Seq.toList
                            httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>().TrackFields(primaryResourceTypes, ctx, req, (resDef.TypeName, this.name))

                      let handler =
                        setStatusCode 200
                        >=> this.modifyGetRelatedResponse ctx entity relatedEntity
                        >=> fieldTrackerHandler
                        >=> jsonApiWithETag<'ctx> doc
                      return! handler next httpCtx
            }
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resDef resp ->
          fun next httpCtx ->
            task {
              match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok () ->
                  let entity = unbox<'entity> entity
                  match! getRelated ctx entity with
                  | Skip -> return! handleErrors [getRelWhileSkip ()] next httpCtx
                  | Include relatedEntity ->
                      let! included = getIncludedForSelfUrl httpCtx ctx req resp this.name resDef entity
                      let doc : ResourceIdentifierDocument = {
                        jsonapi = Skip
                        links = Skip
                        meta = Skip
                        data =
                          relatedEntity |> Option.map (fun e ->
                            let b = resolveEntity e
                            { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                        )
                        included = included
                      }

                      let! fieldTrackerHandler =
                        httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>()
                          .TrackFields(
                            [resDef.TypeName],
                            ctx,
                            req,
                            (resDef.TypeName, this.name),
                            this.name
                          )

                      let handler =
                        setStatusCode 200
                        >=> this.modifyGetSelfResponse ctx entity relatedEntity
                        >=> fieldTrackerHandler
                        >=> jsonApiWithETag<'ctx> doc
                      return! handler next httpCtx
            }
      )

    member _.PostSelf = None

    member this.PatchSelf =
      (this.get, this.set) ||> Option.map2 (fun getRelated set ->
        let idParsers =
          this.idParsers
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        let afterModifySelf =
          this.afterModifySelf
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without AfterModifySelf. This should be caught at startup.")
        fun ctx req _parentTypeName preconditions entity0 resDef resp ->
          fun next httpCtx ->
            task {
              match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok () ->
                  match! this.mapSetCtx ctx (unbox<'entity> entity0) with
                  | Error errs -> return! handleErrors errs next httpCtx
                  | Ok setCtx ->
                      match req.IdentifierDocument.Value with
                      | Error errs -> return! handleErrors errs next httpCtx
                      | Ok None -> return! handleErrors [modifyRelSelfMissingData ""] next httpCtx
                      | Ok (Some { data = identifier }) ->
                          match preconditions.Validate httpCtx ctx entity0 with
                          | Error errors -> return! handleErrors errors next httpCtx
                          | Ok () ->
                              match! this.beforeModifySelf setCtx (unbox<'entity> entity0) with
                              | Error errors -> return! handleErrors errors next httpCtx
                              | Ok entity1 ->
                                  let! entity2Res =
                                    identifier
                                    |> Option.traverseTaskResult (fun id ->
                                        match idParsers.TryGetValue id.``type`` with
                                        | false, _ ->
                                            let allowedTypes = idParsers |> Map.toList |> List.map fst
                                            Error [relInvalidTypeSelf id.``type`` allowedTypes "/data/type"] |> Task.result
                                        | true, parseId ->
                                            parseId ctx id.id
                                            |> TaskResult.map (fun x -> x, id)
                                            // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                            // this just means that the resource does not exist, which is a more helpful result.
                                            |> TaskResult.mapError (fun _ -> [relatedResourceNotFound id.``type`` id.id "/data"])
                                    )
                                    |> TaskResult.bind (fun relIdWithIdentifier -> set ctx setCtx "/data" relIdWithIdentifier (unbox<'entity> entity1))
                                  match entity2Res with
                                  | Error errs -> return! handleErrors errs next httpCtx
                                  | Ok entity2 ->
                                      match! afterModifySelf setCtx (unbox<'entity> entity0) (unbox<'entity> entity2) with
                                      | Error errors -> return! handleErrors errors next httpCtx
                                      | Ok entity3 ->
                                          if this.patchSelfReturn202Accepted then
                                            let handler =
                                              setStatusCode 202
                                              >=> this.modifyPatchSelfAcceptedResponse setCtx (unbox<'entity> entity3)
                                            return! handler next httpCtx
                                          else
                                            match! getRelated ctx (unbox<'entity> entity3) with
                                            | Skip ->
                                                let logger = httpCtx.GetLogger("Felicity.Relationships")
                                                logger.LogError("Relationship {RelationshipName} was updated using a self URL, but no success response could be returned because the relationship getter returned Skip. This violates the JSON:API specification. Make sure that the relationship getter never returns Skip after an update.", this.name)
                                                return! handleErrors [relModifySelfWhileSkip ()] next httpCtx
                                            | Include relatedEntity ->
                                                let! included = getIncludedForSelfUrl httpCtx ctx req resp this.name resDef entity3
                                                let doc : ResourceIdentifierDocument = {
                                                  jsonapi = Skip
                                                  links = Skip
                                                  meta = Skip
                                                  data =
                                                    relatedEntity |> Option.map (fun e ->
                                                      let b = resolveEntity e
                                                      { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                                                    )
                                                  included = included
                                                }

                                                let! fieldTrackerHandler =
                                                  httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>()
                                                    .TrackFields(
                                                      [resDef.TypeName],
                                                      ctx,
                                                      req,
                                                      (resDef.TypeName, this.name),
                                                      this.name
                                                    )

                                                let handler =
                                                  setStatusCode 200
                                                  >=> this.modifyPatchSelfOkResponse setCtx (unbox<'entity> entity3) relatedEntity
                                                  >=> fieldTrackerHandler
                                                  >=> jsonApiWithETag<'ctx> doc
                                                return! handler next httpCtx
            }
      )

    member _.DeleteSelf = None


  member this.Name = this.name

  /// Specify the order in which this field will be set relative to other fields during
  /// POST collection and PATCH resource requests. By default, all fields have SetOrder =
  /// 0. Negative numbers are allowed. The order of fields with identical SetOrder is
  /// unspecified.
  member this.SetOrder (i: int) =
    { this with setOrder = i }

  member this.GetTaskSkip(get: Func<'ctx, 'entity, Task<'relatedEntity option Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetTaskSkip(get: Func<'entity, Task<'relatedEntity option Skippable>>) =
    this.GetTaskSkip(fun _ e -> get.Invoke(e))

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity option Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetAsyncSkip(get: Func<'entity, Async<'relatedEntity option Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc get)

  member this.GetTask (get: Func<'ctx, 'entity, Task<'relatedEntity option>>) =
    this.GetTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetTask (get: Func<'entity, Task<'relatedEntity option>>) =
    this.GetTaskSkip(fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity option>>) =
    this.GetTask(Task.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity option>>) =
    this.GetTask(Task.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity option>>) =
    this.GetTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.result)

  member this.GetSkip (get: Func<'entity, Skippable<'relatedEntity option>>) =
    this.GetTaskSkip(fun _ r -> get.Invoke(r) |> Task.result)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity option>) =
    this.GetTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> Task.result)

  member this.Get (get: Func<'entity, 'relatedEntity option>) =
    this.GetTaskSkip(fun _ r -> get.Invoke r |> Include |> Task.result)

  member this.GetLinkageIfNotIncludedTaskSkip(get: Func<'ctx, 'entity, Task<'relatedId option Skippable>>) =
    if this.resolveId.IsNone then
      failwithf "Can only add linkage getter if the polymorphic resource definition contains an ID resolver."
    { this with
        getLinkageIfNotIncluded =
          fun ctx e ->
            task {
              match! get.Invoke(ctx, e) with
              | Skip -> return Skip
              | Include None -> return Include None
              | Include (Some relatedId) ->
                  let resDef = this.resolveId.Value relatedId
                  return Include <| Some {
                    ``type`` = resDef.name
                    id = resDef.id.fromDomain relatedId
                  }
            }
    }

  member this.GetLinkageIfNotIncludedAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedId option Skippable>>) =
    this.GetLinkageIfNotIncludedTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetLinkageIfNotIncludedTask (get: Func<'ctx, 'entity, Task<'relatedId option>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetLinkageIfNotIncludedTask (get: Func<'entity, Task<'relatedId option>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetLinkageIfNotIncludedAsync (get: Func<'ctx, 'entity, Async<'relatedId option>>) =
    this.GetLinkageIfNotIncludedTask(Task.liftAsyncFunc2 get)

  member this.GetLinkageIfNotIncludedAsync (get: Func<'entity, Async<'relatedId option>>) =
    this.GetLinkageIfNotIncludedTask(Task.liftAsyncFunc get)

  member this.GetLinkageIfNotIncludedSkip (get: Func<'ctx, 'entity, Skippable<'relatedId option>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.result)

  member this.GetLinkageIfNotIncluded (get: Func<'ctx, 'entity, 'relatedId option>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> Task.result)

  member this.GetLinkageIfNotIncluded (get: Func<'entity, 'relatedId option>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun _ r -> get.Invoke r |> Include |> Task.result)

  /// Omits the entire relationship (links, data, and meta) in the returned resource if the predicate returns true. If
  /// using one of the Get...Skip methods, this method should also be called. Otherwise, relationship links will always
  /// be present in the response, but GET operations against them will return an error if the getter returns Skip.
  member this.SkipRelationshipIf(predicate: 'ctx -> 'entity -> bool) =
    { this with skipRelationship = predicate }

  /// Omits the entire relationship (links, data, and meta) in the returned resource if the predicate returns true. If
  /// using one of the Get...Skip methods, this method should also be called. Otherwise, relationship links will always
  /// be present in the response, but GET operations against them will return an error if the getter returns Skip.
  member this.SkipRelationshipIf(predicate: 'entity -> bool) =
    { this with skipRelationship = fun _ e -> predicate e }

  member private this.SetTaskRes (set: Func<'ctx, 'setCtx, Pointer, ('relatedId * ResourceIdentifier) option, 'entity, Task<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with set = Some (fun ctx setCtx ptr relId e -> set.Invoke(ctx, setCtx, ptr, relId, e)) }

  member this.SetTaskRes (set: Func<'setCtx, 'relatedId option, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(fun _ ctx pointer relIdWithIdentifier e -> set.Invoke(ctx, (relIdWithIdentifier |> Option.map fst), e) |> TaskResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetTaskRes (set: Func<'relatedId option, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(fun _ id e -> set.Invoke(id, e))

  member this.SetTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType option, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(this.toIdSetter getRelated (fun ctx relId e -> set.Invoke(ctx, relId, e)))

  member this.SetTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType option, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(this.toIdSetter getRelated (fun _ id e -> set.Invoke(id, e)))

  member this.SetAsyncRes (set: Func<'setCtx, 'relatedId option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(Task.liftAsyncFunc3 set)

  member this.SetAsyncRes (set: Func<'relatedId option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(Task.liftAsyncFunc2 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(getRelated, Task.liftAsyncFunc3 set)

  member this.SetAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType option, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetTaskRes(getRelated, Task.liftAsyncFunc2 set)

  member this.SetTask (set: Func<'setCtx, 'relatedId option, 'entity, Task<'entity>>) =
    this.SetTaskRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.SetTask (set: Func<'relatedId option, 'entity, Task<'entity>>) =
    this.SetTaskRes(fun _ related entity -> set.Invoke(related, entity) |> Task.map Ok)

  member this.SetTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType option, 'entity, Task<'entity>>) =
    this.SetTaskRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Task.map Ok))

  member this.SetTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType option, 'entity, Task<'entity>>) =
    this.SetTaskRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Task.map Ok))

  member this.SetAsync (set: Func<'setCtx, 'relatedId option, 'entity, Async<'entity>>) =
    this.SetTask(Task.liftAsyncFunc3 set)

  member this.SetAsync (set: Func<'relatedId option, 'entity, Async<'entity>>) =
    this.SetTask(Task.liftAsyncFunc2 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType option, 'entity, Async<'entity>>) =
    this.SetTask(getRelated, Task.liftAsyncFunc3 set)

  member this.SetAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType option, 'entity, Async<'entity>>) =
    this.SetTask(getRelated, Task.liftAsyncFunc2 set)

  member this.SetRes (set: Func<'setCtx, 'relatedId option, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(Task.liftFunc3 set)

  member this.SetRes (set: Func<'relatedId option, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(Task.liftFunc2 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType option, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(getRelated, Task.liftFunc3 set)

  member this.SetRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType option, 'entity, Result<'entity, Error list>>) =
    this.SetTaskRes(getRelated, Task.liftFunc2 set)

  member this.Set (set: Func<'setCtx, 'relatedId option, 'entity, 'entity>) =
    this.SetTaskRes(TaskResult.liftFunc3 set)

  member this.Set (set: Func<'relatedId option, 'entity, 'entity>) =
    this.SetTaskRes(TaskResult.liftFunc2 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType option, 'entity, 'entity>) =
    this.SetTaskRes(getRelated, TaskResult.liftFunc3 set)

  member this.Set (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType option, 'entity, 'entity>) =
    this.SetTaskRes(getRelated, TaskResult.liftFunc2 set)

  member this.SetNonNullTaskRes (set: Func<'setCtx, 'relatedId, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(fun ctx relId e ->
      relId
      |> Result.requireSome [setRelNullNotAllowed this.name]
      |> Task.result
      |> TaskResult.bind (fun relId -> set.Invoke(ctx, relId, e))
    )

  member this.SetNonNullTaskRes (set: Func<'relatedId, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetNonNullTaskRes(fun _ id e -> set.Invoke(id, e))

  member this.SetNonNullTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetTaskRes(getRelated, fun ctx relId e ->
      relId
      |> Result.requireSome [setRelNullNotAllowed this.name]
      |> Task.result
      |> TaskResult.bind (fun relId -> set.Invoke(ctx, relId, e))
    )

  member this.SetNonNullTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetNonNullTaskRes(getRelated, fun _ id e -> set.Invoke(id, e))

  member this.SetNonNullAsyncRes (set: Func<'setCtx, 'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullTaskRes(Task.liftAsyncFunc3 set)

  member this.SetNonNullAsyncRes (set: Func<'relatedId, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullTaskRes(Task.liftAsyncFunc2 set)

  member this.SetNonNullAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullTaskRes(getRelated, Task.liftAsyncFunc3 set)

  member this.SetNonNullAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetNonNullTaskRes(getRelated, Task.liftAsyncFunc2 set)

  member this.SetNonNullTask (set: Func<'setCtx, 'relatedId, 'entity, Task<'entity>>) =
    this.SetNonNullTaskRes(fun ctx related entity -> set.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.SetNonNullTask (set: Func<'relatedId, 'entity, Task<'entity>>) =
    this.SetNonNullTaskRes(fun _ related entity -> set.Invoke(related, entity) |> Task.map Ok)

  member this.SetNonNullTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Task<'entity>>) =
    this.SetNonNullTaskRes(getRelated, (fun ctx related entity -> set.Invoke(ctx, related, entity) |> Task.map Ok))

  member this.SetNonNullTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Task<'entity>>) =
    this.SetNonNullTaskRes(getRelated, (fun _ related entity -> set.Invoke(related, entity) |> Task.map Ok))

  member this.SetNonNullAsync (set: Func<'setCtx, 'relatedId, 'entity, Async<'entity>>) =
    this.SetNonNullTask(Task.liftAsyncFunc3 set)

  member this.SetNonNullAsync (set: Func<'relatedId, 'entity, Async<'entity>>) =
    this.SetNonNullTask(Task.liftAsyncFunc2 set)

  member this.SetNonNullAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Async<'entity>>) =
    this.SetNonNullTask(getRelated, Task.liftAsyncFunc3 set)

  member this.SetNonNullAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Async<'entity>>) =
    this.SetNonNullTask(getRelated, Task.liftAsyncFunc2 set)

  member this.SetNonNullRes (set: Func<'setCtx, 'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullTaskRes(Task.liftFunc3 set)

  member this.SetNonNullRes (set: Func<'relatedId, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullTaskRes(Task.liftFunc2 set)

  member this.SetNonNullRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullTaskRes(getRelated, Task.liftFunc3 set)

  member this.SetNonNullRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, Result<'entity, Error list>>) =
    this.SetNonNullTaskRes(getRelated, Task.liftFunc2 set)

  member this.SetNonNull (set: Func<'setCtx, 'relatedId, 'entity, 'entity>) =
    this.SetNonNullTaskRes(TaskResult.liftFunc3 set)

  member this.SetNonNull (set: Func<'relatedId, 'entity, 'entity>) =
    this.SetNonNullTaskRes(TaskResult.liftFunc2 set)

  member this.SetNonNull (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'setCtx, 'lookupType, 'entity, 'entity>) =
    this.SetNonNullTaskRes(getRelated, TaskResult.liftFunc3 set)

  member this.SetNonNull (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, set: Func<'lookupType, 'entity, 'entity>) =
    this.SetNonNullTaskRes(getRelated, TaskResult.liftFunc2 set)

  member this.AddConstraintsTask(getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            task {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsTask(Task.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsTask(Task.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsTask(fun ctx e -> [name, box (getValue ctx e)] |> Task.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.BeforeModifySelfTaskRes(f: Func<'setCtx, 'entity, Task<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfTaskRes(f: Func<'setCtx, 'entity, Task<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> TaskResult.map (fun () -> e))

  member this.BeforeModifySelfTaskRes(f: Func<'entity, Task<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfTaskRes(f: Func<'entity, Task<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'setCtx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'setCtx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc f)

  member this.BeforeModifySelfTask(f: Func<'setCtx, 'entity, Task<'entity>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'setCtx, 'entity, Task<unit>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'entity, Task<'entity>>) =
    this.BeforeModifySelfTaskRes(fun e -> f.Invoke e |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'entity, Task<unit>>) =
    this.BeforeModifySelfTaskRes(fun e -> f.Invoke e |> Task.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'setCtx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'setCtx, 'entity, Async<unit>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc f)

  member this.BeforeModifySelfRes(f: Func<'setCtx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'setCtx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc f)

  member this.BeforeModifySelf(f: Func<'setCtx, 'entity, 'entity>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'setCtx, 'entity, unit>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc f)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc f)

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some f }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> 'entity -> Task<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew |> TaskResult.map (fun () -> eNew)) }

  member this.AfterModifySelfTaskRes(f: 'entity -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun _ eOld eNew -> f eOld eNew) }

  member this.AfterModifySelfTaskRes(f: 'entity -> 'entity -> Task<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun _ eOld eNew -> f eOld eNew |> TaskResult.map (fun () -> eNew)) }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx _ e -> f ctx e) }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> Task<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> TaskResult.map (fun () -> e))

  member this.AfterModifySelfTaskRes(f: 'entity -> Task<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ _ e -> f e)

  member this.AfterModifySelfTaskRes(f: 'entity -> Task<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ _ e -> f e |> TaskResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync f)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun ctx eOld eNew -> f ctx eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun ctx eOld eNew -> f ctx eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun e -> f e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun e -> f e |> Task.map Ok)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift3 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift3 f)

  member this.AfterModifySelfRes(f: 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift f)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift3 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift3 f)

  member this.AfterModifySelf(f: 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift f)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift f)

  member this.PatchSelfReturn202Accepted () =
    { this with patchSelfReturn202Accepted = true }

  member this.ModifyGetRelatedResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler) =
    { this with modifyGetRelatedResponse = getHandler }

  member this.ModifyGetRelatedResponse(f: 'ctx -> 'entity -> 'relatedEntity option -> HttpContext -> unit) =
    this.ModifyGetRelatedResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetRelatedResponse(handler: HttpHandler) =
    this.ModifyGetRelatedResponse(fun _ _ _ -> handler)

  member this.ModifyGetSelfResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity option -> HttpHandler) =
    { this with modifyGetSelfResponse = getHandler }

  member this.ModifyGetSelfResponse(f: 'ctx -> 'entity -> 'relatedEntity option -> HttpContext -> unit) =
    this.ModifyGetSelfResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetSelfResponse(handler: HttpHandler) =
    this.ModifyGetSelfResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfOkResponse(getHandler: 'setCtx -> 'entity -> 'relatedEntity option -> HttpHandler) =
    { this with modifyPatchSelfOkResponse = getHandler }

  member this.ModifyPatchSelfOkResponse(f: 'setCtx -> 'entity -> 'relatedEntity option -> HttpContext -> unit) =
    this.ModifyPatchSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPatchSelfOkResponse(handler: HttpHandler) =
    this.ModifyPatchSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfAcceptedResponse(getHandler: 'setCtx -> 'entity -> HttpHandler) =
    { this with modifyPatchSelfAcceptedResponse = getHandler }

  member this.ModifyPatchSelfAcceptedResponse(f: 'setCtx -> 'entity -> HttpContext -> unit) =
    this.ModifyPatchSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPatchSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPatchSelfAcceptedResponse(fun _ _ -> handler)



[<AutoOpen>]
module ToOneNullableRelationshipExtensions =

  type ToOneNullableRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> with

    member this.AddConstraint (name: string, value: 'a) =
      this.AddConstraint(name, fun _ -> value)



type internal ToManyRelationship<'ctx> =
  abstract Name: RelationshipName
  abstract SelfLink: bool
  abstract RelatedLink: bool
  abstract AllowedTypes: ICollection<ResourceTypeName> option
  abstract BoxedGetRelated: ('ctx -> BoxedEntity -> Task<Skippable<(ResourceDefinition<'ctx> * BoxedEntity) list>>) option
  abstract GetLinkageIfNotIncluded: 'ctx -> BoxedEntity -> Task<Skippable<ResourceIdentifier list>>
  abstract SkipRelationship: 'ctx -> BoxedEntity -> bool


type ToManyRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  idGetter: RequestGetter<'ctx, ('relatedId * ResourceIdentifier) list option>
  getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>
} with

  static member internal Create(name, idGetter: RequestGetter<'ctx, ('relatedId * ResourceIdentifier) list option>, getRelated: ResourceLookup<'ctx, 'relatedEntity, 'relatedId>) : ToManyRelationshipRelatedGetter<'ctx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      idGetter = idGetter
      getRelated = getRelated
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity list option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          this.idGetter.Get(ctx, req, includedTypeAndId)
          |> TaskResult.bind (
              Option.traverseTaskResult (
                List.indexed
                >> List.traverseTaskResultA (fun (i, (resId, identifier)) ->
                    this.getRelated.GetById ctx resId
                    |> TaskResult.mapError (List.map (Error.setSourcePointer ("/data/relationships/" + this.name + "/data/" + string i)))
                    |> TaskResult.requireSome [relatedResourceNotFound identifier.``type`` identifier.id ("/data/relationships/" + this.name + "/data/" + string i)]
                )
              )
          )
    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToManyRelationshipIncludedGetter<'ctx, 'relatedEntity> = internal {
  name: string
  getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>
  allowedTypes: ResourceTypeName list
} with

  static member internal Create(name, getParser, allowedTypes) : ToManyRelationshipIncludedGetter<'ctx, 'relatedEntity> =
    {
      name = name
      getParser = getParser
      allowedTypes = allowedTypes
    }

  member this.Optional =
    { new RequestGetter<'ctx, 'relatedEntity list option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToMany as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + "/" + this.name)] |> Task.result
                  | { data = Include list } ->
                      list
                      |> List.indexed
                      |> List.traverseTaskResultA (fun (i, identifier) ->
                          if not (this.allowedTypes |> List.contains identifier.``type``) then
                            let pointer = relsPointer + "/" + this.name + "/data/" + string i + "/type"
                            Error [relInvalidType this.name identifier.``type`` this.allowedTypes pointer] |> Task.result
                          else
                            RequestParserHelper<'ctx>(ctx, req, (identifier.``type``, identifier.id))
                            |> this.getParser
                            |> fun p -> p.ParseTask()
                      )
                      |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToMany>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result

    }

  interface OptionalRequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'relatedEntity list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]



type ToManyRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> = internal {
  name: string
  setOrder: int
  mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  resolveId: ('relatedId -> ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Task<Result<'relatedId, (ParsedValueInfo -> Error) list>>> option
  get: ('ctx -> 'entity -> Task<'relatedEntity list Skippable>) option
  setAll: ('ctx -> 'setCtx -> Pointer -> ('relatedId * ResourceIdentifier) list -> 'entity -> Task<Result<'entity, Error list>>) option
  add: ('ctx -> 'setCtx -> Pointer -> ('relatedId * ResourceIdentifier) list -> 'entity -> Task<Result<'entity, Error list>>) option
  remove: ('ctx -> 'setCtx -> Pointer -> ('relatedId * ResourceIdentifier) list -> 'entity -> Task<Result<'entity, Error list>>) option
  getLinkageIfNotIncluded: 'ctx -> 'entity -> Task<ResourceIdentifier list Skippable>
  skipRelationship: 'ctx -> 'entity -> bool
  hasConstraints: bool
  getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>
  beforeModifySelf: 'setCtx -> 'entity -> Task<Result<'entity, Error list>>
  afterModifySelf: ('setCtx -> 'entity -> 'entity -> Task<Result<'entity, Error list>>) option
  modifyGetRelatedResponse: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyGetSelfResponse: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyPostSelfOkResponse: 'setCtx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyPostSelfAcceptedResponse: 'setCtx -> 'entity -> HttpHandler
  modifyPatchSelfOkResponse: 'setCtx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyPatchSelfAcceptedResponse: 'setCtx -> 'entity -> HttpHandler
  modifyDeleteSelfOkResponse: 'setCtx -> 'entity -> 'relatedEntity list -> HttpHandler
  modifyDeleteSelfAcceptedResponse: 'setCtx -> 'entity -> HttpHandler
  modifySelfReturn202Accepted: bool
} with

  static member internal Create(name: string, mapSetCtx, resolveEntity, resolveId, idParsers) : ToManyRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> =
    {
      name = name
      setOrder = 0
      mapSetCtx = mapSetCtx
      resolveEntity = resolveEntity
      idParsers = idParsers
      resolveId = resolveId
      get = None
      setAll = None
      add = None
      remove = None
      getLinkageIfNotIncluded = fun _ _ -> Task.result Skip
      skipRelationship = fun _ _ -> false
      hasConstraints = false
      getConstraints = fun _ _ -> Task.result []
      beforeModifySelf = fun _ e -> Ok e |> Task.result
      afterModifySelf = None
      modifyGetRelatedResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyGetSelfResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPostSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPostSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      modifyPatchSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyPatchSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      modifyDeleteSelfOkResponse = fun _ _ _ -> fun next ctx -> next ctx
      modifyDeleteSelfAcceptedResponse = fun _ _ -> fun next ctx -> next ctx
      modifySelfReturn202Accepted = false
    }

  member private _.toIdSetter (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>) (entitySetter: 'setCtx -> 'lookupType list -> 'entity -> Task<Result<'entity, Error list>>) =
    fun ctx setCtx (dataPointer: Pointer) (relatedIdsWithIdentifiers: ('relatedId * ResourceIdentifier) list) entity ->
      relatedIdsWithIdentifiers
      |> Seq.map (fun (relId, identifier) ->
          getRelated.GetById ctx relId
          |> TaskResult.map (fun resOpt -> resOpt, identifier)
      )
      |> Task.WhenAll
      |> Task.map (
          Seq.indexed
          >> Seq.toList
          >> List.traverseResultA (fun (i, t) ->
              t
              |> Result.mapError (List.map (Error.setSourcePointer (dataPointer + "/" + string i)))
              |> Result.bind (fun (resOpt, identifier) ->
                  resOpt
                  |> Result.requireSome [relatedResourceNotFound identifier.``type`` identifier.id (dataPointer + "/" + string i)]
              )
          )
      )
      |> TaskResult.bind (fun r ->
          entitySetter setCtx r entity
          |> TaskResult.mapError (List.map (Error.setSourcePointer dataPointer))
      )


  member private this.OptionalWithIdentifier =
    { new RequestGetter<'ctx, ('relatedId * ResourceIdentifier) list option> with
        member _.FieldName = Some this.name
        member _.QueryParamName = None
        member _.Get(ctx, req, includedTypeAndId) =
          let idParsers =
            this.idParsers
            |> Option.defaultWith (fun () -> failwith $"Attempted to parse resource ID for polymorphic relationship '%s{this.name}', but no ID parsers have been specified.")
          match Request.getRelsAndPointer includedTypeAndId req with
          | Error errs -> Error errs |> Task.result
          | Ok None -> None |> Ok |> Task.result
          | Ok (Some (rels, relsPointer)) ->
              match rels.TryGetValue this.name with
              | true, (:? ToMany as rel) ->
                  match rel with
                  | { data = Skip } -> Error [relMissingData this.name (relsPointer + this.name)] |> Task.result
                  | { data = Include list } ->
                      list
                      |> List.indexed
                      |> List.traverseTaskResultA (fun (i, identifier) ->
                          match idParsers.TryGetValue identifier.``type`` with
                          | false, _ ->
                              let allowedTypes = idParsers |> Map.toList |> List.map fst
                              let pointer = relsPointer + this.name + "/data/" + string i + "/type"
                              Error [relInvalidType this.name identifier.``type`` allowedTypes pointer] |> Task.result
                          | true, parseId ->
                              parseId ctx identifier.id
                              |> TaskResult.map (fun x -> x, identifier)
                              // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                              // this just means that the resource does not exist, which is a more helpful result.
                              |> TaskResult.mapError (fun _ -> [relatedResourceNotFound identifier.``type`` identifier.id (relsPointer + this.name + "/data/" + string i)])
                      )
                      |> TaskResult.map Some
              | true, x -> failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToMany>.FullName}, but was %s{x.GetType().FullName}"
              | false, _ -> None |> Ok |> Task.result
    }


  member this.Optional =
    { new RequestGetter<'ctx, 'relatedId list option> with
        member _.FieldName = this.OptionalWithIdentifier.FieldName
        member _.QueryParamName = this.OptionalWithIdentifier.QueryParamName
        member _.Get(ctx, req, includedTypeAndId) =
          this.OptionalWithIdentifier.Get(ctx, req, includedTypeAndId)
          |> TaskResult.map (Option.map (List.map fst))
    }

  interface OptionalRequestGetter<'ctx, 'relatedId list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)


  interface RequestGetter<'ctx, 'relatedId list> with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      let pointer = Request.pointerForMissingRel includedTypeAndId req
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredRel this.name pointer]

  interface ProhibitedRequestGetter with
    member this.FieldName = Some this.name
    member this.QueryParamName = None
    member this.GetErrors(req, includedTypeAndId) =
      match req.Document.Value with
      | Error errs -> errs
      | Ok (Some { data = Some { relationships = Include rels } }) when rels.ContainsKey this.name ->
          let pointer = Request.pointerForMissingRel includedTypeAndId req + "/" + this.name
          [reqParserProhibitedRel this.name pointer]
      | _ -> []


  interface FieldSetter<'ctx> with
    member this.Names = Set.singleton this.name
    member this.SetOrder = this.setOrder
    member this.Set ctx req entity numSetters =
      task {
        match req.Document.Value with
        | Error errs -> return Error errs
        | Ok (Some { data = Some { ``type`` = t; relationships = Include rels } }) ->
            match this.setAll, rels.TryGetValue this.name with
            | _, (false, _) -> return Ok (entity, false) // not provided in request
            | None, (true, _) ->
                if numSetters[this.name] > 1 then
                  // Provided in request and no setter, but there exists another setter, so ignore
                  return Ok (entity, false)
                else
                  if this.add.IsNone && this.remove.IsNone then
                    return Error [setRelReadOnly this.name ("/data/relationships/" + this.name)]
                  else
                    return Error [setToManyRelReplacementNotSupported this.name t ("/data/relationships/" + this.name) this.add.IsSome this.remove.IsSome]
            | Some set, (true, (:? ToMany as rel)) ->
              match! this.mapSetCtx ctx (unbox<'entity> entity) with
              | Error errs -> return Error errs
              | Ok setCtx ->
                  let idParsers =
                    this.idParsers
                    |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
                  match rel.data with
                  | Skip -> return Error [relMissingData this.name ("/data/relationships/" + this.name)]
                  | Include identifiers ->
                      return!
                        identifiers
                        |> List.indexed
                        |> List.traverseTaskResultA (fun (i, id) ->
                            match idParsers.TryGetValue id.``type`` with
                            | false, _ ->
                                let allowedTypes = idParsers |> Map.toList |> List.map fst
                                let pointer = "/data/relationships/" + this.name + "/data/" + string i + "/type"
                                Error [relInvalidType this.name id.``type`` allowedTypes pointer] |> Task.result
                            | true, parseId ->
                                parseId ctx id.id
                                |> TaskResult.map (fun x -> x, id)
                                // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                // this just means that the resource does not exist, which is a more helpful result.
                                |> TaskResult.mapError (fun _ -> [relatedResourceNotFound id.``type`` id.id ("/data/relationships/" + this.name + "/data/" + string i)])
                        )
                        |> TaskResult.bind (fun relIdWithIdentifier ->
                            set ctx setCtx ("/data/relationships/" + this.name + "/data") relIdWithIdentifier (unbox<'entity> entity)
                        )
                        |> TaskResult.map (fun e -> box<'entity> e, true)
            | Some _, (true, rel) -> return failwith $"Framework bug: Expected relationship '%s{this.name}' to be deserialized to %s{typeof<ToMany>.FullName}, but was %s{rel.GetType().FullName}"
        | _ -> return Ok (entity, false)  // no relationships provided
      }


  interface ToManyRelationship<'ctx> with
    member this.Name = this.name
    member this.SelfLink = this.get.IsSome
    member this.RelatedLink = this.get.IsSome
    member this.AllowedTypes = this.idParsers |> Option.map Map.keys
    member this.BoxedGetRelated =
      this.get |> Option.map (fun get ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx entity ->
          get ctx (unbox<'entity> entity) |> Task.map (Skippable.map (List.map (fun x ->
            let b = resolveEntity x
            b.resourceDef, b.entity
          )))
      )
    member this.GetLinkageIfNotIncluded ctx entity =
      this.getLinkageIfNotIncluded ctx (unbox<'entity> entity)
    member this.SkipRelationship ctx entity =
      this.skipRelationship ctx (unbox<'entity> entity)


  member this.Related (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>) =
    ToManyRelationshipRelatedGetter<'ctx, 'entity, 'lookupType, 'relatedId>.Create(this.name, this.OptionalWithIdentifier, getRelated)


  member this.Included (getParser: RequestParserHelper<'ctx> -> RequestParser<'ctx, 'relatedEntity>) =
    ToManyRelationshipIncludedGetter.Create(this.name, getParser, this.idParsers |> Option.map (Map.toList >> List.map fst) |> Option.defaultValue [])


  interface Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId> with
    member this.Name = this.name

  interface FieldQueryParser<'ctx, 'entity, 'relatedId, string> with
    member this.Name = this.name
    member this.ToDomain ctx getInfo str =
      match this.idParsers |> Option.defaultValue Map.empty |> Map.toList with
      | [] -> failwith $"Relationship '%s{this.name}' does not contain any ID parsers and may not be used to parse query IDs"
      | [_, parseId] -> parseId ctx str |> TaskResult.mapError (List.map (fun getErr -> getErr (getInfo str)))
      | _::_::_ -> failwith $"Relationship '%s{this.name}' contains ID parsers for several types and may therefore not be used to parse query IDs"


  interface Field<'ctx> with
    member this.Name = this.name


  interface ConstrainedField<'ctx> with
    member this.Name = this.name
    member this.HasConstraints = this.hasConstraints
    member this.BoxedGetConstraints ctx e =
      this.getConstraints ctx (unbox<'entity> e)
    member this.RequiresExplicitInclude = false


  member private this.ModifySelfHandler f modifyOkResponse modifyAcceptedResponse =
    (this.get, f) ||> Option.map2 (fun getRelated f ->
      let idParsers =
        this.idParsers
        |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without ID parsers. This should be caught at startup.")
      let resolveEntity =
        this.resolveEntity
        |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
      let afterModifySelf =
        this.afterModifySelf
        |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship setter defined without AfterModifySelf. This should be caught at startup.")
      fun ctx req (preconditions: Preconditions<'ctx>) entity0 resDef (resp: ResponseBuilder<'ctx>) ->
        fun next httpCtx ->
          task {
            match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
            | Error errs -> return! handleErrors errs next httpCtx
            | Ok () ->
                match! this.mapSetCtx ctx (unbox<'entity> entity0) with
                | Error errs -> return! handleErrors errs next httpCtx
                | Ok setCtx ->
                    match req.IdentifierCollectionDocument.Value with
                    | Error errs -> return! handleErrors errs next httpCtx
                    | Ok None -> return! handleErrors [modifyRelSelfMissingData ""] next httpCtx
                    | Ok (Some { data = ids }) ->
                        match preconditions.Validate httpCtx ctx entity0 with
                        | Error errors -> return! handleErrors errors next httpCtx
                        | Ok () ->
                            match! this.beforeModifySelf setCtx (unbox<'entity> entity0) with
                            | Error errors -> return! handleErrors errors next httpCtx
                            | Ok entity1 ->
                                let! entity2Res =
                                  ids
                                  |> Array.traverseTaskResultAIndexed (fun i id ->
                                      match idParsers.TryGetValue id.``type`` with
                                      | false, _ ->
                                          let allowedTypes = idParsers |> Map.toList |> List.map fst
                                          let pointer = "/data/" + string i + "/type"
                                          Error [relInvalidTypeSelf id.``type`` allowedTypes pointer] |> Task.result
                                      | true, parseId ->
                                          parseId ctx id.id
                                          |> TaskResult.map (fun x -> x, id)
                                          // Ignore ID parsing errors; in the context of fetching a related resource by ID,
                                          // this just means that the resource does not exist, which is a more helpful result.
                                          |> TaskResult.mapError (fun _ -> [relatedResourceNotFound id.``type`` id.id ("/data/" + string i)])
                                  )
                                  |> TaskResult.bind (fun domain -> f ctx setCtx "/data" (Array.toList domain) (unbox<'entity> entity1))
                                match entity2Res with
                                | Error errs -> return! handleErrors errs next httpCtx
                                | Ok entity2 ->
                                    match! afterModifySelf setCtx (unbox<'entity> entity0) (unbox<'entity> entity2) with
                                    | Error errors -> return! handleErrors errors next httpCtx
                                    | Ok entity3 ->
                                        if this.modifySelfReturn202Accepted then
                                          let handler =
                                            setStatusCode 202
                                            >=> modifyAcceptedResponse setCtx (unbox<'entity> entity3)
                                          return! handler next httpCtx
                                        else
                                          match! getRelated ctx (unbox<'entity> entity3) with
                                          | Skip ->
                                              let logger = httpCtx.GetLogger("Felicity.Relationships")
                                              logger.LogError("Relationship {RelationshipName} was updated using a self URL, but no success response could be returned because the relationship getter returned Skip. This violates the JSON:API specification. Make sure that the relationship getter never returns Skip after an update.", this.name)
                                              return! handleErrors [relModifySelfWhileSkip ()] next httpCtx
                                          | Include relatedEntities ->
                                              let! included = getIncludedForSelfUrl httpCtx ctx req resp this.name resDef entity3
                                              let doc : ResourceIdentifierCollectionDocument = {
                                                jsonapi = Skip
                                                links = Skip
                                                meta = Skip
                                                data =
                                                  relatedEntities
                                                  |> Seq.map (fun e ->
                                                      let b = resolveEntity e
                                                      { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                                                  )
                                                  |> Seq.toArray
                                                included = included
                                              }

                                              let! fieldTrackerHandler =
                                                httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>()
                                                  .TrackFields(
                                                    [resDef.TypeName],
                                                    ctx,
                                                    req,
                                                    (resDef.TypeName, this.name),
                                                    this.name
                                                  )

                                              let handler =
                                                setStatusCode 200
                                                >=> modifyOkResponse setCtx (unbox<'entity> entity3) relatedEntities
                                                >=> fieldTrackerHandler
                                                >=> jsonApiWithETag<'ctx> doc
                                              return! handler next httpCtx
          }
    )


  interface RelationshipHandlers<'ctx> with

    member this.Name = this.name
    
    member _.IsToMany = true

    member this.IsSettable = this.setAll.IsSome || this.add.IsSome || this.remove.IsSome

    member this.IsSettableButNotGettable = this.setAll.IsSome && this.get.IsNone

    member this.IsSettableWithoutPersist = (this.setAll.IsSome || this.add.IsSome || this.remove.IsSome) && this.afterModifySelf.IsNone

    member this.GetRelated =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resDef resp ->
          fun next httpCtx ->
            task {
              // TODO: Support 'sort' query parameter
              if httpCtx.TryGetQueryStringValue "sort" |> Option.isSome then
                return! handleErrors [sortNotSupported ()] next httpCtx
              else
                match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
                | Error errs -> return! handleErrors errs next httpCtx
                | Ok () ->
                  let entity = unbox<'entity> entity
                  match! getRelated ctx entity with
                  | Skip -> return! handleErrors [getRelWhileSkip ()] next httpCtx
                  | Include relatedEntities ->
                      let! doc = resp.WriteList httpCtx ctx req (relatedEntities |> List.map (fun e ->
                        let b = resolveEntity e
                        b.resourceDef, b.entity
                      ))

                      let! fieldTrackerHandler =
                        match this.idParsers with
                        | None ->
                            logFieldTrackerPolymorphicRelTraversalWarning httpCtx resDef.TypeName this.name
                            Task.result (fun next ctx -> next ctx)
                        | Some parsers ->
                            let primaryResourceTypes = parsers.Keys |> Seq.toList
                            httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>().TrackFields(primaryResourceTypes, ctx, req, (resDef.TypeName, this.name))

                      let handler =
                        setStatusCode 200
                        >=> this.modifyGetRelatedResponse ctx entity relatedEntities
                        >=> fieldTrackerHandler
                        >=> jsonApiWithETag<'ctx> doc
                      return! handler next httpCtx
            }
      )

    member this.GetSelf =
      this.get |> Option.map (fun getRelated ->
        let resolveEntity =
          this.resolveEntity
          |> Option.defaultWith (fun () -> failwithf "Framework bug: Relationship getter defined without entity resolver. This should be caught at startup.")
        fun ctx req entity resDef resp ->
          fun next httpCtx ->
            task {
              match StrictModeHelpers.checkForUnknownQueryParameters<'ctx> httpCtx req Set.empty with
              | Error errs -> return! handleErrors errs next httpCtx
              | Ok () ->
                  let entity = unbox<'entity> entity
                  match! getRelated ctx entity with
                  | Skip -> return! handleErrors [getRelWhileSkip ()] next httpCtx
                  | Include relatedEntities ->
                      let! included = getIncludedForSelfUrl httpCtx ctx req resp this.name resDef entity
                      let doc : ResourceIdentifierCollectionDocument = {
                        jsonapi = Skip
                        links = Skip
                        meta = Skip
                        data =
                          relatedEntities
                          |> Seq.map (fun e ->
                              let b = resolveEntity e
                              { ``type`` = b.resourceDef.TypeName; id = b.resourceDef.GetIdBoxed b.entity }
                          )
                          |> Seq.toArray
                        included = included
                      }

                      let! fieldTrackerHandler =
                        httpCtx.RequestServices.GetRequiredService<FieldTracker<'ctx>>()
                          .TrackFields(
                            [resDef.TypeName],
                            ctx,
                            req,
                            (resDef.TypeName, this.name),
                            this.name
                          )

                      let handler =
                        setStatusCode 200
                        >=> this.modifyGetSelfResponse ctx entity relatedEntities
                        >=> fieldTrackerHandler
                        >=> jsonApiWithETag<'ctx> doc
                      return! handler next httpCtx
            }
      )

    member this.PostSelf =
      this.ModifySelfHandler this.add this.modifyPostSelfOkResponse this.modifyPostSelfAcceptedResponse

    member this.PatchSelf =
      this.ModifySelfHandler this.setAll this.modifyPatchSelfOkResponse this.modifyPatchSelfAcceptedResponse
      |> Option.map (fun patch -> fun ctx req _ entity -> patch ctx req entity)
      
    member this.DeleteSelf =
      this.ModifySelfHandler this.remove this.modifyDeleteSelfOkResponse this.modifyDeleteSelfAcceptedResponse


  member this.Name = this.name

  /// Specify the order in which this field will be set relative to other fields during
  /// POST collection and PATCH resource requests. By default, all fields have SetOrder =
  /// 0. Negative numbers are allowed. The order of fields with identical SetOrder is
  /// unspecified.
  member this.SetOrder (i: int) =
    { this with setOrder = i }

  member this.GetTaskSkip(get: Func<'ctx, 'entity, Task<'relatedEntity list Skippable>>) =
    if this.resolveEntity.IsNone then
      failwithf "Can only add getter if the polymorphic resource definition contains an entity resolver."
    { this with get = Some (fun ctx e -> get.Invoke(ctx, e)) }

  member this.GetTaskSkip(get: Func<'entity, Task<'relatedEntity list Skippable>>) =
    this.GetTaskSkip(fun _ e -> get.Invoke(e))

  member this.GetAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedEntity list Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetAsyncSkip(get: Func<'entity, Async<'relatedEntity list Skippable>>) =
    this.GetTaskSkip(Task.liftAsyncFunc get)

  member this.GetTask (get: Func<'ctx, 'entity, Task<'relatedEntity list>>) =
    this.GetTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetTask (get: Func<'entity, Task<'relatedEntity list>>) =
    this.GetTaskSkip(fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetAsync (get: Func<'ctx, 'entity, Async<'relatedEntity list>>) =
    this.GetTask(Task.liftAsyncFunc2 get)

  member this.GetAsync (get: Func<'entity, Async<'relatedEntity list>>) =
    this.GetTask(Task.liftAsyncFunc get)

  member this.GetSkip (get: Func<'ctx, 'entity, Skippable<'relatedEntity list>>) =
    this.GetTaskSkip(Task.liftFunc2 get)

  member this.GetSkip (get: Func<'entity, Skippable<'relatedEntity list>>) =
    this.GetTaskSkip(Task.liftFunc get)

  member this.Get (get: Func<'ctx, 'entity, 'relatedEntity list>) =
    this.GetTask(Task.liftFunc2 get)

  member this.Get (get: Func<'entity, 'relatedEntity list>) =
    this.GetTask(Task.liftFunc get)

  member this.GetLinkageIfNotIncludedTaskSkip(get: Func<'ctx, 'entity, Task<'relatedId list Skippable>>) =
    if this.resolveId.IsNone then
      failwithf "Can only add linkage getter if the polymorphic resource definition contains an ID resolver."
    { this with
        getLinkageIfNotIncluded =
          fun ctx e ->
            task {
              match! get.Invoke(ctx, e) with
              | Skip -> return Skip
              | Include relatedIds ->
                  return
                    relatedIds
                    |> List.map (fun relId ->
                        let resDef = this.resolveId.Value relId
                        {
                          ``type`` = resDef.name
                          id = resDef.id.fromDomain relId
                        }
                    )
                    |> Include
            }
    }

  member this.GetLinkageIfNotIncludedAsyncSkip(get: Func<'ctx, 'entity, Async<'relatedId list Skippable>>) =
    this.GetLinkageIfNotIncludedTaskSkip(Task.liftAsyncFunc2 get)

  member this.GetLinkageIfNotIncludedTask (get: Func<'ctx, 'entity, Task<'relatedId list>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.map Include)

  member this.GetLinkageIfNotIncludedTask (get: Func<'entity, Task<'relatedId list>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun _ r -> get.Invoke r |> Task.map Include)

  member this.GetLinkageIfNotIncludedAsync (get: Func<'ctx, 'entity, Async<'relatedId list>>) =
    this.GetLinkageIfNotIncludedTask(Task.liftAsyncFunc2 get)

  member this.GetLinkageIfNotIncludedAsync (get: Func<'entity, Async<'relatedId list>>) =
    this.GetLinkageIfNotIncludedTask(Task.liftAsyncFunc get)

  member this.GetLinkageIfNotIncludedSkip (get: Func<'ctx, 'entity, Skippable<'relatedId list>>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Task.result)

  member this.GetLinkageIfNotIncluded (get: Func<'ctx, 'entity, 'relatedId list>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun ctx r -> get.Invoke(ctx, r) |> Include |> Task.result)

  member this.GetLinkageIfNotIncluded (get: Func<'entity, 'relatedId list>) =
    this.GetLinkageIfNotIncludedTaskSkip(fun _ r -> get.Invoke r |> Include |> Task.result)

  /// Omits the entire relationship (links, data, and meta) in the returned resource if the predicate returns true. If
  /// using one of the Get...Skip methods, this method should also be called. Otherwise, relationship links will always
  /// be present in the response, but GET operations against them will return an error if the getter returns Skip.
  member this.SkipRelationshipIf(predicate: 'ctx -> 'entity -> bool) =
    { this with skipRelationship = predicate }

  /// Omits the entire relationship (links, data, and meta) in the returned resource if the predicate returns true. If
  /// using one of the Get...Skip methods, this method should also be called. Otherwise, relationship links will always
  /// be present in the response, but GET operations against them will return an error if the getter returns Skip.
  member this.SkipRelationshipIf(predicate: 'entity -> bool) =
    { this with skipRelationship = fun _ e -> predicate e }

  member private this.SetAllTaskRes (setAll: Func<'ctx, 'setCtx, Pointer, ('relatedId * ResourceIdentifier) list, 'entity, Task<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    { this with setAll = Some (fun ctx setCtx ptr relIds e -> setAll.Invoke(ctx, setCtx, ptr, relIds, e)) }

  member this.SetAllTaskRes (setAll: Func<'setCtx, 'relatedId list, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(fun _ ctx pointer relIds e -> setAll.Invoke(ctx, (relIds |> List.map fst), e) |> TaskResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.SetAllTaskRes (setAll: Func<'relatedId list, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(fun _ ids e -> setAll.Invoke(ids, e))

  member this.SetAllTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'setCtx, 'lookupType list, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(this.toIdSetter getRelated (fun ctx rels e -> setAll.Invoke(ctx, rels, e)))

  member this.SetAllTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'lookupType list, 'entity, Task<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(this.toIdSetter getRelated (fun _ ids e -> setAll.Invoke(ids, e)))

  member this.SetAllAsyncRes (setAll: Func<'setCtx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(Task.liftAsyncFunc3 setAll)

  member this.SetAllAsyncRes (setAll: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(Task.liftAsyncFunc2 setAll)

  member this.SetAllAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'setCtx, 'lookupType list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(getRelated, Task.liftAsyncFunc3 setAll)

  member this.SetAllAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'lookupType list, 'entity, Async<Result<'entity, Error list>>>) =
    this.SetAllTaskRes(getRelated, Task.liftAsyncFunc2 setAll)

  member this.SetAllTask (setAll: Func<'setCtx, 'relatedId list, 'entity, Task<'entity>>) =
    this.SetAllTaskRes(fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.SetAllTask (setAll: Func<'relatedId list, 'entity, Task<'entity>>) =
    this.SetAllTaskRes(fun _ related entity -> setAll.Invoke(related, entity) |> Task.map Ok)

  member this.SetAllTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'setCtx, 'lookupType list, 'entity, Task<'entity>>) =
    this.SetAllTaskRes(getRelated, fun ctx related entity -> setAll.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.SetAllTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'lookupType list, 'entity, Task<'entity>>) =
    this.SetAllTaskRes(getRelated, fun _ related entity -> setAll.Invoke(related, entity) |> Task.map Ok)

  member this.SetAllAsync (setAll: Func<'setCtx, 'relatedId list, 'entity, Async<'entity>>) =
    this.SetAllTask(Task.liftAsyncFunc3 setAll)

  member this.SetAllAsync (setAll: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.SetAllTask(Task.liftAsyncFunc2 setAll)

  member this.SetAllAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'setCtx, 'lookupType list, 'entity, Async<'entity>>) =
    this.SetAllTask(getRelated, Task.liftAsyncFunc3 setAll)

  member this.SetAllAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'lookupType list, 'entity, Async<'entity>>) =
    this.SetAllTask(getRelated, Task.liftAsyncFunc2 setAll)

  member this.SetAllRes (setAll: Func<'setCtx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.SetAllTaskRes(Task.liftFunc3 setAll)

  member this.SetAllRes (setAll: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.SetAllTaskRes(Task.liftFunc2 setAll)

  member this.SetAllRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'setCtx, 'lookupType list, 'entity, Result<'entity, Error list>>) =
    this.SetAllTaskRes(getRelated, Task.liftFunc3 setAll)

  member this.SetAllRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'lookupType list, 'entity, Result<'entity, Error list>>) =
    this.SetAllTaskRes(getRelated, Task.liftFunc2 setAll)

  member this.SetAll (setAll: Func<'setCtx, 'relatedId list, 'entity, 'entity>) =
    this.SetAllTaskRes(TaskResult.liftFunc3 setAll)

  member this.SetAll (setAll: Func<'relatedId list, 'entity, 'entity>) =
    this.SetAllTaskRes(TaskResult.liftFunc2 setAll)

  member this.SetAll (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'setCtx, 'lookupType list, 'entity, 'entity>) =
    this.SetAllTaskRes(getRelated, TaskResult.liftFunc3 setAll)

  member this.SetAll (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, setAll: Func<'lookupType list, 'entity, 'entity>) =
    this.SetAllTaskRes(getRelated, TaskResult.liftFunc2 setAll)

  member private this.AddTaskRes (add: Func<'ctx, 'setCtx, Pointer, ('relatedId * ResourceIdentifier) list, 'entity, Task<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwith "Can only add setter if the polymorphic resource definition contains ID parsers."
    if this.get.IsNone then
      failwith "Can only add POST to relationship if it contains a getter."
    { this with add = Some (fun ctx setCtx ptr relIds e -> add.Invoke(ctx, setCtx, ptr, relIds, e)) }

  member this.AddTaskRes (add: Func<'setCtx, 'relatedId list, 'entity, Task<Result<'entity, Error list>>>) =
    this.AddTaskRes(fun _ ctx pointer relIds e -> add.Invoke(ctx, (relIds |> List.map fst), e) |> TaskResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.AddTaskRes (add: Func<'relatedId list, 'entity, Task<Result<'entity, Error list>>>) =
    this.AddTaskRes(fun _ ids e -> add.Invoke(ids, e))

  member this.AddTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'setCtx, 'lookupType list, 'entity, Task<Result<'entity, Error list>>>) =
    this.AddTaskRes(this.toIdSetter getRelated (fun ctx rels e -> add.Invoke(ctx, rels, e)))

  member this.AddTaskRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'lookupType list, 'entity, Task<Result<'entity, Error list>>>) =
    this.AddTaskRes(this.toIdSetter getRelated (fun _ ids e -> add.Invoke(ids, e)))

  member this.AddAsyncRes (add: Func<'setCtx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddTaskRes(Task.liftAsyncFunc3 add)

  member this.AddAsyncRes (add: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddTaskRes(Task.liftAsyncFunc2 add)

  member this.AddAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'setCtx, 'lookupType list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddTaskRes(getRelated, Task.liftAsyncFunc3 add)

  member this.AddAsyncRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'lookupType list, 'entity, Async<Result<'entity, Error list>>>) =
    this.AddTaskRes(getRelated, Task.liftAsyncFunc2 add)

  member this.AddTask (add: Func<'setCtx, 'relatedId list, 'entity, Task<'entity>>) =
    this.AddTaskRes(fun ctx related entity -> add.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.AddTask (add: Func<'relatedId list, 'entity, Task<'entity>>) =
    this.AddTaskRes(fun _ related entity -> add.Invoke(related, entity) |> Task.map Ok)

  member this.AddTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'setCtx, 'lookupType list, 'entity, Task<'entity>>) =
    this.AddTaskRes(getRelated, fun ctx related entity -> add.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.AddTask (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'lookupType list, 'entity, Task<'entity>>) =
    this.AddTaskRes(getRelated, fun _ related entity -> add.Invoke(related, entity) |> Task.map Ok)

  member this.AddAsync (add: Func<'setCtx, 'relatedId list, 'entity, Async<'entity>>) =
    this.AddTask(Task.liftAsyncFunc3 add)

  member this.AddAsync (add: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.AddTask(Task.liftAsyncFunc2 add)

  member this.AddAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'setCtx, 'lookupType list, 'entity, Async<'entity>>) =
    this.AddTask(getRelated, Task.liftAsyncFunc3 add)

  member this.AddAsync (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'lookupType list, 'entity, Async<'entity>>) =
    this.AddTask(getRelated, Task.liftAsyncFunc2 add)

  member this.AddRes (add: Func<'setCtx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.AddTaskRes(Task.liftFunc3 add)

  member this.AddRes (add: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.AddTaskRes(Task.liftFunc2 add)

  member this.AddRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'setCtx, 'lookupType list, 'entity, Result<'entity, Error list>>) =
    this.AddTaskRes(getRelated, Task.liftFunc3 add)

  member this.AddRes (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'lookupType list, 'entity, Result<'entity, Error list>>) =
    this.AddTaskRes(getRelated, Task.liftFunc2 add)

  member this.Add (add: Func<'setCtx, 'relatedId list, 'entity, 'entity>) =
    this.AddTaskRes(TaskResult.liftFunc3 add)

  member this.Add (add: Func<'relatedId list, 'entity, 'entity>) =
    this.AddTaskRes(TaskResult.liftFunc2 add)

  member this.Add (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'setCtx, 'lookupType list, 'entity, 'entity>) =
    this.AddTaskRes(getRelated, TaskResult.liftFunc3 add)

  member this.Add (getRelated: ResourceLookup<'ctx, 'lookupType, 'relatedId>, add: Func<'lookupType list, 'entity, 'entity>) =
    this.AddTaskRes(getRelated, TaskResult.liftFunc2 add)

  member private this.RemoveTaskRes (remove: Func<'ctx, 'setCtx, Pointer, ('relatedId * ResourceIdentifier) list, 'entity, Task<Result<'entity, Error list>>>) =
    if this.idParsers.IsNone then
      failwithf "Can only add setter if the polymorphic resource definition contains ID parsers."
    if this.get.IsNone then
      failwith "Can only add DELETE to relationship if it contains a getter."
    { this with remove = Some (fun ctx setCtx ptr relIds e -> remove.Invoke(ctx, setCtx, ptr, relIds, e)) }

  member this.RemoveTaskRes (remove: Func<'setCtx, 'relatedId list, 'entity, Task<Result<'entity, Error list>>>) =
    this.RemoveTaskRes(fun _ ctx pointer relIds e -> remove.Invoke(ctx, (relIds |> List.map fst), e) |> TaskResult.mapError (List.map (Error.setSourcePointer pointer)))

  member this.RemoveTaskRes (remove: Func<'relatedId list, 'entity, Task<Result<'entity, Error list>>>) =
    this.RemoveTaskRes(fun _ ids e -> remove.Invoke(ids, e))

  member this.RemoveAsyncRes (remove: Func<'setCtx, 'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveTaskRes(Task.liftAsyncFunc3 remove)

  member this.RemoveAsyncRes (remove: Func<'relatedId list, 'entity, Async<Result<'entity, Error list>>>) =
    this.RemoveTaskRes(Task.liftAsyncFunc2 remove)

  member this.RemoveTask (remove: Func<'setCtx, 'relatedId list, 'entity, Task<'entity>>) =
    this.RemoveTaskRes(fun ctx related entity -> remove.Invoke(ctx, related, entity) |> Task.map Ok)

  member this.RemoveTask (remove: Func<'relatedId list, 'entity, Task<'entity>>) =
    this.RemoveTaskRes(fun _ related entity -> remove.Invoke(related, entity) |> Task.map Ok)

  member this.RemoveAsync (remove: Func<'setCtx, 'relatedId list, 'entity, Async<'entity>>) =
    this.RemoveTask(Task.liftAsyncFunc3 remove)

  member this.RemoveAsync (remove: Func<'relatedId list, 'entity, Async<'entity>>) =
    this.RemoveTask(Task.liftAsyncFunc2 remove)

  member this.RemoveRes (remove: Func<'setCtx, 'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.RemoveTaskRes(Task.liftFunc3 remove)

  member this.RemoveRes (remove: Func<'relatedId list, 'entity, Result<'entity, Error list>>) =
    this.RemoveTaskRes(Task.liftFunc2 remove)

  member this.Remove (remove: Func<'setCtx, 'relatedId list, 'entity, 'entity>) =
    this.RemoveTaskRes(TaskResult.liftFunc3 remove)

  member this.Remove (remove: Func<'relatedId list, 'entity, 'entity>) =
    this.RemoveTaskRes(TaskResult.liftFunc2 remove)

  member this.AddConstraintsTask(getConstraints: 'ctx -> 'entity -> Task<(string * obj) list>) =
    { this with
        hasConstraints = true
        getConstraints =
          fun ctx e ->
            task {
              let! currentCs = this.getConstraints ctx e
              let! newCs = getConstraints ctx e
              return currentCs @ newCs
            }
    }

  member this.AddConstraintsAsync(getConstraints: 'ctx -> 'entity -> Async<(string * obj) list>) =
    this.AddConstraintsTask(Task.liftAsync2 getConstraints)

  member this.AddConstraints(getConstraints: 'ctx -> 'entity -> (string * obj) list) =
    this.AddConstraintsTask(Task.lift2 getConstraints)

  member this.AddConstraint (name: string, getValue: 'ctx -> 'entity -> 'a) =
    this.AddConstraintsTask(fun ctx e -> [name, box (getValue ctx e)] |> Task.result)

  member this.AddConstraint (name: string, getValue: 'entity -> 'a) =
    this.AddConstraint(name, fun _ e -> getValue e)

  member this.ModifyGetRelatedResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyGetRelatedResponse = getHandler }

  member this.ModifyGetRelatedResponse(f: 'ctx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyGetRelatedResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetRelatedResponse(handler: HttpHandler) =
    this.ModifyGetRelatedResponse(fun _ _ _ -> handler)

  member this.ModifyGetSelfResponse(getHandler: 'ctx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyGetSelfResponse = getHandler }

  member this.ModifyGetSelfResponse(f: 'ctx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyGetSelfResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyGetSelfResponse(handler: HttpHandler) =
    this.ModifyGetSelfResponse(fun _ _ _ -> handler)

  member this.ModifyPostSelfOkResponse(getHandler: 'setCtx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyPostSelfOkResponse = getHandler }

  member this.ModifyPostSelfOkResponse(f: 'setCtx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyPostSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPostSelfOkResponse(handler: HttpHandler) =
    this.ModifyPostSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPostSelfAcceptedResponse(getHandler: 'setCtx -> 'entity -> HttpHandler) =
    { this with modifyPostSelfAcceptedResponse = getHandler }

  member this.ModifyPostSelfAcceptedResponse(f: 'setCtx -> 'entity -> HttpContext -> unit) =
    this.ModifyPostSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPostSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPostSelfAcceptedResponse(fun _ _ -> handler)

  member this.ModifyPatchSelfOkResponse(getHandler: 'setCtx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyPatchSelfOkResponse = getHandler }

  member this.ModifyPatchSelfOkResponse(f: 'setCtx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyPatchSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyPatchSelfOkResponse(handler: HttpHandler) =
    this.ModifyPatchSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyPatchSelfAcceptedResponse(getHandler: 'setCtx -> 'entity -> HttpHandler) =
    { this with modifyPatchSelfAcceptedResponse = getHandler }

  member this.ModifyPatchSelfAcceptedResponse(f: 'setCtx -> 'entity -> HttpContext -> unit) =
    this.ModifyPatchSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyPatchSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyPatchSelfAcceptedResponse(fun _ _ -> handler)

  member this.ModifyDeleteSelfOkResponse(getHandler: 'setCtx -> 'entity -> 'relatedEntity list -> HttpHandler) =
    { this with modifyDeleteSelfOkResponse = getHandler }

  member this.ModifyDeleteSelfOkResponse(f: 'setCtx -> 'entity -> 'relatedEntity list -> HttpContext -> unit) =
    this.ModifyDeleteSelfOkResponse(fun ctx e related -> (fun next httpCtx -> f ctx e related httpCtx; next httpCtx))

  member this.ModifyDeleteSelfOkResponse(handler: HttpHandler) =
    this.ModifyDeleteSelfOkResponse(fun _ _ _ -> handler)

  member this.ModifyDeleteSelfAcceptedResponse(getHandler: 'setCtx -> 'entity -> HttpHandler) =
    { this with modifyDeleteSelfAcceptedResponse = getHandler }

  member this.ModifyDeleteSelfAcceptedResponse(f: 'setCtx -> 'entity -> HttpContext -> unit) =
    this.ModifyDeleteSelfAcceptedResponse(fun ctx e -> (fun next httpCtx -> f ctx e httpCtx; next httpCtx))

  member this.ModifyDeleteSelfAcceptedResponse(handler: HttpHandler) =
    this.ModifyDeleteSelfAcceptedResponse(fun _ _ -> handler)

  member this.BeforeModifySelfTaskRes(f: Func<'setCtx, 'entity, Task<Result<'entity, Error list>>>) =
    { this with beforeModifySelf = (fun ctx e -> f.Invoke(ctx, e)) }

  member this.BeforeModifySelfTaskRes(f: Func<'setCtx, 'entity, Task<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> TaskResult.map (fun () -> e))

  member this.BeforeModifySelfTaskRes(f: Func<'entity, Task<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfTaskRes(f: Func<'entity, Task<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(fun _ e -> f.Invoke e)

  member this.BeforeModifySelfAsyncRes(f: Func<'setCtx, 'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'setCtx, 'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<'entity, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc f)

  member this.BeforeModifySelfAsyncRes(f: Func<'entity, Async<Result<unit, Error list>>>) =
    this.BeforeModifySelfTaskRes(Task.liftAsyncFunc f)

  member this.BeforeModifySelfTask(f: Func<'setCtx, 'entity, Task<'entity>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'setCtx, 'entity, Task<unit>>) =
    this.BeforeModifySelfTaskRes(fun ctx e -> f.Invoke(ctx, e) |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'entity, Task<'entity>>) =
    this.BeforeModifySelfTaskRes(fun e -> f.Invoke e |> Task.map Ok)

  member this.BeforeModifySelfTask(f: Func<'entity, Task<unit>>) =
    this.BeforeModifySelfTaskRes(fun e -> f.Invoke e |> Task.map Ok)

  member this.BeforeModifySelfAsync(f: Func<'setCtx, 'entity, Async<'entity>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'setCtx, 'entity, Async<unit>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc2 f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<'entity>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc f)

  member this.BeforeModifySelfAsync(f: Func<'entity, Async<unit>>) =
    this.BeforeModifySelfTask(Task.liftAsyncFunc f)

  member this.BeforeModifySelfRes(f: Func<'setCtx, 'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'setCtx, 'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc2 f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<'entity, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc f)

  member this.BeforeModifySelfRes(f: Func<'entity, Result<unit, Error list>>) =
    this.BeforeModifySelfTaskRes(Task.liftFunc f)

  member this.BeforeModifySelf(f: Func<'setCtx, 'entity, 'entity>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'setCtx, 'entity, unit>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc2 f)

  member this.BeforeModifySelf(f: Func<'entity, 'entity>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc f)

  member this.BeforeModifySelf(f: Func<'entity, unit>) =
    this.BeforeModifySelfTaskRes(TaskResult.liftFunc f)

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some f }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> 'entity -> Task<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun ctx eOld eNew -> f ctx eOld eNew |> TaskResult.map (fun () -> eNew)) }

  member this.AfterModifySelfTaskRes(f: 'entity -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun _ eOld eNew -> f eOld eNew) }

  member this.AfterModifySelfTaskRes(f: 'entity -> 'entity -> Task<Result<unit, Error list>>) =
    { this with afterModifySelf = Some (fun _ eOld eNew -> f eOld eNew |> TaskResult.map (fun () -> eNew)) }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> Task<Result<'entity, Error list>>) =
    { this with afterModifySelf = Some (fun ctx _ e -> f ctx e) }

  member this.AfterModifySelfTaskRes(f: 'setCtx -> 'entity -> Task<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> TaskResult.map (fun () -> e))

  member this.AfterModifySelfTaskRes(f: 'entity -> Task<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ _ e -> f e)

  member this.AfterModifySelfTaskRes(f: 'entity -> Task<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(fun _ _ e -> f e |> TaskResult.map (fun () -> e))

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync3 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'setCtx -> 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync2 f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<'entity, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync f)

  member this.AfterModifySelfAsyncRes(f: 'entity -> Async<Result<unit, Error list>>) =
    this.AfterModifySelfTaskRes(Task.liftAsync f)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun ctx eOld eNew -> f ctx eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun ctx eOld eNew -> f ctx eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun _ eOld eNew -> f eOld eNew |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'setCtx -> 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun ctx e -> f ctx e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> Task<'entity>) =
    this.AfterModifySelfTaskRes(fun e -> f e |> Task.map Ok)

  member this.AfterModifySelfTask(f: 'entity -> Task<unit>) =
    this.AfterModifySelfTaskRes(fun e -> f e |> Task.map Ok)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync3 f)

  member this.AfterModifySelfAsync(f: 'entity -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'setCtx -> 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync2 f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<'entity>) =
    this.AfterModifySelfTask(Task.liftAsync f)

  member this.AfterModifySelfAsync(f: 'entity -> Async<unit>) =
    this.AfterModifySelfTask(Task.liftAsync f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift3 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift3 f)

  member this.AfterModifySelfRes(f: 'entity -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'setCtx -> 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift2 f)

  member this.AfterModifySelfRes(f: 'entity -> Result<'entity, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift f)

  member this.AfterModifySelfRes(f: 'entity -> Result<unit, Error list>) =
    this.AfterModifySelfTaskRes(Task.lift f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift3 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift3 f)

  member this.AfterModifySelf(f: 'entity -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'setCtx -> 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift2 f)

  member this.AfterModifySelf(f: 'entity -> 'entity) =
    this.AfterModifySelfTaskRes(TaskResult.lift f)

  member this.AfterModifySelf(f: 'entity -> unit) =
    this.AfterModifySelfTaskRes(TaskResult.lift f)

  member this.ModifySelfReturn202Accepted () =
    { this with modifySelfReturn202Accepted = true }



[<AutoOpen>]
module ToManyRelationshipExtensions =

  type ToManyRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> with

    member this.AddConstraint (name: string, value: 'a) =
      this.AddConstraint(name, fun _ -> value)



type PolymorphicRelationshipHelper<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> = internal {
  mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>
  resolveEntity: ('relatedEntity -> PolymorphicBuilder<'ctx>) option
  resolveId: ('relatedId -> ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>) option
  idParsers: Map<ResourceTypeName, 'ctx -> ResourceId -> Task<Result<'relatedId, (ParsedValueInfo -> Error) list>>> option
} with

  static member internal Create (mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>) : PolymorphicRelationshipHelper<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId> =
    { mapSetCtx = mapSetCtx; resolveEntity = None; idParsers = None; resolveId = None }

  member this.MapSetContextTaskRes (mapSetCtx: 'ctx -> 'entity -> Task<Result<'mappedSetCtx, Error list>>) =
    {
      mapSetCtx = mapSetCtx
      resolveEntity = this.resolveEntity
      resolveId = this.resolveId
      idParsers = this.idParsers
    }

  member this.MapSetContextAsyncRes (mapSetCtx: 'ctx -> 'entity -> Async<Result<'mappedSetCtx, Error list>>) =
    this.MapSetContextTaskRes (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextTask (mapSetCtx: 'ctx -> 'entity -> Task<'mappedSetCtx>) =
    this.MapSetContextTaskRes (fun ctx e -> mapSetCtx ctx e |> Task.map Ok)

  member this.MapSetContextAsync (mapSetCtx: 'ctx -> 'entity -> Async<'mappedSetCtx>) =
    this.MapSetContextTask (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextRes (mapSetCtx: 'ctx -> 'entity -> Result<'mappedSetCtx, Error list>) =
    this.MapSetContextTaskRes (Task.lift2 mapSetCtx)

  member this.MapSetContext (mapSetCtx: 'ctx -> 'entity -> 'mappedSetCtx) =
    this.MapSetContextTaskRes (TaskResult.lift2 mapSetCtx)

  member this.AddIdParser(resDef: ResourceDefinition<'ctx, 'e, 'relatedId>) =
    { this with
        idParsers =
          this.idParsers
          |> Option.defaultValue Map.empty
          |> Map.add resDef.name resDef.id.toDomain
          |> Some
    }

  member this.AddIdParser(resDef: ResourceDefinition<'ctx, 'e, 'id>, mapId: 'id -> 'relatedId) =
    { this with
        idParsers =
          this.idParsers
          |> Option.defaultValue Map.empty
          |> Map.add resDef.name (fun ctx resId ->
              resDef.id.toDomain ctx resId |> TaskResult.map mapId)
          |> Some
    }

  member this.ResolveEntity(getPolyBuilder: 'relatedEntity -> PolymorphicBuilder<'ctx>) =
    { this with resolveEntity = Some getPolyBuilder }

  member this.ResolveId(getResDef: 'relatedId -> ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>) =
    { this with resolveId = Some getResDef }

  member this.ToOne([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    ToOneRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId>.Create(name, this.mapSetCtx, this.resolveEntity, this.resolveId, this.idParsers)

  member this.ToOneNullable([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    ToOneNullableRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId>.Create(name, this.mapSetCtx, this.resolveEntity, this.resolveId, this.idParsers)

  member this.ToMany([<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    ToManyRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId>.Create(name, this.mapSetCtx, this.resolveEntity, this.resolveId, this.idParsers)



type RelationshipHelper<'ctx, 'setCtx, 'entity> internal (mapSetCtx: 'ctx -> 'entity -> Task<Result<'setCtx, Error list>>) =

  member _.Polymorphic () =
    PolymorphicRelationshipHelper<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId>.Create(mapSetCtx)

  member _.MapSetContextTaskRes (mapSetCtx: 'ctx -> 'entity -> Task<Result<'mappedSetCtx, Error list>>) =
    RelationshipHelper<'ctx, 'mappedSetCtx, 'entity>(mapSetCtx)

  member this.MapSetContextAsyncRes (mapSetCtx: 'ctx -> 'entity -> Async<Result<'mappedSetCtx, Error list>>) =
    this.MapSetContextTaskRes (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextTask (mapSetCtx: 'ctx -> 'entity -> Task<'mappedSetCtx>) =
    this.MapSetContextTaskRes (fun ctx e -> mapSetCtx ctx e |> Task.map Ok)

  member this.MapSetContextAsync (mapSetCtx: 'ctx -> 'entity -> Async<'mappedSetCtx>) =
    this.MapSetContextTask (Task.liftAsync2 mapSetCtx)

  member this.MapSetContextRes (mapSetCtx: 'ctx -> 'entity -> Result<'mappedSetCtx, Error list>) =
    this.MapSetContextTaskRes (Task.lift2 mapSetCtx)

  member this.MapSetContext (mapSetCtx: 'ctx -> 'entity -> 'mappedSetCtx) =
    this.MapSetContextTaskRes (TaskResult.lift2 mapSetCtx)

  member _.ToOne(resourceDef: ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let idParsers = Map.empty |> Map.add resourceDef.name resourceDef.id.toDomain
    let resolveEntity = resourceDef.PolymorphicFor
    let resolveId = fun _ -> resourceDef
    ToOneRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId>.Create(name, mapSetCtx, Some resolveEntity, Some resolveId, Some idParsers)

  member _.ToOneNullable(resourceDef: ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let idParsers = Map.empty |> Map.add resourceDef.name resourceDef.id.toDomain
    let resolveEntity = resourceDef.PolymorphicFor
    let resolveId = fun _ -> resourceDef
    ToOneNullableRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId>.Create(name, mapSetCtx, Some resolveEntity, Some resolveId, Some idParsers)

  member _.ToMany(resourceDef: ResourceDefinition<'ctx, 'relatedEntity, 'relatedId>, [<CallerMemberName; Optional; DefaultParameterValue("")>] name: string) =
    let idParsers = Map.empty |> Map.add resourceDef.name resourceDef.id.toDomain
    let resolveEntity = resourceDef.PolymorphicFor
    let resolveId = fun _ -> resourceDef
    ToManyRelationship<'ctx, 'setCtx, 'entity, 'relatedEntity, 'relatedId>.Create(name, mapSetCtx, Some resolveEntity, Some resolveId, Some idParsers)
