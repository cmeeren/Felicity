namespace Felicity

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Giraffe
open Errors


type Request = internal {
  Document: Lazy<Result<ResourceDocument option, Error list>>
  IdentifierDocument: Lazy<Result<ResourceIdentifierDocument option, Error list>>
  IdentifierCollectionDocument: Lazy<Result<ResourceIdentifierCollectionDocument option, Error list>>
  Headers: Map<string, string>
  Query: Map<string, string>
  Fieldsets: Map<ResourceTypeName, Set<FieldName>>
  Includes: RelationshipName list list
}


module internal Request =

  let private getResourceAndDeepestPointer (includedTypeAndId: (ResourceTypeName * ResourceId) option) req =
    match includedTypeAndId with
    | None -> 
        match req.Document.Value with
        | Error errs -> Error errs
        | Ok None -> Ok (None, "")
        | Ok (Some { data = res }) -> Ok (res, "/data")
    | Some (resType, resId) ->
        match req.Document.Value with
        | Error errs -> Error errs
        | Ok None -> failwith "Framework bug: Attempted to find included resource with no request document"
        | Ok (Some { included = Skip }) -> Error [reqParserMissingIncludedResource resType resId ""]
        | Ok (Some { included = Include (TryFindIndexed (Resource'.matches resType resId) (i, res)) }) ->
            Ok (Some res, "/included/" + string i)
        | Ok (Some { included = Include _ }) -> Error [reqParserMissingIncludedResource resType resId "/included"]

  let getIdAndPointer includedTypeAndId req : Result<(ResourceId * Pointer) option, _> =
    getResourceAndDeepestPointer includedTypeAndId req
    |> Result.map (fun (res, ptr) -> res |> Option.bind (fun res ->
        res.id |> Skippable.toOption |> Option.map (fun rsId -> rsId, ptr + "/id")
    ))

  let getAttrAndPointer includedTypeAndId req : Result<(_ * Pointer) option, _> =
    getResourceAndDeepestPointer includedTypeAndId req
    |> Result.map (fun (res, ptr) -> res |> Option.bind (fun res ->
        res.attributes |> Skippable.toOption |> Option.map (fun attrs -> attrs, ptr + "/attributes")
    ))

  let getRelsAndPointer includedTypeAndId req : Result<(_ * Pointer) option, _> =
    getResourceAndDeepestPointer includedTypeAndId req
    |> Result.map (fun (res, ptr) -> res |> Option.bind (fun res ->
      res.relationships |> Skippable.toOption |> Option.map (fun rels -> rels, ptr + "/relationships")
    ))

  let pointerForMissingId includedTypeAndId req =
    match getResourceAndDeepestPointer includedTypeAndId req with
    | Error _ -> ""  // Won't be used
    | Ok (None, ptr) -> ptr
    | Ok (Some _, ptr) -> ptr + "/id"

  let pointerForMissingAttr includedTypeAndId req =
    match getResourceAndDeepestPointer includedTypeAndId req with
    | Error _ -> ""  // Won't be used
    | Ok (None, ptr) -> ptr
    | Ok (Some res, ptr) ->
        match res.attributes with
        | Skip -> ptr
        | Include _ -> ptr + "/attributes"

  let pointerForMissingRel includedTypeAndId req =
    match getResourceAndDeepestPointer includedTypeAndId req with
    | Error _ -> ""  // Won't be used
    | Ok (None, ptr) -> ptr
    | Ok (Some res, ptr) ->
        match res.relationships with
        | Skip -> ptr
        | Include _ -> ptr + "/relationships"


type RequestGetter<'ctx, 'a> =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract Get: 'ctx * Request * (ResourceTypeName * ResourceId) option -> Task<Result<'a, Error list>>

type OptionalRequestGetter<'ctx, 'a> =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract Get: 'ctx * Request * (ResourceTypeName * ResourceId) option -> Task<Result<'a option, Error list>>

type ProhibitedRequestGetter =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract GetErrors: Request * (ResourceTypeName * ResourceId) option -> Error list


type internal Field<'ctx> =
  abstract Name: string

type internal BoxedPatcher<'ctx> = 'ctx -> Request -> Set<ConsumedFieldName> -> BoxedEntity -> Task<Result<BoxedEntity * Set<FieldName>, Error list>>



type internal MetaGetter<'ctx> (getMeta: 'ctx -> IDictionary<string, obj>) =
  member _.GetMeta ctx = getMeta ctx



type internal LinkConfig<'ctx> (skipStandardLinksQueryParamNames: string [], skipCustomLinksQueryParamNames: string []) =

  member _.ShouldUseStandardLinks(ctx: HttpContext) =
    skipStandardLinksQueryParamNames |> Array.exists ctx.Request.Query.ContainsKey |> not

  member _.ShouldUseCustomLinks(ctx: HttpContext) =
    skipCustomLinksQueryParamNames |> Array.exists ctx.Request.Query.ContainsKey |> not

  member _.GetIllegalValueErrors(ctx: HttpContext) =
    Seq.concat [skipStandardLinksQueryParamNames; skipCustomLinksQueryParamNames]
    |> Seq.collect (fun paramName ->
        match ctx.Request.Query.TryGetValue paramName with
        | false, _ -> []
        | true, values ->
            let nonEmptyValues = values |> Seq.filter (not << String.IsNullOrEmpty)
            match Seq.tryHead nonEmptyValues with
            | None -> []
            | Some value -> [queryDoesNotAcceptValue paramName value]
    )


/// Indicates why a field is used (or not) in a response.
[<RequireQualifiedAccess>]
type FieldUsage =
  /// The field is considered explicitly used for one or more of the following reasons:
  ///
  ///   - The field is specified in a sparse fieldsets (`'fields[...]'`) parameter.
  ///
  ///   - The field is used in a request body (e.g. POST or PATCH).
  ///
  ///   - The field is a relationship and is included using the `include` parameter.
  ///
  ///   - The field is a relationship and the request targets its `self` or `related` link.
  | Explicit

  /// The field is considered implicitly used because none of the requirements for explicit usage were satisfied and
  /// there was no sparse fieldset parameter for the resource.
  | Implicit

  /// The field is considered excluded because none of the requirements for explicit usage were satisfied and there was
  /// a sparse fieldset parameter for the resource.
  | Excluded


type FieldUseInfo = {
  TypeName: string
  FieldName: string
  Usage: FieldUsage
}


type internal FieldTracker<'ctx>
  (
    trackFields,
    resourceModuleMap: Map<ResourceTypeName, Type>,
    httpContextAccessor: IHttpContextAccessor,
    report: ('ctx -> FieldUseInfo list -> Task<HttpHandler>) option
  ) =
  member _.TrackFields
    (
      primaryResourceTypes: ResourceTypeName list,
      ctx: 'ctx,
      req: Request,
      ?relationshipOperationResourceTypeNameAndFieldName: ResourceTypeName * FieldName,
      ?relNameIfRelationshipSelf: FieldName,
      ?consumedFieldNamesWithType: ResourceTypeName * Set<ConsumedFieldName>
    ) : Task<HttpHandler> =
    report
    |> Option.traverseTask (
        trackFields
          resourceModuleMap
          primaryResourceTypes
          ctx
          req
          httpContextAccessor.HttpContext
          relationshipOperationResourceTypeNameAndFieldName
          relNameIfRelationshipSelf
          consumedFieldNamesWithType
    )
    |> Task.map (Option.defaultValue (fun next ctx -> next ctx))
