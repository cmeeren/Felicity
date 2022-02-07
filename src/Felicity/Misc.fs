namespace Felicity

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open Hopac
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
  abstract Get: 'ctx * Request * (ResourceTypeName * ResourceId) option -> Job<Result<'a, Error list>>

type OptionalRequestGetter<'ctx, 'a> =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract Get: 'ctx * Request * (ResourceTypeName * ResourceId) option -> Job<Result<'a option, Error list>>

type ProhibitedRequestGetter =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract GetErrors: Request * (ResourceTypeName * ResourceId) option -> Error list


type internal Field<'ctx> =
  abstract Name: string

type internal BoxedPatcher<'ctx> = 'ctx -> Request -> Set<ConsumedFieldName> -> BoxedEntity -> Job<Result<BoxedEntity, Error list>>



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
        | true, values when values.Count = 0 -> []
        | true, values -> [queryDoesNotAcceptValue paramName values[0]]
    )
