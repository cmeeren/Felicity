namespace Felicity

open System
open System.Collections.Generic
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Giraffe
open Errors


type Request = internal {
    Document: Lazy<Task<Result<ResourceDocument option, Error list>>>
    IdentifierDocument: Lazy<Task<Result<ResourceIdentifierDocument option, Error list>>>
    IdentifierCollectionDocument: Lazy<Task<Result<ResourceIdentifierCollectionDocument option, Error list>>>
    Headers: IHeaderDictionary
    Query: Map<string, string>
    Fieldsets: Map<ResourceTypeName, Set<FieldName>>
    Includes: RelationshipName list list
}


module internal Request =

    let private getResourceAndDeepestPointer (includedTypeAndId: (ResourceTypeName * ResourceId) option) req =
        task {
            match includedTypeAndId with
            | None ->
                match! req.Document.Value with
                | Error errs -> return Error errs
                | Ok None -> return Ok(None, "")
                | Ok(Some { data = res }) -> return Ok(res, "/data")
            | Some(resType, resId) ->
                match! req.Document.Value with
                | Error errs -> return Error errs
                | Ok None ->
                    return failwith "Framework bug: Attempted to find included resource with no request document"
                | Ok(Some { included = Skip }) -> return Error [ reqParserMissingIncludedResource resType resId "" ]
                | Ok(Some {
                              included = Include(TryFindIndexed (Resource'.matches resType resId) (i, res))
                          }) -> return Ok(Some res, "/included/" + string i)
                | Ok(Some { included = Include _ }) ->
                    return Error [ reqParserMissingIncludedResource resType resId "/included" ]
        }

    let getIdAndPointer includedTypeAndId req : Task<Result<(ResourceId * Pointer) option, _>> =
        getResourceAndDeepestPointer includedTypeAndId req
        |> TaskResult.map (fun (res, ptr) ->
            res
            |> Option.bind (fun res -> res.id |> Skippable.toOption |> Option.map (fun rsId -> rsId, ptr + "/id"))
        )

    let getAttrAndPointer includedTypeAndId req : Task<Result<(_ * Pointer) option, _>> =
        getResourceAndDeepestPointer includedTypeAndId req
        |> TaskResult.map (fun (res, ptr) ->
            res
            |> Option.bind (fun res ->
                res.attributes
                |> Skippable.toOption
                |> Option.map (fun attrs -> attrs, ptr + "/attributes")
            )
        )

    let getRelsAndPointer includedTypeAndId req : Task<Result<(_ * Pointer) option, _>> =
        getResourceAndDeepestPointer includedTypeAndId req
        |> TaskResult.map (fun (res, ptr) ->
            res
            |> Option.bind (fun res ->
                res.relationships
                |> Skippable.toOption
                |> Option.map (fun rels -> rels, ptr + "/relationships")
            )
        )

    let pointerForMissingId includedTypeAndId req =
        task {
            match! getResourceAndDeepestPointer includedTypeAndId req with
            | Error _ -> return "" // Won't be used
            | Ok(None, ptr) -> return ptr
            | Ok(Some _, ptr) -> return ptr + "/id"
        }

    let pointerForMissingAttr includedTypeAndId req =
        task {
            match! getResourceAndDeepestPointer includedTypeAndId req with
            | Error _ -> return "" // Won't be used
            | Ok(None, ptr) -> return ptr
            | Ok(Some res, ptr) ->
                match res.attributes with
                | Skip -> return ptr
                | Include _ -> return ptr + "/attributes"
        }

    let pointerForMissingRel includedTypeAndId req =
        task {
            match! getResourceAndDeepestPointer includedTypeAndId req with
            | Error _ -> return "" // Won't be used
            | Ok(None, ptr) -> return ptr
            | Ok(Some res, ptr) ->
                match res.relationships with
                | Skip -> return ptr
                | Include _ -> return ptr + "/relationships"
        }


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
    abstract GetErrors: Request * (ResourceTypeName * ResourceId) option -> Task<Error list>


type internal Field<'ctx> =
    abstract Name: string

type internal BoxedPatcher<'ctx> =
    'ctx -> Request -> Set<ConsumedFieldName> -> BoxedEntity -> Task<Result<BoxedEntity * Set<FieldName>, Error list>>



type internal MetaGetter<'ctx>(getMeta: 'ctx -> IDictionary<string, obj>) =
    member _.GetMeta ctx = getMeta ctx



type internal LinkConfig<'ctx>(skipStandardLinksQueryParamNames: string[], skipCustomLinksQueryParamNames: string[]) =

    member _.QueryParamNames =
        Seq.concat [ skipStandardLinksQueryParamNames; skipCustomLinksQueryParamNames ]

    member _.ShouldUseStandardLinks(ctx: HttpContext) =
        skipStandardLinksQueryParamNames
        |> Array.exists ctx.Request.Query.ContainsKey
        |> not

    member _.ShouldUseCustomLinks(ctx: HttpContext) =
        skipCustomLinksQueryParamNames
        |> Array.exists ctx.Request.Query.ContainsKey
        |> not

    member _.GetIllegalValueErrors(ctx: HttpContext) =
        Seq.concat [ skipStandardLinksQueryParamNames; skipCustomLinksQueryParamNames ]
        |> Seq.collect (fun paramName ->
            match ctx.Request.Query.TryGetValue paramName with
            | false, _ -> []
            | true, values ->
                let nonEmptyValues = values |> Seq.filter (not << String.IsNullOrEmpty)

                match Seq.tryHead nonEmptyValues with
                | None -> []
                | Some value -> [ queryDoesNotAcceptValue paramName value ]
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
        sp: IServiceProvider,
        report: (IServiceProvider -> 'ctx -> FieldUseInfo list -> Task<HttpHandler>) option
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
                sp
                relationshipOperationResourceTypeNameAndFieldName
                relNameIfRelationshipSelf
                consumedFieldNamesWithType
        )
        |> Task.map (Option.defaultValue (fun next ctx -> next ctx))


[<RequireQualifiedAccess>]
type internal UnknownFieldStrictMode<'ctx> =
    | Ignore
    | Warn of LogLevel
    | Error


[<RequireQualifiedAccess>]
type internal UnknownQueryParamStrictMode<'ctx> =
    | Ignore
    | Warn of LogLevel
    | Error
