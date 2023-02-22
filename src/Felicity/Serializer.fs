namespace Felicity

open System
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Json
open InternalDeserializationModelDoNotUse
open Errors


type internal ErrorSerializerCtx = ErrorSerializerCtx


module private ToDocumentModel =



    let private resourceIdentifier ptr (d: DResourceIdentifier) : Result<ResourceIdentifier, Error list> =
        let errs = [
            if isNull d.``type`` then
                invalidNull "type" (ptr + "/type")
            elif LanguagePrimitives.PhysicalEquality d.``type`` skippedString then
                requiredMemberMissing "type" ptr

            if isNull d.id then
                invalidNull "id" (ptr + "/id")
            elif LanguagePrimitives.PhysicalEquality d.id skippedString then
                requiredMemberMissing "id" ptr
        ]

        if not errs.IsEmpty then
            Error errs
        else
            Ok { ``type`` = d.``type``; id = d.id }


    let private toOneRelationship resType relName ptr (d: DToOneRelationship) : Result<ToOne, Error list> =
        if isNull d.data then
            Error [ relInvalidNull resType relName (ptr + "/data") ]
        elif LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifier then
            Ok
                {
                    links = Skip
                    data = Skip
                    meta = Skip
                }
        else
            d.data
            |> resourceIdentifier (ptr + "/data")
            |> Result.map (fun data -> {
                links = Skip
                data = Include data
                meta = Skip
            })


    let private toOneNullableRelationship ptr (d: DToOneNullableRelationship) : Result<ToOneNullable, Error list> =
        if LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifier then
            Ok
                {
                    links = Skip
                    data = Skip
                    meta = Skip
                }
        else
            d.data
            |> Option.ofObj
            |> Option.traverseResult (resourceIdentifier (ptr + "/data"))
            |> Result.map (fun data -> {
                links = Skip
                data = Include data
                meta = Skip
            })


    let private toManyRelationship ptr (d: DToManyRelationship) : Result<ToMany, Error list> =
        if isNull d.data then
            Error [ invalidNull "data" (ptr + "/data") ]
        elif LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifierArray then
            Ok
                {
                    links = Skip
                    data = Skip
                    meta = Skip
                }
        else
            d.data
            |> Array.traverseResultAIndexed (fun i d ->
                let ptr = ptr + "/data/" + string i

                if isNull d then
                    Error [ invalidNullArrayItem "data" ptr ]
                else
                    resourceIdentifier ptr d)
            |> Result.map (fun data -> {
                links = Skip
                data = data |> Array.toList |> Include
                meta = Skip
            })


    let private resource
        (loggerFactory: ILoggerFactory)
        fieldStrictMode
        (getFieldType: ResourceTypeName -> FieldName -> Type option)
        (options: JsonSerializerOptions)
        ptr
        (d: DResource)
        : Result<Resource, Error list> =
        let errs = [
            if isNull d.``type`` then
                invalidNull "type" (ptr + "/type")
            elif LanguagePrimitives.PhysicalEquality d.``type`` skippedString then
                requiredMemberMissing "type" ptr

            if isNull d.id then
                invalidNull "id" (ptr + "/id")

            if isNull d.attributes then
                invalidNull "attributes" (ptr + "/attributes")
            if isNull d.relationships then
                invalidNull "relationships" (ptr + "/relationships")
        ]

        if not errs.IsEmpty then
            Error errs
        else
            let attrs =
                if LanguagePrimitives.PhysicalEquality d.attributes skippedJsonElementDict then
                    Ok Skip
                else
                    d.attributes
                    |> Seq.toArray
                    |> Array.traverseResultA (fun kvp ->
                        let attrName = kvp.Key
                        let jsonEl = kvp.Value

                        match getFieldType d.``type`` attrName with
                        | None ->
                            match fieldStrictMode with
                            | UnknownFieldStrictMode.Ignore -> Ok None
                            | UnknownFieldStrictMode.Warn logLevel ->
                                let logger = loggerFactory.CreateLogger "Felicity.StrictMode"

                                logger.Log(
                                    logLevel,
                                    "Request contained unknown attribute '{AttrName}' on type '{TypeName}' at {Pointer}",
                                    attrName,
                                    d.``type``,
                                    ptr + "/attributes/" + attrName
                                )

                                Ok None
                            | UnknownFieldStrictMode.Error ->
                                Error [ strictModeUnknownAttr d.``type`` attrName (ptr + "/attributes/" + attrName) ]
                        | Some tp ->
                            let isOption =
                                tp.IsGenericType && tp.GetGenericTypeDefinition() = typedefof<Option<_>>

                            if jsonEl.ValueKind = JsonValueKind.Null && not isOption then
                                Error [ attrInvalidNull d.``type`` attrName (ptr + "/attributes/" + attrName) ]
                            else
                                try
                                    Ok(Some(attrName, JsonSerializer.Deserialize(jsonEl.GetRawText(), tp, options)))
                                with :? JsonException as ex ->
                                    Error [ fieldInvalidJson attrName ex (ptr + "/attributes/" + attrName) ])
                    |> Result.map (Array.choose id >> dict >> Include)

            let rels =
                if LanguagePrimitives.PhysicalEquality d.relationships skippedJsonElementDict then
                    Ok Skip
                else
                    d.relationships
                    |> Seq.toArray
                    |> Array.traverseResultA (fun kvp ->
                        let relName = kvp.Key
                        let jsonEl = kvp.Value

                        match getFieldType d.``type`` relName with
                        | None ->
                            match fieldStrictMode with
                            | UnknownFieldStrictMode.Ignore -> Ok None
                            | UnknownFieldStrictMode.Warn logLevel ->
                                let logger = loggerFactory.CreateLogger "Felicity.StrictMode"

                                logger.Log(
                                    logLevel,
                                    "Request contained unknown relationship '{RelName}' on type '{TypeName}': {Pointer}",
                                    relName,
                                    d.``type``,
                                    ptr + "/relationships/" + relName
                                )

                                Ok None
                            | UnknownFieldStrictMode.Error ->
                                Error [ strictModeUnknownRel d.``type`` relName (ptr + "/relationships/" + relName) ]
                        | Some tp ->
                            try
                                let dRel = JsonSerializer.Deserialize(jsonEl.GetRawText(), tp, options)

                                let rel =
                                    match dRel with
                                    | null -> Error [ invalidNull relName (ptr + "/relationships/" + relName) ]
                                    | :? DToOneRelationship as r ->
                                        toOneRelationship d.``type`` relName (ptr + "/relationships/" + relName) r
                                        |> Result.map (fun r -> r :> IRelationship)
                                    | :? DToOneNullableRelationship as r ->
                                        toOneNullableRelationship (ptr + "/relationships/" + relName) r
                                        |> Result.map (fun r -> r :> IRelationship)
                                    | :? DToManyRelationship as r ->
                                        toManyRelationship (ptr + "/relationships/" + relName) r
                                        |> Result.map (fun r -> r :> IRelationship)
                                    | _ ->
                                        failwith
                                            $"Framework bug: Relationship was serialized to unknown type %s{dRel.GetType().FullName}"

                                rel |> Result.map (fun r -> Some(relName, r))
                            with :? JsonException as ex ->
                                Error [ fieldInvalidJson relName ex (ptr + "/relationships/" + relName) ])
                    |> Result.map (Array.choose id >> dict >> Include)

            match attrs, rels with
            | Error errs1, Error errs2 -> Error(errs1 @ errs2)
            | Error errs, Ok _
            | Ok _, Error errs -> Error errs
            | Ok attrs, Ok rels ->
                Ok
                    {
                        ``type`` = d.``type``
                        id =
                            if LanguagePrimitives.PhysicalEquality d.id skippedString then
                                Skip
                            else
                                Include d.id
                        attributes = attrs
                        links = Skip
                        relationships = rels
                        meta = Skip
                    }


    let resourceDocument
        loggerFactory
        fieldStrictMode
        getFieldType
        options
        (d: DResourceDocument)
        : Result<ResourceDocument, Error list> =
        let data =
            if LanguagePrimitives.PhysicalEquality d.data skippedResource then
                Error [ requiredMemberMissing "data" "" ]
            elif isNull d.data then
                Ok None
            else
                resource loggerFactory fieldStrictMode getFieldType options "/data" d.data
                |> Result.map Some

        let included =
            if LanguagePrimitives.PhysicalEquality d.included skippedResourceArray then
                Ok Skip
            elif isNull d.included then
                Error [ invalidNull "included" "/included" ]
            else
                d.included
                |> Array.indexed
                |> Array.traverseResultA (fun (i, r) ->
                    resource loggerFactory fieldStrictMode getFieldType options ("/included/" + string i) r)
                |> Result.map Include

        match data, included with
        | Error errs1, Error errs2 -> Error(errs1 @ errs2)
        | Error errs, Ok _
        | Ok _, Error errs -> Error errs
        | Ok d, Ok i ->
            Ok
                {
                    jsonapi = Skip
                    links = Skip
                    meta = Skip
                    data = d
                    included = i
                }


    let resourceIdentifierDocument (d: DResourceIdentifierDocument) : Result<ResourceIdentifierDocument, Error list> =
        let data =
            if LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifier then
                Error [ requiredMemberMissing "data" "" ]
            elif isNull d.data then
                Ok None
            else
                resourceIdentifier "/data" d.data |> Result.map Some

        data
        |> Result.map (fun d -> {
            jsonapi = Skip
            links = Skip // support later when valid use-cases arrive; remember to check LinkConfig
            meta = Skip
            data = d
            included = Skip
        })


    let resourceIdentifierCollectionDocument
        (d: DResourceIdentifierCollectionDocument)
        : Result<ResourceIdentifierCollectionDocument, Error list> =
        let data =
            if LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifierArray then
                Error [ requiredMemberMissing "data" "" ]
            elif isNull d.data then
                Error [ invalidNull "data" "/data" ]
            else
                d.data
                |> Array.traverseResultAIndexed (fun i d ->
                    let ptr = "/data/" + string i

                    if isNull d then
                        Error [ invalidNullArrayItem "data" ptr ]
                    else
                        resourceIdentifier ptr d)

        data
        |> Result.map (fun d -> {
            jsonapi = Skip
            links = Skip // support later when valid use-cases arrive; remember to check LinkConfig
            meta = Skip
            data = d
            included = Skip
        })



type internal Serializer<'ctx>
    (
        loggerFactory: ILoggerFactory,
        invalidJsonRequestBodyLogLevel,
        invalidJsonRequestBodyMaxSize,
        fieldStrictMode: UnknownFieldStrictMode<'ctx>,
        getFieldType,
        getFieldSerializationOrder,
        configureOptions
    ) =

    let logInvalidJsonRequestBody (json: string) (ex: JsonException) =
        match invalidJsonRequestBodyLogLevel with
        | None -> ()
        | Some logLevel ->
            let logger = loggerFactory.CreateLogger "Felicity.RequestBody"

            match invalidJsonRequestBodyMaxSize with
            | None ->
                logger.Log(
                    logLevel,
                    ex,
                    "Deserialization failed for the following request body:\n\n{RequestBody}",
                    json
                )
            | Some maxSize ->
                logger.Log(
                    logLevel,
                    ex,
                    "Deserialization failed for the following request body (first {MaxSize} of {Size} characters shown):\n\n{RequestBody}",
                    maxSize,
                    json.Length,
                    json.Substring(0, min maxSize json.Length)
                )


    let options = JsonSerializerOptions()

    do
        options.Encoder <- JavaScriptEncoder.UnsafeRelaxedJsonEscaping

        options.Converters.Add(JsonApiConverter())
        options.Converters.Add(LinkConverter())
        options.Converters.Add(ErrorSourceConverter())
        options.Converters.Add(ErrorConverter())
        options.Converters.Add(ResourceIdentifierConverter())
        options.Converters.Add(ToOneConverter())
        options.Converters.Add(ToOneNullableConverter())
        options.Converters.Add(ToManyConverter())
        options.Converters.Add(IRelationshipConverter())
        options.Converters.Add(ResourceConverter(getFieldSerializationOrder))
        options.Converters.Add(ResourceDocumentConverter())
        options.Converters.Add(ResourceCollectionDocumentConverter())
        options.Converters.Add(ResourceIdentifierDocumentConverter())
        options.Converters.Add(ResourceIdentifierCollectionDocumentConverter())
        options.Converters.Add(ErrorDocumentConverter())
        options.Converters.Add(JsonFSharpConverter())

        configureOptions options

    member _.SerializeToUtf8Bytes x =
        JsonSerializer.SerializeToUtf8Bytes(x, options)

    member _.DeserializeResourceDocument(json: string) =
        if String.IsNullOrEmpty json then
            Ok None
        else
            try
                JsonSerializer.Deserialize<DResourceDocument>(json, options)
                |> ToDocumentModel.resourceDocument loggerFactory fieldStrictMode getFieldType options
                |> Result.map Some
            with :? JsonException as ex ->
                logInvalidJsonRequestBody json ex
                Error [ invalidJson ex ]

    member _.DeserializeResourceIdentifierDocument(json: string) =
        if String.IsNullOrEmpty json then
            Ok None
        else
            try
                JsonSerializer.Deserialize<DResourceIdentifierDocument>(json, options)
                |> ToDocumentModel.resourceIdentifierDocument
                |> Result.map Some
            with :? JsonException as ex ->
                logInvalidJsonRequestBody json ex
                Error [ invalidJson ex ]

    member _.DeserializeResourceIdentifierCollectionDocument(json: string) =
        if String.IsNullOrEmpty json then
            Ok None
        else
            try
                JsonSerializer.Deserialize<DResourceIdentifierCollectionDocument>(json, options)
                |> ToDocumentModel.resourceIdentifierCollectionDocument
                |> Result.map Some
            with :? JsonException as ex ->
                logInvalidJsonRequestBody json ex
                Error [ invalidJson ex ]
