namespace Felicity

open System
open System.Collections.Generic
open System.ComponentModel
open System.Text.Json.Serialization


[<EditorBrowsable(EditorBrowsableState.Never)>]
module InternalDeserializationModelDoNotUse =

    open System.Text.Json


    // Assuming no-one will ever use this string
    let skippedString = "bd3d00d8ff9c4f84b190cf5c5082b140"

    [<AllowNullLiteral>]
    type DResourceIdentifier() =
        member val ``type`` = skippedString with get, set
        member val id = skippedString with get, set

    let skippedResourceIdentifier = DResourceIdentifier()
    let skippedResourceIdentifierArray: DResourceIdentifier[] = [||]

    [<AllowNullLiteral>]
    type DToOneRelationship() =
        member val data = skippedResourceIdentifier with get, set

    [<AllowNullLiteral>]
    type DToOneNullableRelationship() =
        member val data = skippedResourceIdentifier with get, set

    [<AllowNullLiteral>]
    type DToManyRelationship() =
        member val data = skippedResourceIdentifierArray with get, set


    let skippedJsonElementDict = Dictionary<string, JsonElement>()

    [<AllowNullLiteral>]
    type DResource() =
        member val ``type`` = skippedString with get, set
        member val id = skippedString with get, set
        member val attributes = skippedJsonElementDict with get, set
        member val relationships = skippedJsonElementDict with get, set

    let skippedResource = DResource()
    let skippedResourceArray: DResource[] = [||]


    [<AllowNullLiteral>]
    type DResourceDocument() =
        member val data = skippedResource with get, set
        member val included = skippedResourceArray with get, set


    [<AllowNullLiteral>]
    type DResourceIdentifierDocument() =
        member val data = skippedResourceIdentifier with get, set


    [<AllowNullLiteral>]
    type DResourceIdentifierCollectionDocument() =
        member val data = skippedResourceIdentifierArray with get, set



[<CLIMutable>]
type internal JsonApi = {
    version: string Skippable
    meta: IDictionary<string, obj> Skippable
}


[<CLIMutable>]
type internal Link = {
    href: string option
    meta: IDictionary<string, obj> Skippable
}


[<CLIMutable>]
type internal ErrorSource = {
    pointer: string Skippable
    parameter: string Skippable
}


[<CLIMutable>]
type Error = internal {
    id: string Skippable
    links: Map<string, Link> Skippable
    status: string Skippable
    code: string Skippable
    title: string Skippable
    detail: string Skippable
    source: ErrorSource Skippable
    meta: Map<string, obj> Skippable
    headers: (string * string) list
    appendPointer: string voption
}


[<CLIMutable; Struct>]
type internal ResourceIdentifier = { ``type``: string; id: string }


type internal IRelationship =
    interface
    end


[<CLIMutable>]
type internal ToOne =
    {
        links: IDictionary<string, Link> Skippable
        mutable data: ResourceIdentifier Skippable
        meta: IDictionary<string, obj> Skippable
    }

    interface IRelationship


[<CLIMutable>]
type internal ToOneNullable =
    {
        links: IDictionary<string, Link> Skippable
        mutable data: ResourceIdentifier option Skippable
        meta: IDictionary<string, obj> Skippable
    }

    interface IRelationship


[<CLIMutable>]
type internal ToMany =
    {
        links: IDictionary<string, Link> Skippable
        mutable data: ResourceIdentifier list Skippable
        meta: IDictionary<string, obj> Skippable
    }

    interface IRelationship


[<CLIMutable>]
type internal Resource = {
    ``type``: string
    id: string Skippable
    attributes: IDictionary<string, obj> Skippable
    links: IDictionary<string, Link> Skippable
    mutable relationships: IDictionary<string, IRelationship> Skippable
    meta: IDictionary<string, obj> Skippable
}


[<CLIMutable>]
type internal ResourceDocument = {
    jsonapi: JsonApi Skippable
    links: IDictionary<string, Link> Skippable
    meta: IDictionary<string, obj> Skippable
    data: Resource option
    included: Resource[] Skippable
}



[<CLIMutable>]
type internal ResourceCollectionDocument = {
    jsonapi: JsonApi Skippable
    links: IDictionary<string, Link> Skippable
    meta: IDictionary<string, obj> Skippable
    data: Resource[]
    included: Resource[] Skippable
}


[<CLIMutable>]
type internal NoResourceDocument = {
    jsonapi: JsonApi Skippable
    links: IDictionary<string, Link> Skippable
    meta: IDictionary<string, obj> Skippable
}


[<CLIMutable>]
type internal ResourceIdentifierDocument = {
    jsonapi: JsonApi Skippable
    links: IDictionary<string, Link> Skippable
    meta: IDictionary<string, obj> Skippable
    data: ResourceIdentifier option
    included: Resource[] Skippable
}


[<CLIMutable>]
type internal ResourceIdentifierCollectionDocument = {
    jsonapi: JsonApi Skippable
    links: IDictionary<string, Link> Skippable
    meta: IDictionary<string, obj> Skippable
    data: ResourceIdentifier[]
    included: Resource[] Skippable
}


[<CLIMutable>]
type internal ErrorDocument = {
    jsonapi: JsonApi Skippable
    errors: Error list
    links: IDictionary<string, Link> Skippable
    meta: IDictionary<string, obj> Skippable
}



module internal Relationship =

    let isEmpty (rel: IRelationship) =
        match rel with
        | :? ToOne as r -> r.data.isSkip && r.links.isSkip && r.meta.isSkip
        | :? ToOneNullable as r -> r.data.isSkip && r.links.isSkip && r.meta.isSkip
        | :? ToMany as r -> r.data.isSkip && r.links.isSkip && r.meta.isSkip
        | _ ->
            failwith
                $"Framework bug: Attempted to check emptiness of unknown relationship type %s{rel.GetType().FullName}"



module internal Resource' =

    let matches resType resId (res: Resource) =
        res.``type`` = resType && res.id = Include resId



module internal Json =

    open System.Text.Json


    type JsonApiConverter() =
        inherit JsonConverter<JsonApi>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use JsonApiConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, jsonapi: JsonApi, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            jsonapi.version
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "version"
                JsonSerializer.Serialize(writer, x, options))

            jsonapi.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type LinkConverter() =
        inherit JsonConverter<Link>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use LinkConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, link: Link, options: JsonSerializerOptions) =
            if link.meta = Skip then
                JsonSerializer.Serialize(writer, link.href, options)
            else
                writer.WriteStartObject()

                writer.WritePropertyName "href"
                JsonSerializer.Serialize(writer, link.href, options)

                link.meta
                |> Skippable.iter (fun x ->
                    writer.WritePropertyName "meta"
                    JsonSerializer.Serialize(writer, x, options))

                writer.WriteEndObject()


    type ErrorSourceConverter() =
        inherit JsonConverter<ErrorSource>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ErrorSourceConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, errSource: ErrorSource, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            errSource.pointer
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "pointer"
                JsonSerializer.Serialize(writer, x, options))

            errSource.parameter
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "parameter"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ErrorConverter() =
        inherit JsonConverter<Error>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ErrorConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, err: Error, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            err.id
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "id"
                JsonSerializer.Serialize(writer, x, options))

            err.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            err.status
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "status"
                JsonSerializer.Serialize(writer, x, options))

            err.code
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "code"
                JsonSerializer.Serialize(writer, x, options))

            err.title
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "title"
                JsonSerializer.Serialize(writer, x, options))

            err.detail
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "detail"
                JsonSerializer.Serialize(writer, x, options))

            err.source
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "source"
                JsonSerializer.Serialize(writer, x, options))

            err.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ResourceIdentifierConverter() =
        inherit JsonConverter<ResourceIdentifier>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ResourceIdentifierConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, id: ResourceIdentifier, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            writer.WritePropertyName "type"
            JsonSerializer.Serialize(writer, id.``type``, options)

            writer.WritePropertyName "id"
            JsonSerializer.Serialize(writer, id.id, options)

            writer.WriteEndObject()


    type ToOneConverter() =
        inherit JsonConverter<ToOne>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ToOneConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, rel: ToOne, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            rel.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            rel.data
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "data"
                JsonSerializer.Serialize(writer, x, options))

            rel.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ToOneNullableConverter() =
        inherit JsonConverter<ToOneNullable>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ToOneNullableConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, rel: ToOneNullable, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            rel.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            rel.data
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "data"
                JsonSerializer.Serialize(writer, x, options))

            rel.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ToManyConverter() =
        inherit JsonConverter<ToMany>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ToManyConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, rel: ToMany, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            rel.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            rel.data
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "data"
                JsonSerializer.Serialize(writer, x, options))

            rel.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type IRelationshipConverter() =
        inherit JsonConverter<IRelationship>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use IRelationshipConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, rel: IRelationship, options: JsonSerializerOptions) =
            match rel with
            | :? ToOne as r -> JsonSerializer.Serialize(writer, r, options)
            | :? ToOneNullable as r -> JsonSerializer.Serialize(writer, r, options)
            | :? ToMany as r -> JsonSerializer.Serialize(writer, r, options)
            | _ ->
                failwith $"Framework bug: Attempted to serialize unknown relationship type %s{rel.GetType().FullName}"



    type ResourceConverter(getFieldSerializationOrder: ResourceTypeName -> FieldName[]) =
        inherit JsonConverter<Resource>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ResourceConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, res: Resource, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            writer.WritePropertyName "type"
            JsonSerializer.Serialize(writer, res.``type``, options)

            res.id
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "id"
                JsonSerializer.Serialize(writer, x, options))

            res.attributes
            |> Skippable.filter (fun x -> x.Count > 0)
            |> Skippable.iter (fun attrs ->
                writer.WritePropertyName "attributes"
                writer.WriteStartObject()

                for fieldName in getFieldSerializationOrder res.``type`` do
                    match attrs.TryGetValue fieldName with
                    | false, _ -> ()
                    | true, attr ->
                        writer.WritePropertyName fieldName
                        JsonSerializer.Serialize(writer, attr, options)

                writer.WriteEndObject())

            res.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            res.relationships
            |> Skippable.filter (fun x -> x.Count > 0)
            |> Skippable.iter (fun rels ->
                writer.WritePropertyName "relationships"
                writer.WriteStartObject()

                for fieldName in getFieldSerializationOrder res.``type`` do
                    match rels.TryGetValue fieldName with
                    | false, _ -> ()
                    | true, rel ->
                        // A relationship object MUST contain at least one of links/data/meta, so
                        // don't serialize it if it's empty
                        if not (Relationship.isEmpty rel) then
                            writer.WritePropertyName fieldName
                            JsonSerializer.Serialize(writer, rel, options)

                writer.WriteEndObject())

            res.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ResourceDocumentConverter() =
        inherit JsonConverter<ResourceDocument>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ResourceDocumentConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, doc: ResourceDocument, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            doc.jsonapi
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "jsonapi"
                JsonSerializer.Serialize(writer, x, options))

            doc.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            doc.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WritePropertyName "data"
            JsonSerializer.Serialize(writer, doc.data, options)

            doc.included
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "included"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ResourceCollectionDocumentConverter() =
        inherit JsonConverter<ResourceCollectionDocument>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ResourceCollectionDocumentConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, doc: ResourceCollectionDocument, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            doc.jsonapi
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "jsonapi"
                JsonSerializer.Serialize(writer, x, options))

            doc.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            doc.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WritePropertyName "data"
            JsonSerializer.Serialize(writer, doc.data, options)

            doc.included
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "included"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ResourceIdentifierDocumentConverter() =
        inherit JsonConverter<ResourceIdentifierDocument>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ResourceIdentifierDocumentConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, doc: ResourceIdentifierDocument, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            doc.jsonapi
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "jsonapi"
                JsonSerializer.Serialize(writer, x, options))

            doc.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            doc.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WritePropertyName "data"
            JsonSerializer.Serialize(writer, doc.data, options)

            doc.included
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "included"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ResourceIdentifierCollectionDocumentConverter() =
        inherit JsonConverter<ResourceIdentifierCollectionDocument>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ResourceIdentifierCollectionDocumentConverter when deserializing"

        override _.Write
            (
                writer: Utf8JsonWriter,
                doc: ResourceIdentifierCollectionDocument,
                options: JsonSerializerOptions
            ) =
            writer.WriteStartObject()

            doc.jsonapi
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "jsonapi"
                JsonSerializer.Serialize(writer, x, options))

            doc.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            doc.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WritePropertyName "data"
            JsonSerializer.Serialize(writer, doc.data, options)

            doc.included
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "included"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()


    type ErrorDocumentConverter() =
        inherit JsonConverter<ErrorDocument>()

        override _.Read(_, _, _) =
            failwith "Framework bug: Attempted to use ErrorDocumentConverter when deserializing"

        override _.Write(writer: Utf8JsonWriter, doc: ErrorDocument, options: JsonSerializerOptions) =
            writer.WriteStartObject()

            doc.jsonapi
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "jsonapi"
                JsonSerializer.Serialize(writer, x, options))

            writer.WritePropertyName "errors"
            JsonSerializer.Serialize(writer, doc.errors, options)

            doc.links
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "links"
                JsonSerializer.Serialize(writer, x, options))

            doc.meta
            |> Skippable.iter (fun x ->
                writer.WritePropertyName "meta"
                JsonSerializer.Serialize(writer, x, options))

            writer.WriteEndObject()



module internal Links =


    let private toMeta (entries: IDictionary<string, obj>) =
        entries |> Include |> Skippable.filter (not << Seq.isEmpty)


    /// Adds the specified link and meta to the link collection. The meta property
    /// is not included if there are no meta entries.
    let addOptWithMeta name (uri: string option) metaEntries links =
        links
        |> Map.add
            name
            {
                href = uri
                meta = toMeta metaEntries
            }


    /// Adds the specified link and meta to the link collection. The meta property
    /// is not included if there are no meta entries.
    let addWithMeta name uri metaEntries links =
        addOptWithMeta name (Some uri) metaEntries links


    /// Adds the specified link to the link collection.
    let addOpt name uri links = addOptWithMeta name uri Map.empty links


    /// Adds the specified link to the link collection if the condition is true.
    let add name uri links =
        addOptWithMeta name (Some uri) Map.empty links


    /// Adds the specified link to the link collection if the condition is true.
    let addIf cond name uri links =
        if cond then
            addOptWithMeta name (Some uri) Map.empty links
        else
            links



module Error =


    /// An empty Error object.
    let empty = {
        id = Skip
        links = Skip
        status = Skip
        code = Skip
        title = Skip
        detail = Skip
        source = Skip
        meta = Skip
        headers = []
        appendPointer = ValueNone
    }


    /// Creates an Error object with the given status and with the id property set to a
    /// random GUID.
    let create (status: int) =
        { empty with
            id = Guid.NewGuid().ToString("N") |> string |> Include
            status = Include(string status)
        }


    /// Sets the error's id property.
    let setId (id: string) (err: Error) = { err with id = Include id }


    /// Sets the error's status property.
    let setStatus (statusCode: int) (err: Error) =
        { err with
            status = Include(string statusCode)
        }


    /// Sets the error's code property.
    let setCode code (err: Error) = { err with code = Include code }


    /// Sets the error's Title property.
    let setTitle text (err: Error) = { err with title = Include text }


    /// Sets the error's detail property.
    let setDetail text (err: Error) = { err with detail = Include text }


    /// Sets the error's detail property using a format string.
    let setDetailf format = Printf.ksprintf setDetail format


    /// Sets the error's source.parameter property (and removes any source.pointer value).
    /// Note that Felicity almost always sets/overrides the pointer/parameter, even on
    /// user-defined errors. You should very rarely need to use this.
    let setSourceParam queryParamName (err: Error) =
        { err with
            source =
                Include
                    {
                        parameter = Include queryParamName
                        pointer = Skip
                    }
        }


    /// Sets the error's source.pointer property (and removes any source.parameter value).
    /// Note that Felicity almost always sets/overrides the pointer/parameter, even on
    /// user-defined errors. You should very rarely need to use this.
    let setSourcePointer jsonPointer (err: Error) =
        { err with
            source =
                Include
                    {
                        parameter = Skip
                        pointer = Include(jsonPointer + defaultValueArg err.appendPointer "")
                    }
        }


    /// Adds the specified key-value pair to the error's Meta object.
    let addMeta key value (err: Error) =
        { err with
            meta =
                err.meta
                |> Skippable.map (Map.add key (box value))
                |> Skippable.orElse (Map.empty.Add(key, box value) |> Include)
        }


    /// Adds the specified key-value pair to the error's Meta object if condition
    /// is true.
    let addMetaIf condition key value err =
        if condition then addMeta key value err else err


    /// Adds the specified header to responses containing this error.
    let addHeader key value (err: Error) =
        { err with
            headers = err.headers @ [ key, value ]
        }


    /// Appends the specified value to the error's (current or future) pointer. The value should start with '/' for the
    /// pointer to be correct.
    ///
    /// This function can be useful for pointing to paths within object- or array-valued attributes, where the "base"
    /// source pointer (the pointer to the attribute) is set automatically by Felicity. For example, Felicity will set the
    /// pointer to the value '/data/attributes/myComplexAttr', and you can then call this function with the value
    /// '/waffles/1/isTasty'. The error's pointer will then be '/data/attributes/myComplexAttr/waffles/1/isTasty'.
    let appendPointer (value: string) (err: Error) =
        { err with
            appendPointer = ValueSome value
            source =
                err.source
                |> Skippable.map (fun s ->
                    { s with
                        pointer = s.pointer |> Skippable.map (fun p -> p + value)
                    })
        }


    /// Transforms the error using the specified function on the inner value if opt is Some.
    let ifSome opt f (err: Error) =
        match opt with
        | None -> err
        | Some x -> err |> f x
