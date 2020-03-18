namespace Felicity

open System
open System.ComponentModel
open System.Text.Json.Serialization


[<EditorBrowsable(EditorBrowsableState.Never)>]
module InternalDeserializationModelDoNotUse =

  open System.Text.Json
  open System.Collections.Generic


  // Assuming no-one will ever use this string
  let skippedString = "bd3d00d8ff9c4f84b190cf5c5082b140"

  [<AllowNullLiteral>]
  type DResourceIdentifier () =
    member val ``type`` = skippedString with get, set
    member val id = skippedString with get, set

  let skippedResourceIdentifier = DResourceIdentifier ()
  let skippedResourceIdentifierArray : DResourceIdentifier [] = [||]

  [<AllowNullLiteral>]
  type DToOneRelationship () =
    member val data = skippedResourceIdentifier with get, set

  [<AllowNullLiteral>]
  type DToOneNullableRelationship () =
    member val data = skippedResourceIdentifier with get, set

  [<AllowNullLiteral>]
  type DToManyRelationship () =
    member val data = skippedResourceIdentifierArray with get, set


  let skippedJsonElementDict = Dictionary<string, JsonElement>()

  [<AllowNullLiteral>]
  type DResource () =
    member val ``type`` = skippedString with get, set
    member val id = skippedString with get, set
    member val attributes = skippedJsonElementDict with get, set
    member val relationships = skippedJsonElementDict with get, set

  let skippedResource = DResource ()
  let skippedResourceArray : DResource [] = [||]


  [<AllowNullLiteral>]
  type DResourceDocument () =
    member val data = skippedResource with get, set
    member val included = skippedResourceArray with get, set


  [<AllowNullLiteral>]
  type DResourceIdentifierDocument () =
    member val data = skippedResourceIdentifier with get, set


  [<AllowNullLiteral>]
  type DResourceIdentifierCollectionDocument () =
    member val data = skippedResourceIdentifierArray with get, set



[<CLIMutable>]
type internal JsonApi =
  {
    version: string Skippable
    meta: Map<string, obj> Skippable
  }


[<CLIMutable>]
type internal Link =
  {
    href: Uri option
    meta: Map<string, obj> Skippable
  }


[<CLIMutable>]
type internal ErrorSource =
  {
    pointer: string Skippable
    parameter: string Skippable
  }


[<CLIMutable>]
type Error =
  internal {
    id: string Skippable
    links: Map<string, Link> Skippable
    status: string Skippable
    code: string Skippable
    title: string Skippable
    detail: string Skippable
    source: ErrorSource Skippable
    meta: Map<string, obj> Skippable
    headers: (string * string) list
  }


[<CLIMutable>]
type internal ResourceIdentifier =
  {
    ``type``: string
    id: string
  }


type internal IRelationship = interface end


[<CLIMutable>]
type internal ToOne =
  {
    links: Map<string, Link> Skippable
    data: ResourceIdentifier Skippable
    meta: Map<string, obj> Skippable
  }
  interface IRelationship


[<CLIMutable>]
type internal ToOneNullable =
  {
    links: Map<string, Link> Skippable
    data: ResourceIdentifier option Skippable
    meta: Map<string, obj> Skippable
  }
  interface IRelationship


[<CLIMutable>]
type internal ToMany =
  {
    links: Map<string, Link> Skippable
    data: ResourceIdentifier list Skippable
    meta: Map<string, obj> Skippable
  }
  interface IRelationship


[<CLIMutable>]
type internal Resource =
  {
    ``type``: string
    id: string Skippable
    attributes: Map<string, obj> Skippable
    links: Map<string, Link> Skippable
    relationships: Map<string, IRelationship> Skippable
    meta: Map<string, obj> Skippable
  }


[<CLIMutable>]
type internal ResourceDocument =
  {
    jsonapi: JsonApi Skippable
    links: Map<string, Link> Skippable
    meta: Map<string, obj> Skippable
    data: Resource option
    included: Resource list Skippable
  }



[<CLIMutable>]
type internal ResourceCollectionDocument =
  {
    jsonapi: JsonApi Skippable
    links: Map<string, Link> Skippable
    meta: Map<string, obj> Skippable
    data: Resource list
    included: Resource list Skippable
  }


[<CLIMutable>]
type internal ResourceIdentifierDocument =
  {
    jsonapi: JsonApi Skippable
    links: Map<string, Link> Skippable
    meta: Map<string, obj> Skippable
    data: ResourceIdentifier option
  }


[<CLIMutable>]
type internal ResourceIdentifierCollectionDocument =
  {
    jsonapi: JsonApi Skippable
    links: Map<string, Link> Skippable
    meta: Map<string, obj> Skippable
    data: ResourceIdentifier list
  }


[<CLIMutable>]
type internal ErrorDocument =
  {
    jsonapi: JsonApi Skippable
    errors: Error list
    links: Map<string, Link> Skippable
    meta: Map<string, obj> Skippable
  }



module internal Relationship =

  let isEmpty (rel: IRelationship) =
    match rel with
    | :? ToOne as r -> r.data.isSkip && r.links.isSkip && r.meta.isSkip
    | :? ToOneNullable as r -> r.data.isSkip && r.links.isSkip && r.meta.isSkip
    | :? ToMany as r -> r.data.isSkip && r.links.isSkip && r.meta.isSkip
    | _ -> failwithf "Framework bug: Attempted to check emptiness of unknown relationship type %s" (rel.GetType().FullName)



module internal Resource' =

  let matches resType resId (res: Resource) =
    res.``type`` = resType && res.id = Include resId



module internal Json =

  open System.Text.Json
  open System.Text.Json.Serialization


  // Serialize the actual Uri objects using Uri.ToString() instead of
  // System.Text.Json's default Uri.OriginalString to avoid unwanted
  // ports when the Uri has been transformed by UriBuilder.
  //  - https://github.com/dotnet/runtime/issues/2279
  type UriConverter() =
    inherit JsonConverter<Uri>()

    override __.CanConvert (t: Type) =
      t = typeof<Uri>

    override _.Read(reader: byref<Utf8JsonReader>, t: Type, options: JsonSerializerOptions) =
      Uri(reader.GetString())

    override _.Write(writer: Utf8JsonWriter, uri: Uri, options: JsonSerializerOptions) =
      writer.WriteStringValue(uri.ToString())


  type JsonApiConverter () =
    inherit JsonConverter<JsonApi> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use JsonApiConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, jsonapi: JsonApi, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      jsonapi.version |> Skippable.iter (fun x ->
        writer.WritePropertyName "version"
        JsonSerializer.Serialize(writer, x, options)
      )

      jsonapi.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type LinkConverter () =
    inherit JsonConverter<Link> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use LinkConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, link: Link, options: JsonSerializerOptions) =
      if link.meta = Skip then
        JsonSerializer.Serialize(writer, link.href, options)
      else
        writer.WriteStartObject()

        writer.WritePropertyName "href"
        JsonSerializer.Serialize(writer, link.href, options)

        link.meta |> Skippable.iter (fun x ->
          writer.WritePropertyName "meta"
          JsonSerializer.Serialize(writer, x, options)
        )

        writer.WriteEndObject()


  type ErrorSourceConverter () =
    inherit JsonConverter<ErrorSource> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ErrorSourceConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, errSource: ErrorSource, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      errSource.pointer |> Skippable.iter (fun x ->
        writer.WritePropertyName "pointer"
        JsonSerializer.Serialize(writer, x, options)
      )

      errSource.parameter |> Skippable.iter (fun x ->
        writer.WritePropertyName "parameter"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type ErrorConverter () =
    inherit JsonConverter<Error> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ErrorConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, err: Error, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      err.id |> Skippable.iter (fun x ->
        writer.WritePropertyName "id"
        JsonSerializer.Serialize(writer, x, options)
      )

      err.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      err.status |> Skippable.iter (fun x ->
        writer.WritePropertyName "status"
        JsonSerializer.Serialize(writer, x, options)
      )

      err.code |> Skippable.iter (fun x ->
        writer.WritePropertyName "code"
        JsonSerializer.Serialize(writer, x, options)
      )

      err.title |> Skippable.iter (fun x ->
        writer.WritePropertyName "title"
        JsonSerializer.Serialize(writer, x, options)
      )

      err.detail |> Skippable.iter (fun x ->
        writer.WritePropertyName "detail"
        JsonSerializer.Serialize(writer, x, options)
      )

      err.source |> Skippable.iter (fun x ->
        writer.WritePropertyName "source"
        JsonSerializer.Serialize(writer, x, options)
      )

      err.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type ResourceIdentifierConverter () =
    inherit JsonConverter<ResourceIdentifier> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ResourceIdentifierConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, id: ResourceIdentifier, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      writer.WritePropertyName "type"
      JsonSerializer.Serialize(writer, id.``type``, options)

      writer.WritePropertyName "id"
      JsonSerializer.Serialize(writer, id.id, options)

      writer.WriteEndObject()


  type ToOneConverter () =
    inherit JsonConverter<ToOne> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ToOneConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, rel: ToOne, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      rel.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      rel.data |> Skippable.iter (fun x ->
        writer.WritePropertyName "data"
        JsonSerializer.Serialize(writer, x, options)
      )

      rel.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type ToOneNullableConverter () =
    inherit JsonConverter<ToOneNullable> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ToOneNullableConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, rel: ToOneNullable, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      rel.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      rel.data |> Skippable.iter (fun x ->
        writer.WritePropertyName "data"
        JsonSerializer.Serialize(writer, x, options)
      )

      rel.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type ToManyConverter () =
    inherit JsonConverter<ToMany> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ToManyConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, rel: ToMany, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      rel.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      rel.data |> Skippable.iter (fun x ->
        writer.WritePropertyName "data"
        JsonSerializer.Serialize(writer, x, options)
      )

      rel.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type IRelationshipConverter () =
    inherit JsonConverter<IRelationship> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use IRelationshipConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, rel: IRelationship, options: JsonSerializerOptions) =
      match rel with
      | :? ToOne as r -> JsonSerializer.Serialize(writer, r, options)
      | :? ToOneNullable as r -> JsonSerializer.Serialize(writer, r, options)
      | :? ToMany as r -> JsonSerializer.Serialize(writer, r, options)
      | _ -> failwithf "Framework bug: Attempted to serialize unknown relationship type %s" (rel.GetType().FullName)



  type ResourceConverter (getFieldSerializationOrder: ResourceTypeName -> FieldName []) =
    inherit JsonConverter<Resource> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ResourceConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, res: Resource, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      writer.WritePropertyName "type"
      JsonSerializer.Serialize(writer, res.``type``, options)

      res.id |> Skippable.iter (fun x ->
        writer.WritePropertyName "id"
        JsonSerializer.Serialize(writer, x, options)
      )

      res.attributes
      |> Skippable.filter (not << Map.isEmpty)
      |> Skippable.iter (fun attrs ->
        writer.WritePropertyName "attributes"
        writer.WriteStartObject ()
        for fieldName in getFieldSerializationOrder res.``type`` do
          match attrs.TryGetValue fieldName with
          | false, _ -> ()
          | true, attr ->
              writer.WritePropertyName fieldName
              JsonSerializer.Serialize(writer, attr, options)
        writer.WriteEndObject ()
      )

      res.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      res.relationships
      |> Skippable.filter (not << Map.isEmpty)
      |> Skippable.iter (fun rels ->
        writer.WritePropertyName "relationships"
        writer.WriteStartObject ()
        for fieldName in getFieldSerializationOrder res.``type`` do
          match rels.TryGetValue fieldName with
          | false, _ -> ()
          | true, rel ->
              // A relationship object MUST contain at least one of links/data/meta, so
              // don't serialize it if it's empty
              if not (Relationship.isEmpty rel) then
                writer.WritePropertyName fieldName
                JsonSerializer.Serialize(writer, rel, options)
        writer.WriteEndObject ()
      )

      res.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type ResourceDocumentConverter () =
    inherit JsonConverter<ResourceDocument> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ResourceDocumentConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, doc: ResourceDocument, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      doc.jsonapi |> Skippable.iter (fun x ->
        writer.WritePropertyName "jsonapi"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WritePropertyName "data"
      JsonSerializer.Serialize(writer, doc.data, options)

      doc.included |> Skippable.iter (fun x ->
        writer.WritePropertyName "included"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type ResourceCollectionDocumentConverter () =
    inherit JsonConverter<ResourceCollectionDocument> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ResourceCollectionDocumentConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, doc: ResourceCollectionDocument, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      doc.jsonapi |> Skippable.iter (fun x ->
        writer.WritePropertyName "jsonapi"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WritePropertyName "data"
      JsonSerializer.Serialize(writer, doc.data, options)

      doc.included |> Skippable.iter (fun x ->
        writer.WritePropertyName "included"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()


  type ResourceIdentifierDocumentConverter () =
    inherit JsonConverter<ResourceIdentifierDocument> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ResourceIdentifierDocumentConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, doc: ResourceIdentifierDocument, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      doc.jsonapi |> Skippable.iter (fun x ->
        writer.WritePropertyName "jsonapi"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WritePropertyName "data"
      JsonSerializer.Serialize(writer, doc.data, options)

      writer.WriteEndObject()


  type ResourceIdentifierCollectionDocumentConverter () =
    inherit JsonConverter<ResourceIdentifierCollectionDocument> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ResourceIdentifierCollectionDocumentConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, doc: ResourceIdentifierCollectionDocument, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      doc.jsonapi |> Skippable.iter (fun x ->
        writer.WritePropertyName "jsonapi"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WritePropertyName "data"
      JsonSerializer.Serialize(writer, doc.data, options)

      writer.WriteEndObject()


  type ErrorDocumentConverter () =
    inherit JsonConverter<ErrorDocument> ()

    override _.Read(_, _, _) =
      failwith "Framework bug: Attempted to use ErrorDocumentConverter when deserializing"

    override _.Write(writer: Utf8JsonWriter, doc: ErrorDocument, options: JsonSerializerOptions) =
      writer.WriteStartObject()

      doc.jsonapi |> Skippable.iter (fun x ->
        writer.WritePropertyName "jsonapi"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WritePropertyName "errors"
      JsonSerializer.Serialize(writer, doc.errors, options)

      doc.links |> Skippable.iter (fun x ->
        writer.WritePropertyName "links"
        JsonSerializer.Serialize(writer, x, options)
      )

      doc.meta |> Skippable.iter (fun x ->
        writer.WritePropertyName "meta"
        JsonSerializer.Serialize(writer, x, options)
      )

      writer.WriteEndObject()




module internal Links =


  let private toMeta (entries: Map<string, obj>) =
    entries
    |> Include
    |> Skippable.filter (not << Seq.isEmpty)


  /// Adds the specified link and meta to the link collection. The meta property
  /// is not included if there are no meta entries.
  let addOptWithMeta name (uri: Uri option) metaEntries links =
    links |> Map.add name { href = uri; meta = toMeta metaEntries }


  /// Adds the specified link and meta to the link collection. The meta property
  /// is not included if there are no meta entries.
  let addWithMeta name uri metaEntries links =
    addOptWithMeta name (Some uri) metaEntries links


  /// Adds the specified link to the link collection.
  let addOpt name uri links =
    addOptWithMeta name uri Map.empty links


  /// Adds the specified link to the link collection if the condition is true.
  let add name uri links =
    addOptWithMeta name (Some uri) Map.empty links


  /// Adds the specified link to the link collection if the condition is true.
  let addIf cond name uri links =
    if cond then addOptWithMeta name (Some uri) Map.empty links else links



module Error =


  /// An empty Error object.
  let empty =
    { id = Skip; links = Skip; status = Skip; code = Skip;
      title = Skip; detail = Skip; source = Skip; meta = Skip; headers = [] }


  /// Creates an Error object with the given status and with the id property set to a
  /// random GUID.
  let create (status: int) =
    { empty with
        id = Guid.NewGuid().ToString("N") |> string |> Include
        status = Include (string status)
    }


  /// Sets the error's id property.
  let setId (id: string) (err: Error) =
    { err with id = Include id }


  /// Sets the error's status property.
  let setStatus (statusCode: int) (err: Error) =
    { err with status = Include (string statusCode) }


  /// Sets the error's code property.
  let setCode code (err: Error) =
    { err with code = Include code }


  /// Sets the error's Title property.
  let setTitle text (err: Error) =
    { err with title = Include text }


  /// Sets the error's detail property.
  let setDetail text (err: Error) =
    { err with detail = Include text }


  /// Sets the error's detail property using a format string.
  let setDetailf format =
    Printf.ksprintf (fun s -> setDetail s) format


  /// Sets the error's source.parameter property (and removes any source.pointer value).
  let internal setSourceParam queryParam (err: Error) =
    { err with source = Include { parameter = Include queryParam; pointer = Skip } }


  /// Sets the error's source.pointer property (and removes any source.parameter value).
  let internal setSourcePointer jsonPointer (err: Error) =
    { err with source = Include { parameter = Skip; pointer = Include jsonPointer } }


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
    { err with headers = err.headers @ [key, value] }


  /// Transforms the error using the specified function on the inner value if opt is Some.
  let ifSome opt f (err: Error) =
    match opt with
    | None -> err
    | Some x -> err |> f x
