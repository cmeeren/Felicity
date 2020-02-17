namespace Felicity

open System
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Serialization
open Json
open InternalDeserializationModelDoNotUse
open Errors


module private ToDocumentModel =



  let private resourceIdentifier ptr (d: DResourceIdentifier) : Result<ResourceIdentifier, Error list> =
    let errs = [
      if isNull d.``type`` then invalidNull "type" (ptr + "/type")
      elif LanguagePrimitives.PhysicalEquality d.``type`` skippedString then requiredMemberMissing "type" ptr

      if isNull d.id then invalidNull "id" (ptr + "/id")
      elif LanguagePrimitives.PhysicalEquality d.id skippedString then requiredMemberMissing "id" ptr
    ]

    if not errs.IsEmpty then Error errs
    else
      Ok {
        ``type`` = d.``type`` 
        id = d.id
      }


  let private toOneRelationship resType relName ptr (d: DToOneRelationship) : Result<ToOne, Error list> =
    if isNull d.data then Error [relInvalidNull resType relName (ptr + "/data")]
    elif LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifier then
      Ok {
        links = Skip
        data = Skip
        meta = Skip
      }
    else
      d.data
      |> resourceIdentifier (ptr + "/data")
      |> Result.map (fun data ->
          {
            links = Skip
            data = Include data
            meta = Skip
          }
      )


  let private toOneNullableRelationship ptr (d: DToOneNullableRelationship) : Result<ToOneNullable, Error list> =
    if LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifier then
      Ok {
        links = Skip
        data = Skip
        meta = Skip
      }
    else
      d.data
      |> Option.ofObj
      |> Option.traverseResult (resourceIdentifier (ptr + "/data"))
      |> Result.map (fun data ->
          {
            links = Skip
            data = Include data
            meta = Skip
          }
      )


  let private toManyRelationship ptr (d: DToManyRelationship) : Result<ToMany, Error list> =
    if isNull d.data then Error [invalidNull "data" (ptr + "/data")]
    elif LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifierArray then
      Ok {
        links = Skip
        data = Skip
        meta = Skip
      }
    else
      d.data
      |> Array.mapi (fun i d -> resourceIdentifier (ptr + "/data/" + string i) d)
      |> Array.sequenceResultA
      |> Result.map (fun data ->
          {
            links = Skip
            data = data |> Array.toList |> Include
            meta = Skip
          }
      )


  let private resource (getFieldType: ResourceTypeName -> FieldName -> Type option) (options: JsonSerializerOptions) ptr (d: DResource) : Result<Resource, Error list> =
    let errs = [
      if isNull d.``type`` then invalidNull "type" (ptr + "/type")
      elif LanguagePrimitives.PhysicalEquality d.``type`` skippedString then requiredMemberMissing "type" ptr

      if isNull d.id then invalidNull "id" (ptr + "/id")

      if isNull d.attributes then invalidNull "attributes" (ptr + "/attributes")
      if isNull d.relationships then invalidNull "relationships" (ptr + "/relationships")
    ]

    if not errs.IsEmpty then Error errs
    else
      let attrs =
        if LanguagePrimitives.PhysicalEquality d.attributes skippedJsonElementDict then Ok Skip
        else
          d.attributes
          |> Seq.toArray
          |> Array.map (fun kvp ->
              let attrName = kvp.Key
              let jsonEl = kvp.Value
              getFieldType d.``type`` attrName
              |> Option.traverseResult (fun tp ->
                  try
                    Ok (attrName, JsonSerializer.Deserialize(jsonEl.GetRawText (), tp, options))
                  with (:? JsonException as ex) ->
                    Error [attrInvalidJson attrName (Exception.getInnerMsg ex) (ptr + "/attributes/" + attrName)]
              )
          )
          |> Array.sequenceResultA
          |> Result.map (Array.choose id >> Map.ofArray >> Include)

      let rels =
        if LanguagePrimitives.PhysicalEquality d.relationships skippedJsonElementDict then Ok Skip
        else
          d.relationships
          |> Seq.toArray
          |> Array.map (fun kvp ->
              let relName = kvp.Key
              let jsonEl = kvp.Value
              getFieldType d.``type`` relName
              |> Option.traverseResult (fun tp ->
                  try
                    let dRel = JsonSerializer.Deserialize(jsonEl.GetRawText (), tp, options)
                    let rel =
                      match dRel with
                      | :? DToOneRelationship as r ->
                          toOneRelationship d.``type`` relName (ptr + "/relationships/" + relName) r
                          |> Result.map (fun r -> r :> IRelationship)
                      | :? DToOneNullableRelationship as r ->
                          toOneNullableRelationship (ptr + "/relationships/" + relName) r
                          |> Result.map (fun r -> r :> IRelationship)
                      | :? DToManyRelationship as r ->
                          toManyRelationship (ptr + "/relationships/" + relName) r
                          |> Result.map (fun r -> r :> IRelationship)
                      | _ -> failwithf "Library bug: Relationship was serialized to unknown type %s" (dRel.GetType().FullName)
                    rel |> Result.map (fun r -> relName, r)
                  with (:? JsonException as ex) ->
                    Error [attrInvalidJson relName (Exception.getInnerMsg ex) (ptr + "/relationships/" + relName)]
              )
          )
          |> Array.sequenceResultA
          |> Result.map (Array.choose id >> Map.ofArray >> Include)

      match attrs, rels with
      | Error errs1, Error errs2 -> Error (errs1 @ errs2)
      | Error errs, Ok _ | Ok _, Error errs -> Error errs
      | Ok attrs, Ok rels ->
          Ok {
            ``type`` = d.``type``
            id = if LanguagePrimitives.PhysicalEquality d.id skippedString then Skip else Include d.id
            attributes = attrs
            links = Skip
            relationships = rels
            meta = Skip
          }


  let resourceDocument getFieldType options (d: DResourceDocument) : Result<ResourceDocument, Error list> =
    let data =
      if LanguagePrimitives.PhysicalEquality d.data skippedResource then Error [requiredMemberMissing "data" ""]
      elif isNull d.data then Ok None
      else resource getFieldType options "/data" d.data |> Result.map Some
    data |> Result.map (fun d ->
      {
        jsonapi = Skip
        links = Skip
        meta = Skip
        data = d
        included = Skip
      }
    )


  let resourceIdentifierDocument (d: DResourceIdentifierDocument) : Result<ResourceIdentifierDocument, Error list> =
    let data =
      if LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifier then Error [requiredMemberMissing "data" ""]
      elif isNull d.data then Ok None
      else resourceIdentifier "/data" d.data |> Result.map Some
    data |> Result.map (fun d ->
      {
        jsonapi = Skip
        links = Skip
        meta = Skip
        data = d
      }
    )


  let resourceIdentifierCollectionDocument (d: DResourceIdentifierCollectionDocument) : Result<ResourceIdentifierCollectionDocument, Error list> =
    let data =
      if LanguagePrimitives.PhysicalEquality d.data skippedResourceIdentifierArray then Error [requiredMemberMissing "data" ""]
      elif isNull d.data then Error [invalidNull "data" "/data"]
      else d.data |> Array.mapi (fun i d -> resourceIdentifier ("/data/" + string i) d) |> Array.sequenceResultA
    data |> Result.map (fun d ->
      {
        jsonapi = Skip
        links = Skip
        meta = Skip
        data = Array.toList d
      }
    )



type internal Serializer(getFieldType, getFieldSerializationOrder, configureOptions) =

    let options = JsonSerializerOptions()

    do
      options.Encoder <- JavaScriptEncoder.UnsafeRelaxedJsonEscaping

      options.Converters.Add(UriConverter())
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

    member _.Serialize x =
      JsonSerializer.Serialize(x, options)

    member _.DeserializeResourceDocument (json: string) =
      if String.IsNullOrEmpty json then Ok None
      else
        try
          JsonSerializer.Deserialize<DResourceDocument>(json, options)
          |> ToDocumentModel.resourceDocument getFieldType options
          |> Result.map Some
        with :? JsonException as ex ->
          let err = invalidJson ex.Message
          Error [err]

    member _.DeserializeResourceIdentifierDocument (json: string) =
      if String.IsNullOrEmpty json then Ok None
      else
        try
          JsonSerializer.Deserialize<DResourceIdentifierDocument>(json, options)
          |> ToDocumentModel.resourceIdentifierDocument
          |> Result.map Some
        with :? JsonException as ex ->
          let err = invalidJson ex.Message
          Error [err]

    member _.DeserializeResourceIdentifierCollectionDocument (json: string) =
      if String.IsNullOrEmpty json then Ok None
      else
        try
          JsonSerializer.Deserialize<DResourceIdentifierCollectionDocument>(json, options)
          |> ToDocumentModel.resourceIdentifierCollectionDocument
          |> Result.map Some
        with :? JsonException as ex ->
          let err = invalidJson ex.Message
          Error [err]
