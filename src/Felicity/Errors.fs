module internal Felicity.Errors

  open System.Text.Json

  (*
   * Any request: Negotiation and basic request validation
  *)

  let invalidAccept () =
    Error.create 406
    |> Error.setTitle "Invalid Accept header"
    |> Error.setDetail $"The client must accept the JSON:API media type (%s{Constants.jsonApiMediaType})"

  let invalidAcceptParams () =
    // "Servers MUST respond with a 406 Not Acceptable status code if a request’s Accept
    // header contains the JSON:API media type and all instances of that media type are
    // modified with media type parameters."
    Error.create 406
    |> Error.setTitle "Invalid JSON:API Accept params"
    |> Error.setDetail "The JSON:API media type in the Accept header must not be modified with media type parameters"

  let invalidContentType () =
    Error.create 415
    |> Error.setTitle "Invalid content type"
    |> Error.setDetail $"Request content must be sent with Content-Type set to the JSON:API media type (%s{Constants.jsonApiMediaType})"

  let invalidContentTypeParams () =
    // "Servers MUST respond with a 415 Unsupported Media Type status code if a request
    // specifies the header Content-Type: application/vnd.api+json with any media type
    // parameters."
    Error.create 415
    |> Error.setTitle "Invalid JSON:API Content-Type params"
    |> Error.setDetail "The JSON:API media type in the Content-Type header must not be modified with media type parameters"

  let illegalQueryParamName paramName =
    Error.create 400
    |> Error.setTitle "Invalid query parameter name"
    |> Error.setDetail $"'%s{paramName}' is not an allowed query parameter name according to the JSON:API specification"
    |> Error.setSourceParam paramName

  let incorrectPathCase invalidPath expectedPath =
    Error.create 404
    |> Error.setTitle "Incorrect path case"
    |> Error.setDetail $"The path '%s{invalidPath}' does not exist, but differs only by case from the existing path '%s{expectedPath}'. Paths are case sensitive."

  let incorrectPartialPathCase expectedPathPrefix =
    Error.create 404
    |> Error.setTitle "Incorrect path case"
    |> Error.setDetail $"Expected path '%s{expectedPathPrefix}'. Paths are case sensitive."


  (*
   * Any request: Misc.
  *)

  let unknownError () =
    Error.create 500
    |> Error.setTitle "Unknown error"
    |> Error.setDetail "An unknown error has occurred"

  let opMapCtxFailedNone () =
    Error.create 403
    |> Error.setTitle "Operation not available"
    |> Error.setDetail "This operation is currently not available"

  let preconditionRequired hasETag hasLastModified =
    let msg =
      match hasETag, hasLastModified with
      | false, false -> "This operation requires a precondition"  // Should never happen
      | true, false -> "This operation requires a precondition to be specified using the If-Match header"
      | false, true -> "This operation requires a precondition to be specified using the If-Unmodified-Since header"
      | true, true -> "This operation requires a precondition to be specified using the If-Match or If-Unmodified-Since headers"
    Error.create 428
    |> Error.setTitle "Precondition required"
    |> Error.setDetail msg

  let preconditionFailed hasETag hasLastModified =
    let msg =
      match hasETag, hasLastModified with
      | false, false -> "The precondition failed"  // Should never happen
      | true, false -> "The precondition specified in the If-Match header failed"
      | false, true -> "The precondition specified in the If-Unmodified-Since header failed"
      | true, true -> "The precondition specified in the If-Match or If-Unmodified-Since header failed"
    Error.create 412
    |> Error.setTitle "Precondition failed"
    |> Error.setDetail msg

  let lockTimeout () =
    Error.create 503
    |> Error.setTitle "Resource lock timeout"
    |> Error.setDetailf "Timed out waiting for the completion of other operations on the requested resource"


  (*
   * Any request: Deserialization and document validation
  *)

  let invalidJson (ex: JsonException) =
    Error.create 400
    |> Error.setTitle "Invalid request body"
    |> Error.setDetail ex.SafeMessage

  let fieldInvalidJson fieldName (ex: JsonException) pointer =
    Error.create 400
    |> Error.setTitle "Invalid request body"
    |> Error.setDetail (ex.SafeMessageForField(fieldName))
    |> Error.setSourcePointer pointer

  let invalidNull memberName pointer =
    Error.create 400
    |> Error.setTitle "Null not allowed"
    |> Error.setDetail $"Member '%s{memberName}' may not be null"
    |> Error.setSourcePointer pointer

  let invalidNullArrayItem arrayMemberName pointer =
    Error.create 400
    |> Error.setTitle "Null not allowed"
    |> Error.setDetail $"Array '%s{arrayMemberName}' may not have null items"
    |> Error.setSourcePointer pointer

  let requiredMemberMissing memberName pointer =
    Error.create 400
    |> Error.setTitle "Missing required member"
    |> Error.setDetail $"Missing required member '%s{memberName}'"
    |> Error.setSourcePointer pointer

  let attrInvalidNull resType attrName pointer =
    Error.create 400
    |> Error.setTitle "Null not allowed"
    |> Error.setDetail $"Attribute '%s{attrName}' on type '%s{resType}' is not nullable"
    |> Error.setSourcePointer pointer

  let relInvalidNull resType relName pointer =
    Error.create 400
    |> Error.setTitle "Null not allowed"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' is not nullable"
    |> Error.setSourcePointer pointer

  let identifierTypeMissingOrNull pointer =
    Error.create 400
    |> Error.setTitle "Malformed resource identifier"
    |> Error.setDetail "Resource identifier objects must contain a non-null 'type' member"
    |> Error.setSourcePointer pointer

  let identifierIdMissingOrNull pointer =
    Error.create 400
    |> Error.setTitle "Malformed resource identifier"
    |> Error.setDetail "Resource identifier objects must contain a non-null 'id' member"
    |> Error.setSourcePointer pointer

  let resTypeMissingOrNull pointer =
    Error.create 400
    |> Error.setTitle "Missing resource type"
    |> Error.setDetail "Resource objects must contain a non-null 'type' member"
    |> Error.setSourcePointer pointer


  (*
   * Any request: Strict mode
  *)

  let strictModeUnknownAttr resType attrName pointer =
    Error.create 400
    |> Error.setTitle "Unknown field"
    |> Error.setDetail $"Type '%s{resType}' has no attribute '%s{attrName}'"
    |> Error.setSourcePointer pointer

  let strictModeUnknownRel resType relName pointer =
    Error.create 400
    |> Error.setTitle "Unknown field"
    |> Error.setDetail $"Type '%s{resType}' has no relationship '%s{relName}'"
    |> Error.setSourcePointer pointer

  let strictModeUnknownOrUnusedQueryParam paramName =
    Error.create 400
    |> Error.setTitle "Unknown query parameter"
    |> Error.setDetail $"Query parameter '%s{paramName}' is not recognized for this operation"
    |> Error.setSourceParam paramName


  (*
   * Any request: Request parser
  *)

  let reqParserMissingData pointer =
    Error.create 400
    |> Error.setTitle "Missing resource object"
    |> Error.setDetail "This operation requires a single resource object as primary data"
    |> Error.setSourcePointer pointer

  let reqParserInvalidType expectedType invalidType pointer =
    Error.create 403
    |> Error.setTitle "Invalid type"
    |> Error.setDetail $"Expected a resource object with type '%s{expectedType}', but got '%s{invalidType}'"
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredId pointer =
    Error.create 400
    |> Error.setTitle "Missing resource ID"
    |> Error.setDetail "This operation requires a resource ID"
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredAttr attrName pointer =
    Error.create 400
    |> Error.setTitle "Missing required attribute"
    |> Error.setDetail $"Attribute '%s{attrName}' is required for this operation"
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredRel relName pointer =
    Error.create 400
    |> Error.setTitle "Missing required relationship"
    |> Error.setDetail $"Relationship '%s{relName}' is required for this operation"
    |> Error.setSourcePointer pointer

  let reqParserMissingIncludedResource resType resId pointer =
    Error.create 400
    |> Error.setTitle "Missing included resource"
    |> Error.setDetail $"Expected to find referenced resource with type '%s{resType}' and ID '%s{resId}' in the 'included' part of the request"
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredHeader headerName =
    Error.create 400
    |> Error.setTitle "Missing required header"
    |> Error.setDetail $"HTTP header '%s{headerName}' is required for this operation"

  let reqParserMissingRequiredQueryParam paramName =
    Error.create 400
    |> Error.setTitle "Missing required query parameter"
    |> Error.setDetail $"Query parameter '%s{paramName}' is required for this operation"
    |> Error.setSourceParam paramName

  let reqParserProhibitedAttr attrName pointer =
    Error.create 400
    |> Error.setTitle "Attribute not allowed"
    |> Error.setDetail $"Attribute '%s{attrName}' is not allowed for this operation"
    |> Error.setSourcePointer pointer

  let reqParserProhibitedRel relName pointer =
    Error.create 400
    |> Error.setTitle "Relationship not allowed"
    |> Error.setDetail $"Relationship '%s{relName}' is not allowed for this operation"
    |> Error.setSourcePointer pointer

  let reqParserProhibitedQueryParam paramName =
    Error.create 400
    |> Error.setTitle "Query parameter not allowed"
    |> Error.setDetail $"Query parameter '%s{paramName}' is not allowed for this operation"
    |> Error.setSourceParam paramName

  let reqParserProhibitedHeader headerName =
    Error.create 400
    |> Error.setTitle "Header not allowed"
    |> Error.setDetail $"Header '%s{headerName}' is not allowed for this operation"


  (*
   * Any request: Parsing
  *)

  let invalidParsedNone (info: ParsedValueInfo) =
    match info with
    | FromQuery data ->
        if data.NumValues > 1 then
          Error.create 400
          |> Error.setTitle "Invalid query parameter value"
          |> Error.setDetail $"Comma-separated query parameter '%s{data.Name}' got invalid value '%s{data.Value}' for item %i{data.ValueIndex + 1}"
          |> Error.setSourceParam data.Name
        else
          Error.create 400
          |> Error.setTitle "Invalid query parameter value"
          |> Error.setDetail $"Query parameter '%s{data.Name}' got invalid value '%s{data.Value}'"
          |> Error.setSourceParam data.Name
    | FromHeader data ->
        Error.create 400
        |> Error.setTitle "Invalid header value"
        |> Error.setDetail $"Header '%s{data.Name}' got invalid value '%s{data.Value}'"
    | FromBodyAttribute data ->
        let value =  data.StringValue |> String.truncate "…" 200
        Error.create 400
        |> Error.setTitle "Invalid attribute value"
        |> Error.setDetail $"Attribute '%s{data.Name}' got invalid value '%s{value}'"
    | FromBodyId data ->
        Error.create 400
        |> Error.setTitle "Invalid ID"
        |> Error.setDetail $"Got invalid resource ID value '%s{data.Value}'"

  let invalidParsedErrMsg (info: ParsedValueInfo) errMsg =
    match info with
    | FromQuery data ->
        if data.NumValues > 1 then
          Error.create 400
          |> Error.setTitle "Invalid query parameter value"
          |> Error.setDetail $"Comma-separated query parameter '%s{data.Name}' got invalid value '%s{data.Value}' for item %i{data.ValueIndex + 1}: %s{errMsg}"
          |> Error.setSourceParam data.Name
        else
          Error.create 400
          |> Error.setTitle "Invalid query parameter value"
          |> Error.setDetail $"Query parameter '%s{data.Name}' got invalid value '%s{data.Value}': %s{errMsg}"
          |> Error.setSourceParam data.Name
    | FromHeader data ->
        Error.create 400
        |> Error.setTitle "Invalid header value"
        |> Error.setDetail $"Header '%s{data.Name}' got invalid value '%s{data.Value}': %s{errMsg}"
    | FromBodyAttribute data ->
        let value =  data.StringValue |> String.truncate "…" 200
        Error.create 400
        |> Error.setTitle "Invalid attribute value"
        |> Error.setDetail $"Attribute '%s{data.Name}' got invalid value '%s{value}': %s{errMsg}"
    | FromBodyId data ->
        Error.create 400
        |> Error.setTitle "Invalid ID"
        |> Error.setDetail $"Got invalid resource ID value '%s{data.Value}': %s{errMsg}"

  let invalidEnum (info: ParsedValueInfo) allowedValues =
    let expectedStr =
      match allowedValues with
      | [x] -> x
      | xs -> "one of " + (xs |> List.map (sprintf "'%s'") |> String.concat ", ")

    match info with
    | FromQuery data ->
        // This error is not specific to sorting, but it's used there, so this is relevant:
        //
        // "If the server does not support sorting as specified in the query parameter sort,
        // it MUST return 400 Bad Request."
        if data.NumValues > 1 then
          Error.create 400
          |> Error.setTitle "Invalid query parameter value"
          |> Error.setDetail $"Comma-separated query parameter '%s{data.Name}' got invalid value '%s{data.Value}' for item %i{data.ValueIndex + 1}; expected %s{expectedStr}"
          |> Error.setSourceParam data.Name
        else
          Error.create 400
          |> Error.setTitle "Invalid query parameter value"
          |> Error.setDetail $"Query parameter '%s{data.Name}' got invalid value '%s{data.Value}'; expected %s{expectedStr}"
          |> Error.setSourceParam data.Name
    | FromHeader data ->
        Error.create 400
        |> Error.setTitle "Invalid header value"
        |> Error.setDetail $"Header '%s{data.Name}' got invalid value'%s{data.Value}'; expected %s{expectedStr}"
    | FromBodyAttribute data ->
        let value =  data.StringValue |> String.truncate "…" 200
        Error.create 400
        |> Error.setTitle "Invalid attribute value"
        |> Error.setDetail $"Attribute '%s{data.Name}' got invalid value '%s{value}'; expected %s{expectedStr}"
    | FromBodyId data ->
        Error.create 400
        |> Error.setTitle "Invalid ID"
        |> Error.setDetail $"Got invalid resource ID value '%s{data.Value}'; expected %s{expectedStr}"

  let queryNotSingular paramName numValues =
    Error.create 400
    |> Error.setTitle "Multiple values not allowed"
    |> Error.setDetail $"Query parameter '%s{paramName}' only accepts a single value, but got %i{numValues} comma-separated values"
    |> Error.setSourceParam paramName

  let queryDoesNotAcceptValue paramName invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetail $"Query parameter '%s{paramName}' must be specified without a value, but got '%s{invalidValue}'"
    |> Error.setSourceParam paramName


  (*
   * Field setters (relevant for POST collection and PATCH resource)
  *)

  let setAttrReadOnly attrName pointer =
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    Error.create 403
    |> Error.setTitle "Attribute read-only"
    |> Error.setDetail $"Attribute '%s{attrName}' is read-only"
    |> Error.setSourcePointer pointer

  let setAttrNullNotAllowed attrName =
    Error.create 403
    |> Error.setTitle "Null not allowed"
    |> Error.setDetail $"Attribute '%s{attrName}' may not be set to null"

  let setRelReadOnly relName pointer =
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    Error.create 403
    |> Error.setTitle "Relationship read-only"
    |> Error.setDetail $"Relationship '%s{relName}' is read-only"
    |> Error.setSourcePointer pointer

  let setRelNullNotAllowed relName =
    Error.create 403
    |> Error.setTitle "Null not allowed"
    |> Error.setDetail $"Relationship '%s{relName}' may not be set to null"

  let setToManyRelReplacementNotSupported relName resType pointer supportsPost supportsDelete =
    let extraMessage =
      match supportsPost, supportsDelete with
      | true, true -> "; it supports POST/DELETE to add/remove items"
      | true, false -> "; it supports POST to add items"
      | false, true -> "; it supports DELETE to remove items"
      | false, false -> ""
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    //
    // "A server MAY reject an attempt to do a full replacement of a to-many
    // relationship. In such a case, the server MUST reject the entire update, and return
    // a 403 Forbidden response."
    Error.create 403
    |> Error.setTitle "Complete replacement not allowed"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' does not support complete replacement using PATCH%s{extraMessage}"
    |> Error.setSourcePointer pointer

  let set2OneFieldMissing presentFieldName missingFieldName =
    Error.create 400
    |> Error.setTitle "Missing required field"
    |> Error.setDetail $"Field '%s{presentFieldName}' can only be set together with field '%s{missingFieldName}', which was missing"

  let set2DifferentNull field1Name field2Name =
    Error.create 400
    |> Error.setTitle "Invalid value combination"
    |> Error.setDetail $"The fields '%s{field1Name}' and '%s{field2Name}' must both be either null or non-null"


  (*
   * GET collection
  *)

  let collGetNotAllowed collName =
    Error.create 403
    |> Error.setTitle "GET collection not allowed"
    |> Error.setDetail $"Collection '%s{collName}' does not support fetching resources"

  let sortNotSupported () =
    // "If the server does not support sorting as specified in the query parameter sort,
    // it MUST return 400 Bad Request."
    Error.create 400
    |> Error.setTitle "Sort not supported"
    |> Error.setDetail "This operation does not support sorting"
    |> Error.setSourceParam "sort"


  (*
   * POST collection
  *)

  // Some of the errors under PATCH resource is also relevant, since

  let collPostNotAllowed collName =
    // "A server MAY return 403 Forbidden in response to an unsupported request to create
    // a resource."
    Error.create 403
    |> Error.setTitle "POST collection not allowed"
    |> Error.setDetail $"Collection '%s{collName}' does not support creating resources"

  let collPostMissingResourceObject pointer =
    Error.create 400
    |> Error.setTitle "Missing resource object"
    |> Error.setDetail "Request to create resources must contain a single resource object as primary data"
    |> Error.setSourcePointer pointer

  let collPostClientIdNotAllowed collName resType =
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // create a resource with a client-generated ID."
    Error.create 403
    |> Error.setTitle "Client-generated ID not supported"
    |> Error.setDetail $"Collection '%s{collName}' does not support creating resource type '%s{resType}' with a client-generated ID"
    |> Error.setSourcePointer "/data/id"

  let collPostTypeNotAllowed collName invalidType allowedTypes pointer =
    // "A server MUST return 409 Conflict when processing a POST request in which the
    // resource object’s type is not among the type(s) that constitute the collection
    // represented by the endpoint."
    Error.create 409
    |> Error.setTitle "Invalid resource type"
    |> match allowedTypes with
       | [x] -> Error.setDetail $"Collection '%s{collName}' does not support creating resources with type '%s{invalidType}'; expected '%s{x}'"
       | xs -> Error.setDetailf "Collection '%s' does not support creating resources with type '%s'; expected one of %s" collName invalidType (xs |> List.map (sprintf "'%s'") |> String.concat ", ")
    |> Error.setSourcePointer pointer


  (*
   * Resource lookup (and any resource-specific operation)
  *)

  let collLookupNotSupported collName =
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    Error.create 403
    |> Error.setTitle "Resource operations not supported"
    |> Error.setDetail $"Collection '%s{collName}' does not support any resource-specific operations"


  (*
   * GET resource
  *)

  let resGetNotSupportedPolymorphic resType collName =
    Error.create 403
    |> Error.setTitle "GET not supported"
    |> Error.setDetail $"GET is not supported for resource type '%s{resType}' (it may be supported for other resource types in collection '%s{collName}')"

  let resGetNotSupportedForAnyResource collName =
    Error.create 403
    |> Error.setTitle "GET not supported"
    |> Error.setDetail $"GET is not supported for any resource in collection '%s{collName}'"

  let resourceNotFound collName resId =
    Error.create 404
    |> Error.setTitle "Resource not found"
    |> Error.setDetail $"Collection '%s{collName}' does not contain a resource with ID '%s{resId}'"


  (*
   * PATCH resource
  *)

  let resPatchNotSupportedPolymorphic resType collName =
    // A server MUST return 403 Forbidden in response to an unsupported request to update
    // a resource or relationship.
    Error.create 403
    |> Error.setTitle "PATCH not supported"
    |> Error.setDetail $"PATCH is not supported for resource type '%s{resType}' (it may be supported for other resource types in collection '%s{collName}')"

  let resPatchNotSupportedForAnyResource collName =
    // A server MUST return 403 Forbidden in response to an unsupported request to update
    // a resource or relationship.
    Error.create 403
    |> Error.setTitle "PATCH not supported"
    |> Error.setDetail $"PATCH is not supported for any resource in collection '%s{collName}'"

  let resPatchMissingResourceObject pointer =
    Error.create 400
    |> Error.setTitle "Missing resource object"
    |> Error.setDetail "Request to update resources must contain a single resource object as primary data"
    |> Error.setSourcePointer pointer

  let resPatchMissingResourceId pointer =
    Error.create 400
    |> Error.setTitle "Missing resource ID"
    |> Error.setDetail "Request to update resources must contain a resource ID"
    |> Error.setSourcePointer pointer

  let resPatchTypeMismatch invalidType expectedType =
    // "A server MUST return 409 Conflict when processing a PATCH request in which the
    // resource object’s type and id do not match the server’s endpoint."
    Error.create 409
    |> Error.setTitle "Incorrect type"
    |> Error.setDetail $"Expected resource type '%s{expectedType}', but got '%s{invalidType}'"

  let resPatchIdMismatch invalidId expectedId =
    // "A server MUST return 409 Conflict when processing a PATCH request in which the
    // resource object’s type and id do not match the server’s endpoint."
    Error.create 409
    |> Error.setTitle "Incorrect ID"
    |> Error.setDetail $"Expected resource ID '%s{expectedId}', but got '%s{invalidId}'"


  (*
   * DELETE resource
  *)

  let resDeleteNotSupportedPolymorphic resType collName =
    Error.create 403  // 403 corresponds to unsupported PATCH
    |> Error.setTitle "DELETE not supported"
    |> Error.setDetail $"DELETE is not supported for resource type '%s{resType}' (it may be supported for other resource types in collection '%s{collName}')"

  let resDeleteNotSupportedForAnyResource collName =
    Error.create 403  // 403 corresponds to unsupported PATCH
    |> Error.setTitle "DELETE not supported"
    |> Error.setDetail $"DELETE is not supported for any resource in collection '%s{collName}'"


  (*
   * Any relationship self/related request
  *)

  let relNotDefinedPolymorphic relName resType collName =
    // "A server MUST return 404 Not Found when processing a request to fetch a
    // relationship link URL that does not exist."
    Error.create 404
    |> Error.setTitle "Relationship does not exist"
    |> Error.setDetail $"Relationship '%s{relName}' is not defined for resource type '%s{resType}' (other resource types in collection '%s{collName}' may have a relationship called '%s{relName}')"


  (*
   * GET relationship self or related
  *)

  let getRelNotDefinedPolymorphic relName resType collName =
    Error.create 403
    |> Error.setTitle "GET relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' is not readable (other resource types in collection '%s{collName}' may have a readable relationship called '%s{relName}')"

  let getRelNotDefinedForAnyResource relName collName =
    Error.create 403
    |> Error.setTitle "GET relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' is not readable for any resource in collection '%s{collName}'"

  let getRelWhileSkip () =
    Error.create 403
    |> Error.setTitle "No relationship value"
    |> Error.setDetail "The server has chosen not to disclose the value of this relationship"


  (*
   * Any request updating or parsing relationships
  *)

  let relMissingData relName pointer =
    Error.create 400
    |> Error.setTitle "Missing relationship data"
    |> Error.setDetail $"Relationship '%s{relName}' was specified without relationship data"
    |> Error.setSourcePointer pointer

  let relInvalidType relName invalidType allowedTypes pointer =
    Error.create 409  // 409 corresponds to PATCH type mismatch
    |> Error.setTitle "Invalid relationship type"
    |> match allowedTypes with
       | [x] -> Error.setDetail $"Relationship '%s{relName}' contains data with invalid type '%s{invalidType}'; expected '%s{x}'"
       | xs -> Error.setDetailf "Relationship '%s' contains data with invalid type '%s'; expected one of %s" relName invalidType (xs |> List.map (sprintf "'%s'") |> String.concat ", ")
    |> Error.setSourcePointer pointer


  (*
   * PATCH/POST/DELETE relationship self
  *)

  let relInvalidTypeSelf invalidType allowedTypes pointer =
    Error.create 409  // 409 corresponds to PATCH type mismatch
    |> Error.setTitle "Invalid relationship type"
    |> match allowedTypes with
       | [x] -> Error.setDetail $"Data contains invalid type '%s{invalidType}'; expected '%s{x}'"
       | xs -> Error.setDetailf "Data contains invalid type '%s'; expected one of %s" invalidType (xs |> List.map (sprintf "'%s'") |> String.concat ", ")
    |> Error.setSourcePointer pointer

  let patchToManyRelSelfNotAllowedPolymorphic relName resType supportsPost supportsDelete collName =
    let extraMessage =
      match supportsPost, supportsDelete with
      | true, true -> "; it supports POST/DELETE to add/remove items"
      | true, false -> "; it supports POST to add items"
      | false, true -> "; it supports DELETE to remove items"
      | false, false -> ""
    // "A server MAY reject an attempt to do a full replacement of a to-many
    // relationship. In such a case, the server MUST reject the entire update, and return
    // a 403 Forbidden response."
    Error.create 403
    |> Error.setTitle "Complete replacement not allowed"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' does not support complete replacement using PATCH%s{extraMessage} (other resource types in collection '%s{collName}' may have a relationship called '%s{relName}' that supports PATCH)"

  let patchRelSelfSettableButNotGettablePolymorphic relName resType collName =
    Error.create 403
    |> Error.setTitle "PATCH relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' is write-only and may only be updated through PATCH requests to the parent resource (other resource types in collection '%s{collName}' may have a relationship called '%s{relName}' that supports this operation)"

  let patchRelSelfNotAllowedForAnyResource relName collName supportsPost supportsDelete =
    let extraMessage =
      match supportsPost, supportsDelete with
      | true, true -> "; it may support POST/DELETE to add/remove items"
      | true, false -> "; it may support POST to add items"
      | false, true -> "; it may support DELETE to remove items"
      | false, false -> ""
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    //
    // "A server MAY reject an attempt to do a full replacement of a to-many
    // relationship. In such a case, the server MUST reject the entire update, and return
    // a 403 Forbidden response."
    Error.create 403
    |> Error.setTitle "PATCH relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' does not support PATCH for any resource in collection '%s{collName}'%s{extraMessage}"

  let postToManyRelSelfNotAllowedPolymorphic relName resType supportsPatch supportsDelete collName =
    let extraMessage =
      match supportsPatch, supportsDelete with
      | true, true -> "; it supports PATCH/DELETE to replace/remove items"
      | true, false -> "; it supports PATCH to replace items"
      | false, true -> "; it supports DELETE to remove items"
      | false, false -> ""
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a relationship."
    Error.create 403
    |> Error.setTitle "POST relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' does not support adding members using POST%s{extraMessage} (other resource types in collection '%s{collName}' may have a relationship called '%s{relName}' that supports POST)"

  let postToManyRelSelfNotAllowedForAnyResource relName collName supportsPatch supportsDelete =
    let extraMessage =
      match supportsPatch, supportsDelete with
      | true, true -> "; it may support PATCH/DELETE to replace/remove items"
      | true, false -> "; it may support PATCH to replace items"
      | false, true -> "; it may support DELETE to remove items"
      | false, false -> ""
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a relationship."
    Error.create 403
    |> Error.setTitle "POST relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' does not support adding members using POST for any resource in collection '%s{collName}'%s{extraMessage}"

  let deleteToManyRelSelfNotAllowedPolymorphic relName resType supportsPatch supportsPost collName =
    let extraMessage =
      match supportsPatch, supportsPost with
      | true, true -> "; it supports PATCH/POST to replace/add items"
      | true, false -> "; it supports PATCH to replace items"
      | false, true -> "; it supports POST to add items"
      | false, false -> ""
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a relationship."
    //
    // "If the client makes a DELETE request to a URL from a relationship link the server
    // MUST delete the specified members from the relationship or return a 403 Forbidden
    // response."
    Error.create 403
    |> Error.setTitle "DELETE relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' does not support removing members using DELETE%s{extraMessage} (other resource types in collection '%s{collName}' may have a relationship called '%s{relName}' that supports DELETE)"

  let deleteToManyRelSelfNotAllowedForAnyResource relName collName supportsPatch supportsPost =
    let extraMessage =
      match supportsPatch, supportsPost with
      | true, true -> "; it may support PATCH/POST to replace/add items"
      | true, false -> "; it may support PATCH to replace items"
      | false, true -> "; it may support POST to add items"
      | false, false -> ""
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a relationship."
    //
    // "If the client makes a DELETE request to a URL from a relationship link the server
    // MUST delete the specified members from the relationship or return a 403 Forbidden
    // response."
    Error.create 403
    |> Error.setTitle "DELETE relationship not supported"
    |> Error.setDetail $"Relationship '%s{relName}' does not support removing members using DELETE for any resource in collection '%s{collName}'%s{extraMessage}"

  let modifyRelSelfMissingData pointer =
    Error.create 400
    |> Error.setTitle "Missing data"
    |> Error.setDetail "The request must specify the primary data to update"
    |> Error.setSourcePointer pointer

  let modifyRelSelfReadOnly relName resType =
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    Error.create 403
    |> Error.setTitle "Relationship read-only"
    |> Error.setDetail $"Relationship '%s{relName}' on type '%s{resType}' is read-only"

  let relatedResourceNotFound resType resId pointer =
    // "A server MUST return 404 Not Found when processing a request that references a
    // related resource that does not exist."
    Error.create 404
    |> Error.setTitle "Resource not found"
    |> Error.setDetail $"The related resource with type '%s{resType}' and ID '%s{resId}' does not exist"
    |> Error.setSourcePointer pointer

  let relModifySelfWhileSkip () =
    Error.create 500
    |> Error.setTitle "No relationship value"
    |> Error.setDetail "The relationship was updated, but the server has erroneously chosen not to disclose the value of the updated relationship"


  (*
   * Custom operations (links)
  *)

  let customOpConditionFalse () =
    Error.create 403
    |> Error.setTitle "Operation not available"
    |> Error.setDetail "This operation is currently not available"

  let customOpNotDefinedPolymorphic linkName resType collName =
    Error.create 404
    |> Error.setTitle "Operation does not exist"
    |> Error.setDetail $"Operation '%s{linkName}' is not defined for resource type '%s{resType}' (it may exist for other resource types in collection '%s{collName}')"

  let customOpVerbNotDefinedPolymorphic linkName resType invalidVerb allowHeader collName =
    Error.create 405
    |> Error.setTitle "Method not allowed"
    |> Error.setDetail $"Operation '%s{linkName}' on type '%s{resType}' does not support %s{invalidVerb} (other resource types in collection '%s{collName}' may have an operation '%s{linkName}' supporting %s{invalidVerb})"
    |> Error.addHeader "Allow" allowHeader

  let customOpVerbNotDefinedForAnyResource linkName invalidVerb collName allowHeader =
    Error.create 405
    |> Error.setTitle "Method not allowed"
    |> Error.setDetail $"Operation '%s{linkName}' does not support %s{invalidVerb} for any resource in collection '%s{collName}'"
    |> Error.addHeader "Allow" allowHeader


  (*
   * Misc.
  *)

  let relationshipDoesNotExistForAnyResource relName collName =
    Error.create 404
    |> Error.setTitle "Unknown relationship"
    |> Error.setDetail $"The relationship '%s{relName}' does not exist for any resource in collection '%s{collName}'"

  let linkOrRelationshipDoesNotExistForAnyResource linkOrRelName collName =
    Error.create 404
    |> Error.setTitle "Unknown link or relationship"
    |> Error.setDetail $"The link or relationship '%s{linkOrRelName}' does not exist for any resource in collection '%s{collName}'"

  let invalidPath path collName =
    Error.create 404
    |> Error.setTitle "Invalid path"
    |> Error.setDetail $"The path '%s{path}' does not exist for resources in collection '%s{collName}'"

  let methodNotAllowed method allowedHeaderValue =
    Error.create 405
    |> Error.setTitle "Method not allowed"
    |> Error.setDetail $"%s{method} is not a known method for this endpoint"
    |> Error.addHeader "Allow" allowedHeaderValue
