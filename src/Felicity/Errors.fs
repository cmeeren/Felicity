module internal Felicity.Errors

  (*
   * Any request: Negotiation and basic request validation
  *)

  let invalidAccept =
    Error.create 406
    |> Error.setTitle "Invalid Accept header"
    |> Error.setDetailf "The client must accept the JSON:API media type (%s)" Constants.jsonApiMediaType

  let invalidAcceptParams =
    // "Servers MUST respond with a 406 Not Acceptable status code if a request’s Accept
    // header contains the JSON:API media type and all instances of that media type are
    // modified with media type parameters."
    Error.create 406
    |> Error.setTitle "Invalid JSON:API Accept params"
    |> Error.setDetail "The JSON:API media type in the Accept header must not be modified with media type parameters"

  let invalidContentType =
    Error.create 415
    |> Error.setTitle "Invalid content type"
    |> Error.setDetailf "Request content must be sent with Content-Type set to the JSON:API media type (%s)" Constants.jsonApiMediaType

  let invalidContentTypeParams =
    // "Servers MUST respond with a 415 Unsupported Media Type status code if a request
    // specifies the header Content-Type: application/vnd.api+json with any media type
    // parameters."
    Error.create 415
    |> Error.setTitle "Invalid JSON:API Content-Type params"
    |> Error.setDetail "The JSON:API media type in the Content-Type header must not be modified with media type parameters"

  let illegalQueryParamName paramName =
    Error.create 400
    |> Error.setTitle "Invalid query parameter name"
    |> Error.setDetailf "'%s' is not an allowed query parameter name according to the JSON:API specification" paramName
    |> Error.setSourceParam paramName


  (*
   * Any request: Misc.
  *)

  let unknownError =
    Error.create 500
    |> Error.setTitle "Unknown error"
    |> Error.setDetail "An unknown error has occurred"

  let opMapCtxFailedNone =
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


  (*
   * Any request: Deserialization and document validation
  *)

  let invalidJson msg =
    Error.create 400
    |> Error.setTitle "Invalid request body"
    |> Error.setDetail msg

  let attrInvalidJson attrName msg pointer =
    Error.create 400
    |> Error.setTitle "Invalid request body"
    |> Error.setDetailf "Invalid JSON for attribute '%s': %s" attrName msg
    |> Error.setSourcePointer pointer

  let invalidNull memberName pointer =
    Error.create 400
    |> Error.setTitle "Null not allowed"
    |> Error.setDetailf "Member '%s' may not be null" memberName
    |> Error.setSourcePointer pointer

  let requiredMemberMissing memberName pointer =
    Error.create 400
    |> Error.setTitle "Missing required member"
    |> Error.setDetailf "Missing required member '%s'" memberName
    |> Error.setSourcePointer pointer

  let attrInvalidNull resType attrName pointer =
    Error.create 400
    |> Error.setTitle "Null not allowed"
    |> Error.setDetailf "Attribute '%s' on type '%s' is not nullable" attrName resType
    |> Error.setSourcePointer pointer

  let relInvalidNull resType relName pointer =
    Error.create 400
    |> Error.setTitle "Null not allowed"
    |> Error.setDetailf "Relationship '%s' on type '%s' is not nullable" relName resType
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
    |> Error.setDetailf "Expected a resource object with type '%s', but got '%s'" expectedType invalidType
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredId pointer =
    Error.create 400
    |> Error.setTitle "Missing resource ID"
    |> Error.setDetail "This operation requires a resource ID"
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredAttr attrName pointer =
    Error.create 400
    |> Error.setTitle "Missing required attribute"
    |> Error.setDetailf "Attribute '%s' is required for this operation" attrName
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredRel relName pointer =
    Error.create 400
    |> Error.setTitle "Missing required relationship"
    |> Error.setDetailf "Relationship '%s' is required for this operation" relName
    |> Error.setSourcePointer pointer

  let reqParserMissingIncludedResource resType resId pointer =
    Error.create 400
    |> Error.setTitle "Missing included resource"
    |> Error.setDetailf "Expected to find referenced resource with type '%s' and ID '%s' in the 'included' part of the request" resType resId
    |> Error.setSourcePointer pointer

  let reqParserMissingRequiredHeader headerName =
    Error.create 400
    |> Error.setTitle "Missing required header"
    |> Error.setDetailf "HTTP header '%s' is required for this operation" headerName

  let reqParserMissingRequiredQueryParam paramName =
    Error.create 400
    |> Error.setTitle "Missing required query parameter"
    |> Error.setDetailf "Query parameter '%s' is required for this operation" paramName
    |> Error.setSourceParam paramName

  let reqParserProhibitedAttr attrName pointer =
    Error.create 400
    |> Error.setTitle "Attribute not allowed"
    |> Error.setDetailf "Attribute '%s' is not allowed for this operation" attrName
    |> Error.setSourcePointer pointer

  let reqParserProhibitedRel relName pointer =
    Error.create 400
    |> Error.setTitle "Relationship not allowed"
    |> Error.setDetailf "Relationship '%s' is not allowed for this operation" relName
    |> Error.setSourcePointer pointer

  let reqParserProhibitedQueryParam paramName =
    Error.create 400
    |> Error.setTitle "Query parameter not allowed"
    |> Error.setDetailf "Query parameter '%s' is not allowed for this operation" paramName
    |> Error.setSourceParam paramName

  let reqParserProhibitedHeader headerName =
    Error.create 400
    |> Error.setTitle "Header not allowed"
    |> Error.setDetailf "Header '%s' is not allowed for this operation" headerName


  (*
   * Any request: Parsing
  *)

  // Must also make sense when used in a filter query parameter
  let attrInvalidParsedNone attrName =
    Error.create 400
    |> Error.setTitle "Invalid attribute value"
    |> Error.setDetailf "This is not a valid value for attribute '%s'" attrName

  // Must also make sense when used in a filter query parameter
  let attrInvalidParsedErrMsg attrName errMsg =
    Error.create 400
    |> Error.setTitle "Invalid attribute value"
    |> Error.setDetailf "This is not a valid value for attribute '%s': %s" attrName errMsg

  // Must also make sense when used in a filter query parameter
  let attrInvalidEnum attrName invalidValue allowedValues =
    Error.create 400
    |> Error.setTitle "Invalid attribute value"
    |> match allowedValues with
       | [x] -> Error.setDetailf "Value '%s' is not valid for attribute '%s'; expected '%s'" invalidValue attrName x
       | xs -> Error.setDetailf "Value '%s' is not valid for attribute '%s'; expected one of %s" invalidValue attrName (xs |> List.map (sprintf "'%s'") |> String.concat ", ")

  // Must also make sense when used in a filter query parameter
  let idInvalidParsedNone =
    Error.create 400
    |> Error.setTitle "Invalid ID"
    |> Error.setDetail "This is not a valid value for this resource ID"

  // Must also make sense when used in a filter query parameter
  let idInvalidErrMsg errMsg =
    Error.create 400
    |> Error.setTitle "Invalid ID"
    |> Error.setDetailf "This is not a valid value for this resource ID: %s" errMsg

  let queryInvalidEnum paramName invalidValue allowedValues =
    // This error is not specific to sorting, but it's used there, so this is relevant:
    //
    // "If the server does not support sorting as specified in the query parameter sort,
    // it MUST return 400 Bad Request."
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> match allowedValues with
       | [x] -> Error.setDetailf "Query parameter '%s' got invalid value '%s'; expected '%s'" paramName invalidValue x
       | xs -> Error.setDetailf "Query parameter '%s' got invalid value '%s'; expected one of %s" paramName invalidValue (xs |> List.map (sprintf "'%s'") |> String.concat ", ")
    |> Error.setSourceParam paramName

  let queryInvalidEnumUnnamed invalidValue allowedValues =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> match allowedValues with
       | [x] -> Error.setDetailf "Query parameter got invalid value '%s'; expected '%s'" invalidValue x
       | xs -> Error.setDetailf "Query parameter got invalid value '%s'; expected one of %s" invalidValue (xs |> List.map (sprintf "'%s'") |> String.concat ", ")

  let queryInvalidInt paramName invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Query parameter '%s' expected an integer, but got '%s'" paramName invalidValue
    |> Error.setSourceParam paramName

  let queryIntTooSmall paramName invalidValue minValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Query parameter '%s' has minimum value %i, but got %i" paramName minValue invalidValue
    |> Error.setSourceParam paramName

  let queryIntTooLarge paramName invalidValue maxValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Query parameter '%s' has maximum value %i, but got %i" paramName maxValue invalidValue
    |> Error.setSourceParam paramName

  let queryInvalidIntUnnamed invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Expected an integer, but got '%s'" invalidValue

  let queryInvalidFloatUnnamed invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Expected a number, but got '%s'" invalidValue

  let queryInvalidParsedNone queryParamName invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Query parameter '%s' got invalid value '%s'" queryParamName invalidValue

  let queryInvalidParsedNoneUnnamed invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "The value '%s' is not valid" invalidValue

  let queryInvalidParsedErrMsg queryParamName invalidValue errMsg =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Query parameter '%s' got invalid value '%s': %s" queryParamName invalidValue errMsg

  let queryInvalidParsedErrMsgUnnamed invalidValue errMsg =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "The value '%s' is not valid: %s" invalidValue errMsg

  let queryNotSingular paramName numValues =
    Error.create 400
    |> Error.setTitle "Multiple values not allowed"
    |> Error.setDetailf "Query parameter '%s' only accepts a single value, but got %i comma-separated values" paramName numValues
    |> Error.setSourceParam paramName

  let headerInvalidParsedNone headerName invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid header value"
    |> Error.setDetailf "Header '%s' got invalid value '%s'" headerName invalidValue

  let headerInvalidParsedErrMsg headerName invalidValue errMsg =
    Error.create 400
    |> Error.setTitle "Invalid header value"
    |> Error.setDetailf "Header '%s' got invalid value '%s': %s" headerName invalidValue errMsg


  (*
   * Field setters (relevant for POST collection and PATCH resource)
  *)

  let setAttrReadOnly attrName pointer =
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    Error.create 403
    |> Error.setTitle "Attribute read-only"
    |> Error.setDetailf "Attribute '%s' is read-only" attrName
    |> Error.setSourcePointer pointer

  let setAttrNullNotAllowed attrName =
    Error.create 403
    |> Error.setTitle "Null not allowed"
    |> Error.setDetailf "Attribute '%s' may not be set to null" attrName

  let setRelReadOnly relName pointer =
    // "A server MUST return 403 Forbidden in response to an unsupported request to
    // update a resource or relationship."
    Error.create 403
    |> Error.setTitle "Relationship read-only"
    |> Error.setDetailf "Relationship '%s' is read-only" relName
    |> Error.setSourcePointer pointer

  let setRelNullNotAllowed relName =
    Error.create 403
    |> Error.setTitle "Null not allowed"
    |> Error.setDetailf "Relationship '%s' may not be set to null" relName

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
    |> Error.setDetailf "Relationship '%s' on type '%s' does not support complete replacement using PATCH%s" relName resType extraMessage
    |> Error.setSourcePointer pointer


  (*
   * GET collection
  *)

  let collGetNotAllowed collName =
    Error.create 403
    |> Error.setTitle "GET collection not allowed"
    |> Error.setDetailf "Collection '%s' does not support fetching resources" collName

  let sortNotSupported =
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
    |> Error.setDetailf "Collection '%s' does not support creating resources" collName

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
    |> Error.setDetailf "Collection '%s' does not support creating resource type '%s' with a client-generated ID" collName resType
    |> Error.setSourcePointer "/data/id"

  let collPostTypeNotAllowed collName invalidType allowedTypes pointer =
    // "A server MUST return 409 Conflict when processing a POST request in which the
    // resource object’s type is not among the type(s) that constitute the collection
    // represented by the endpoint."
    Error.create 409
    |> Error.setTitle "Invalid resource type"
    |> match allowedTypes with
       | [x] -> Error.setDetailf "Collection '%s' does not support creating resources with type '%s'; expected '%s'" collName invalidType x
       | xs -> Error.setDetailf "Collection '%s' does not support creating resources with type '%s'; expected one of %s" collName invalidType (xs |> List.map (sprintf "'%s'") |> String.concat ", ")
    |> Error.setSourcePointer pointer


  (*
   * Resource lookup (and any resource-specific operation)
  *)

  let collLookupNotSupported collName =
    /// "A server MUST return 403 Forbidden in response to an unsupported request to
    /// update a resource or relationship."
    Error.create 403
    |> Error.setTitle "Resource operations not supported"
    |> Error.setDetailf "Collection '%s' does not support any resource-specific operations" collName


  (*
   * GET resource
  *)

  let resGetNotSupportedPolymorphic resType collName =
    Error.create 403
    |> Error.setTitle "GET not supported"
    |> Error.setDetailf "GET is not supported for resource type '%s' (it may be supported for other resource types in collection '%s')" resType collName

  let resGetNotSupportedForAnyResource collName =
    Error.create 403
    |> Error.setTitle "GET not supported"
    |> Error.setDetailf "GET is not supported for any resource in collection '%s'" collName

  let resourceNotFound collName resId =
    Error.create 404
    |> Error.setTitle "Resource not found"
    |> Error.setDetailf "Collection '%s' does not contain a resource with ID '%s'" collName resId


  (*
   * PATCH resource
  *)

  let resPatchNotSupportedPolymorphic resType collName =
    // A server MUST return 403 Forbidden in response to an unsupported request to update
    // a resource or relationship.
    Error.create 403
    |> Error.setTitle "PATCH not supported"
    |> Error.setDetailf "PATCH is not supported for resource type '%s' (it may be supported for other resource types in collection '%s')" resType collName

  let resPatchNotSupportedForAnyResource collName =
    // A server MUST return 403 Forbidden in response to an unsupported request to update
    // a resource or relationship.
    Error.create 403
    |> Error.setTitle "PATCH not supported"
    |> Error.setDetailf "PATCH is not supported for any resource in collection '%s'" collName

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
    |> Error.setDetailf "Expected resource type '%s', but got '%s'" expectedType invalidType

  let resPatchIdMismatch invalidId expectedId =
    // "A server MUST return 409 Conflict when processing a PATCH request in which the
    // resource object’s type and id do not match the server’s endpoint."
    Error.create 409
    |> Error.setTitle "Incorrect ID"
    |> Error.setDetailf "Expected resource ID '%s', but got '%s'" expectedId invalidId


  (*
   * DELETE resource
  *)

  let resDeleteNotSupportedPolymorphic resType collName =
    Error.create 403  // 403 corresponds to unsupported PATCH
    |> Error.setTitle "DELETE not supported"
    |> Error.setDetailf "DELETE is not supported for resource type '%s' (it may be supported for other resource types in collection '%s')" resType collName

  let resDeleteNotSupportedForAnyResource collName =
    Error.create 403  // 403 corresponds to unsupported PATCH
    |> Error.setTitle "DELETE not supported"
    |> Error.setDetailf "DELETE is not supported for any resource in collection '%s'" collName


  (*
   * Any relationship self/related request
  *)

  let relNotDefinedPolymorphic relName resType collName =
    // "A server MUST return 404 Not Found when processing a request to fetch a
    // relationship link URL that does not exist."
    Error.create 404
    |> Error.setTitle "Relationship does not exist"
    |> Error.setDetailf "Relationship '%s' is not defined for resource type '%s' (other resource types in collection '%s' may have a relationship called '%s')" relName resType collName relName

  (*
   * Any relationship self request
  *)

  let relSelfIncludeNotSupported =
    // "If a server is unable to identify a relationship path or does not support
    // inclusion of resources from a path, it MUST respond with 400 Bad Request."
    Error.create 400
    |> Error.setTitle "Include not supported"
    |> Error.setDetail "Included resources are not currently supported for relationship self links"
    |> Error.setSourceParam "include"


  (*
   * GET relationship self or related
  *)

  let getRelNotDefinedPolymorphic relName resType collName =
    Error.create 403
    |> Error.setTitle "GET relationship not supported"
    |> Error.setDetailf "Relationship '%s' on type '%s' is not readable (other resource types in collection '%s' may have a readable relationship called '%s')" relName resType collName relName

  let getRelNotDefinedForAnyResource relName collName =
    Error.create 403
    |> Error.setTitle "GET relationship not supported"
    |> Error.setDetailf "Relationship '%s' is not readable for any resource in collection '%s'" relName collName

  let getRelWhileSkip =
    Error.create 403
    |> Error.setTitle "No relationship value"
    |> Error.setDetail "The server has chosen not to disclose the value of this relationship"


  (*
   * Any request updating or parsing relationships
  *)

  let relMissingData relName pointer =
    Error.create 400
    |> Error.setTitle "Missing relationship data"
    |> Error.setDetailf "Relationship '%s' was specified without relationship data" relName
    |> Error.setSourcePointer pointer

  let relInvalidType relName invalidType allowedTypes pointer =
    Error.create 409  // 409 corresponds to PATCH type mismatch
    |> Error.setTitle "Invalid relationship type"
    |> match allowedTypes with
       | [x] -> Error.setDetailf "Relationship '%s' contains data with invalid type '%s'; expected '%s'" relName invalidType x
       | xs -> Error.setDetailf "Relationship '%s' contains data with invalid type '%s'; expected one of %s" relName invalidType (xs |> List.map (sprintf "'%s'") |> String.concat ", ")
    |> Error.setSourcePointer pointer


  (*
   * PATCH/POST/DELETE relationship self
  *)

  let relInvalidTypeSelf invalidType allowedTypes pointer =
    Error.create 409  // 409 corresponds to PATCH type mismatch
    |> Error.setTitle "Invalid relationship type"
    |> match allowedTypes with
       | [x] -> Error.setDetailf "Data contains invalid type '%s'; expected '%s'" invalidType x
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
    |> Error.setDetailf "Relationship '%s' on type '%s' does not support complete replacement using PATCH%s (other resource types in collection '%s' may have a relationship called '%s' that supports PATCH)" relName resType extraMessage collName relName

  let patchRelSelfSettableButNotGettablePolymorphic relName resType collName =
    Error.create 403
    |> Error.setTitle "PATCH relationship not supported"
    |> Error.setDetailf "Relationship '%s' on type '%s' is write-only and may only be updated through PATCH requests to the parent resource (other resource types in collection '%s' may have a relationship called '%s' that supports this operation)" relName resType collName relName

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
    |> Error.setDetailf "Relationship '%s' does not support PATCH for any resource in collection '%s'%s" relName collName extraMessage

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
    |> Error.setDetailf "Relationship '%s' on type '%s' does not support adding members using POST%s (other resource types in collection '%s' may have a relationship called '%s' that supports POST)" relName resType extraMessage collName relName

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
    |> Error.setDetailf "Relationship '%s' does not support adding members using POST for any resource in collection '%s'%s" relName collName extraMessage

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
    |> Error.setDetailf "Relationship '%s' on type '%s' does not support removing members using DELETE%s (other resource types in collection '%s' may have a relationship called '%s' that supports DELETE)" relName resType extraMessage collName relName

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
    |> Error.setDetailf "Relationship '%s' does not support removing members using DELETE for any resource in collection '%s'%s" relName collName extraMessage

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
    |> Error.setDetailf "Relationship '%s' on type '%s' is read-only" relName resType
  
  let relatedResourceNotFound pointer =
    // "A server MUST return 404 Not Found when processing a request that references a
    // related resource that does not exist."
    Error.create 404
    |> Error.setTitle "Resource not found"
    |> Error.setDetail "The related resource does not exist"
    |> Error.setSourcePointer pointer

  let relModifySelfWhileSkip =
    Error.create 500
    |> Error.setTitle "No relationship value"
    |> Error.setDetail "The relationship was updated, but the server has erroneously chosen not to disclose the value of the updated relationship"


  (*
   * Custom operations (links)
  *)

  let customOpConditionFalse =
    Error.create 403
    |> Error.setTitle "Operation not available"
    |> Error.setDetail "This operation is currently not available"

  let customOpNotDefinedPolymorphic linkName resType collName =
    Error.create 404
    |> Error.setTitle "Operation does not exist"
    |> Error.setDetailf "Operation '%s' is not defined for resource type '%s' (it may exist for other resource types in collection '%s')" linkName resType collName

  let customOpVerbNotDefinedPolymorphic linkName resType invalidVerb allowHeader collName =
    Error.create 405
    |> Error.setTitle "Method not allowed"
    |> Error.setDetailf "Operation '%s' on type '%s' does not support %s (other resource types in collection '%s' may have an operation '%s' supporting %s)" linkName resType invalidVerb collName linkName invalidVerb
    |> Error.addHeader "Allow" allowHeader

  let customOpVerbNotDefinedForAnyResource linkName invalidVerb collName allowHeader =
    Error.create 405
    |> Error.setTitle "Method not allowed"
    |> Error.setDetailf "Operation '%s' does not support %s for any resource in collection '%s'" linkName invalidVerb collName
    |> Error.addHeader "Allow" allowHeader


  (*
   * Misc.
  *)

  let linkOrRelationshipDoesNotExistForAnyResource linkOrRelName collName =
    Error.create 404
    |> Error.setTitle "Unknown link or relationship"
    |> Error.setDetailf "Link or relationship '%s' does not exist for any resource in collection '%s'" linkOrRelName collName

  let methodNotAllowed method allowedHeaderValue =
    Error.create 405
    |> Error.setTitle "Method not allowed"
    |> Error.setDetailf "%s is not a known method for this endpoint" method
    |> Error.addHeader "Allow" allowedHeaderValue
