namespace Felicity

open Microsoft.AspNetCore.Http


module internal RequestValidation =

  /// Indicates if the client accepts the JSON:API media type (if Accept contains
  /// application/vnd.api+json or */*).
  let acceptsJsonApi (ctx: HttpContext) =
    ctx.Request.GetTypedHeaders().Accept
    |> Option.ofObj
    |> Option.map List.ofSeq
    |> Option.defaultValue []
    |> Seq.exists (fun x ->
        x.MediaType.Value = Constants.jsonApiMediaType
        || x.MediaType.Value = "*/*")


  /// Returns true if Content-Type is present and not equal to application/vnd.api+json.
  let hasNonJsonApiContent (ctx: HttpContext) =
    let cType = ctx.Request.GetTypedHeaders().ContentType
    not <| isNull cType && cType.MediaType.Value <> Constants.jsonApiMediaType


  /// Returns true if Content-Type is application/vnd.api+json and it is modified
  /// with media type parameters. According to the JSON:API specification, the server
  /// then MUST return 415 Unsupported Media Type.
  let jsonApiContentTypeHasParams (ctx: HttpContext) =
    let headers = ctx.Request.GetTypedHeaders()
    not <| isNull headers.ContentType
    && headers.ContentType.MediaType.Value = Constants.jsonApiMediaType
    && headers.ContentType.Parameters.Count > 0


  /// Returns true if all JSON:API media types in the Accept header are modified
  /// with media type parameters. According to the JSON:API specification, the
  /// server then MUST return 406 Not Acceptable.
  let allJsonApiAcceptsHaveParams (ctx: HttpContext) =
    let headers = ctx.Request.GetTypedHeaders()
    let jsonApiAccepts =
      headers.Accept
      |> Option.ofObj
      |> Option.map List.ofSeq
      |> Option.defaultValue []
      |> List.filter (fun x -> x.MediaType.Value = Constants.jsonApiMediaType)
    not jsonApiAccepts.IsEmpty
    && jsonApiAccepts |> List.forall (fun x -> x.Parameters.Count > 0)


  /// Returns a list of all query string parameter names present in the request
  /// that are illegal according to the JSON:API specification. If the returned
  /// list is not empty, then according to the JSON:API specification, the server
  /// MUST respond with 400 Bad Request.
  let getIllegalQueryStringParams (ctx: HttpContext) =
    ctx.Request.Query
    |> Seq.map (fun kvp -> kvp.Key)
    |> Seq.filter (not << QueryParam.isValidName)
    |> Seq.toList
