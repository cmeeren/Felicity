namespace Felicity

open System


module Constants =

  /// The JSON:API media type: application/vnd.api+json
  let [<Literal>] jsonApiMediaType = "application/vnd.api+json"


  let internal polymorphicTypeName = "bbb60ec990624ea1bcf10e12e5fab50d"


type Request = internal {
  Document: Lazy<Result<ResourceDocument option, Error list>>
  IdentifierDocument: Lazy<Result<ResourceIdentifierDocument option, Error list>>
  IdentifierCollectionDocument: Lazy<Result<ResourceIdentifierCollectionDocument option, Error list>>
  Headers: Map<string, string>
  Query: Map<string, string>
  Fieldsets: Map<ResourceTypeName, Set<FieldName>>
  Includes: RelationshipName list list
}


type RequestGetter<'ctx, 'a> =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract Get: 'ctx * Request -> Async<Result<'a, Error list>>

type OptionalRequestGetter<'ctx, 'a> =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract Get: 'ctx * Request -> Async<Result<'a option, Error list>>

type ProhibitedRequestGetter =
  abstract FieldName: FieldName option
  abstract QueryParamName: QueryParamName option
  abstract GetErrors: Request -> Error list


type internal Field<'ctx> =
  abstract Name: string

type internal BoxedPatcher<'ctx> = 'ctx -> Request -> Set<ConsumedFieldName> -> BoxedEntity -> Async<Result<BoxedEntity, Error list>>
