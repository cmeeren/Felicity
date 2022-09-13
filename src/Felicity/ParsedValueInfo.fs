namespace Felicity


type ParsedValueFromQueryData = {
  Name: QueryParamName
  Value: string
  NumValues: int
  ValueIndex: int
}

type ParsedValueFromHeaderData = {
  Name: HeaderName
  Value: string
}

type ParsedValueFromAttributeData = {
  Name: AttributeName
}

type ParsedValueFromRelationshipIdData = {
  Name: RelationshipName
  Value: ResourceId
}

type ParsedValueFromIdData = {
  Value: ResourceId
}

type ParsedValueInfo =
  | FromQuery of ParsedValueFromQueryData
  | FromHeader of ParsedValueFromHeaderData
  | FromBodyAttribute of ParsedValueFromAttributeData
  | FromBodyId of ParsedValueFromIdData
