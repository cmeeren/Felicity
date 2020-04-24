namespace Felicity

open System
open Hopac
open Errors


[<AutoOpen>]
module private QueryParseHelpers =

  let parseBool = function
    | "true" -> Ok true
    | "false" -> Ok false
    | invalid -> Error [queryInvalidEnumUnnamed invalid ["true"; "false"]]

  let parseInt (str: string) =
    match Int32.TryParse str with
    | true, x -> Ok x
    | false, _ -> Error [queryInvalidIntUnnamed str]

  let parseFloat (str: string) =
    match Double.TryParse str with
    | true, x -> Ok x
    | false, _ -> Error [queryInvalidFloatUnnamed str]



type ListFilter<'ctx, 'a> internal (fieldName: string, parse: 'ctx -> string -> Job<Result<'a, Error list>>, ?operator: string) =

  let addOperator s =
    match operator with
    | None -> s
    | Some op -> s + "[" + op + "]"

  let queryParamName = "filter[" + fieldName + "]" |> addOperator

  member _.Optional =
    { new RequestGetter<'ctx, 'a list option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Job.result
          | true, "" ->  Ok (Some []) |> Job.result
          | true, str ->
              str.Split(',')
              |> Array.toList
              |> List.traverseJobResultA (
                  parse ctx
                  >> JobResult.mapError (List.map (Error.setSourceParam queryParamName))
              )
              |> JobResult.map Some
    }

  interface OptionalRequestGetter<'ctx, 'a list> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'a list> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]


  member _.Operator(operator) =
    ListFilter<'ctx, 'a>(fieldName, parse, operator)

  member _.Bool =
    ListFilter<'ctx, bool>(fieldName, (fun _ s -> parseBool s |> Job.result), ?operator=operator)



type SingleFilter<'ctx, 'a> internal (fieldName: string, parse: 'ctx -> string -> Job<Result<'a, Error list>>, ?operator: string) =

  let addOperator s =
    match operator with
    | None -> s
    | Some op -> s + "[" + op + "]"

  let queryParamName = "filter[" + fieldName + "]" |> addOperator

  member _.Optional =
    { new RequestGetter<'ctx, 'a option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Job.result
          | true, str ->
              if str.Contains "," then
                let numElements = str.Split(',').Length
                Error [queryNotSingular queryParamName numElements] |> Job.result
              else
                parse ctx str
                |> JobResult.mapError (List.map (Error.setSourceParam queryParamName))
                |> JobResult.map Some
    }

  interface OptionalRequestGetter<'ctx, 'a> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'a> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]

  member _.Operator(operator) =
    SingleFilter<'ctx, 'a>(fieldName, parse, operator)

  member _.List =
    ListFilter<'ctx, 'a>(fieldName, parse, ?operator=operator)

  member _.Bool =
    SingleFilter<'ctx, bool>(fieldName, (fun _ s -> parseBool s |> Job.result), ?operator=operator)


type ListSort<'ctx, 'a> internal (parse: 'ctx -> string -> Job<Result<'a, Error list>>) =

  let queryParamName = "sort"
  
  member _.Optional =
    { new RequestGetter<'ctx, ('a * bool) list option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Job.result
          | true, "" -> Ok (Some []) |> Job.result
          | true, str ->
              str.Split(',')
              |> Array.toList
              |> List.traverseJobResultA (fun str ->
                  let sort, isDescending =
                    if str.StartsWith("-")
                    then str.Substring(1), true
                    else str, false
                  parse ctx sort
                  |> JobResult.mapError (List.map (Error.setSourceParam queryParamName))
                  |> JobResult.map (fun s -> s, isDescending)
                )
              |> JobResult.map Some
    }

  interface OptionalRequestGetter<'ctx, ('a * bool) list> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, ('a * bool) list> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]



type SingleSort<'ctx, 'a> internal (parse: 'ctx -> string -> Job<Result<'a, Error list>>) =

  let queryParamName = "sort"

  member _.Optional =
    { new RequestGetter<'ctx, ('a * bool) option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Job.result
          | true, str ->
              if str.Contains "," then
                let numElements = str.Split(',').Length
                Error [queryNotSingular queryParamName numElements] |> Job.result
              else
                let sort, isDescending =
                  if str.StartsWith("-")
                  then str.Substring(1), true
                  else str, false
                parse ctx sort
                |> JobResult.mapError (List.map (Error.setSourceParam queryParamName))
                |> JobResult.map (fun s -> Some (s, isDescending))
    }

  interface OptionalRequestGetter<'ctx, ('a * bool)> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, ('a * bool)> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]

  member _.List =
    ListSort<'ctx, 'a>(parse)


type PageParam<'ctx> internal (pageName: string, ?min: int , ?max: int) =

  let queryParamName = "page[" + pageName + "]"

  member _.Optional =
    { new RequestGetter<'ctx, int option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Job.result
          | true, i ->
              match Int32.TryParse i with
              | false, _ -> Error [queryInvalidInt queryParamName i] |> Job.result
              | true, i ->
                  match min, max with
                  | Some min, _ when i < min -> Error [queryIntTooSmall queryParamName i min] |> Job.result
                  | _, Some max when i > max -> Error [queryIntTooLarge queryParamName i max] |> Job.result
                  | _ -> Ok (Some i) |> Job.result
    }

  interface OptionalRequestGetter<'ctx, int> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, int> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]

  member _.Min(minValue: int) =
    PageParam<'ctx>(pageName, min = minValue, ?max = max)

  member _.Max(maxValue: int) =
    PageParam<'ctx>(pageName, ?min = min, max = maxValue)


type CustomQueryParam<'ctx, 'a> internal (queryParamName, parse: 'ctx -> string -> Job<Result<'a, Error list>>) =

  member _.Optional =
    { new RequestGetter<'ctx, 'a option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Job.result
          | true, str ->
              parse ctx str
              |> JobResult.mapError (List.map (Error.setSourceParam queryParamName))
              |> JobResult.map Some
    }

  interface OptionalRequestGetter<'ctx, 'a> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'a> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]


type Header<'ctx, 'a> internal (headerName: string, parse: 'ctx -> string -> Job<Result<'a, Error list>>) =

  member _.Optional =
    { new RequestGetter<'ctx, 'a option> with
        member _.FieldName = None
        member _.QueryParamName = None
        member _.Get(ctx, req, _) =
          match req.Headers.TryGetValue headerName with
          | false, _ -> Ok None |> Job.result
          | true, str -> parse ctx str |> JobResult.map Some
    }

  interface OptionalRequestGetter<'ctx, 'a> with
    member _.FieldName = None
    member _.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'a> with
    member _.FieldName = None
    member _.QueryParamName = None
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> JobResult.requireSome [reqParserMissingRequiredHeader headerName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = None
    member _.GetErrors(req, _) =
      match req.Headers.TryGetValue headerName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedHeader headerName]



type Filter =

  static member Field(id: Id<'ctx, 'entity, 'id>) =
    SingleFilter<'ctx, 'id>("id", fun ctx str -> id.toDomain ctx str)

  static member Field(attribute: NonNullableAttribute<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(attribute: NonNullableAttribute<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(attribute: NullableAttribute<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(attribute: NullableAttribute<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(attribute: NonNullableAttribute<'ctx, 'entity, 'attr, string>) =
    Filter.Field(attribute, Ok)

  static member Field(attribute: NonNullableAttribute<'ctx, 'entity, 'attr, bool>) =
    Filter.Field(attribute, parseBool)

  static member Field(attribute: NonNullableAttribute<'ctx, 'entity, 'attr, int>) =
    Filter.Field(attribute, parseInt)

  static member Field(attribute: NonNullableAttribute<'ctx, 'entity, 'attr, float>) =
    Filter.Field(attribute, parseFloat)

  static member FieldAsNonNullable(attribute: NullableAttribute<'ctx, 'entity, 'attr, string>) =
    Filter.FieldAsNonNullable(attribute, Ok)

  static member FieldAsNonNullable(attribute: NullableAttribute<'ctx, 'entity, 'attr, bool>) =
    Filter.FieldAsNonNullable(attribute, parseBool)

  static member FieldAsNonNullable(attribute: NullableAttribute<'ctx, 'entity, 'attr, int>) =
    Filter.FieldAsNonNullable(attribute, parseInt)

  static member FieldAsNonNullable(attribute: NullableAttribute<'ctx, 'entity, 'attr, float>) =
    Filter.FieldAsNonNullable(attribute, parseFloat)

  static member Field(relationship: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>) =
    SingleFilter<'ctx, 'relatedId>(relationship.Name, relationship.IdToDomain)

  static member Field(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NonNullableAttribute<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NonNullableAttribute<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NullableAttribute<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NullableAttribute<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NonNullableAttribute<'ctx, 'relatedEntity, 'attr, string>) =
    Filter.Field(path, attribute, Ok)

  static member Field(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NonNullableAttribute<'ctx, 'relatedEntity, 'attr, bool>) =
    Filter.Field(path, attribute, parseBool)

  static member Field(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NonNullableAttribute<'ctx, 'relatedEntity, 'attr, int>) =
    Filter.Field(path, attribute, parseInt)

  static member Field(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NonNullableAttribute<'ctx, 'relatedEntity, 'attr, float>) =
    Filter.Field(path, attribute, parseFloat)

  static member FieldAsNonNullable(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NullableAttribute<'ctx, 'relatedEntity, 'attr, string>) =
    Filter.FieldAsNonNullable(path, attribute, Ok)

  static member FieldAsNonNullable(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NullableAttribute<'ctx, 'relatedEntity, 'attr, bool>) =
    Filter.FieldAsNonNullable(path, attribute, parseBool)

  static member FieldAsNonNullable(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NullableAttribute<'ctx, 'relatedEntity, 'attr, int>) =
    Filter.FieldAsNonNullable(path, attribute, parseInt)

  static member FieldAsNonNullable(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, attribute: NullableAttribute<'ctx, 'relatedEntity, 'attr, float>) =
    Filter.FieldAsNonNullable(path, attribute, parseFloat)

  static member Field(path: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity, 'relatedId>, relationship: RelationshipQueryIdParser<'ctx, 'relatedEntity, 'relatedEntity2, 'relatedId2>) =
    SingleFilter<'ctx, 'relatedId2>(path.Name + "." + relationship.Name, relationship.IdToDomain)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NonNullableAttribute<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NonNullableAttribute<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str]  |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NullableAttribute<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NullableAttribute<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NonNullableAttribute<'ctx, 'relatedEntity2, 'attr, string>) =
    Filter.Field(path1, path2, attribute, Ok)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NonNullableAttribute<'ctx, 'relatedEntity2, 'attr, bool>) =
    Filter.Field(path1, path2, attribute, parseBool)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NonNullableAttribute<'ctx, 'relatedEntity2, 'attr, int>) =
    Filter.Field(path1, path2, attribute, parseInt)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NonNullableAttribute<'ctx, 'relatedEntity2, 'attr, float>) =
    Filter.Field(path1, path2, attribute, parseFloat)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NullableAttribute<'ctx, 'relatedEntity2, 'attr, string>) =
    Filter.FieldAsNonNullable(path1, path2, attribute, Ok)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NullableAttribute<'ctx, 'relatedEntity2, 'attr, bool>) =
    Filter.FieldAsNonNullable(path1, path2, attribute, parseBool)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NullableAttribute<'ctx, 'relatedEntity2, 'attr, int>) =
    Filter.FieldAsNonNullable(path1, path2, attribute, parseInt)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, attribute: NullableAttribute<'ctx, 'relatedEntity2, 'attr, float>) =
    Filter.FieldAsNonNullable(path1, path2, attribute, parseFloat)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, relationship: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>) =
    SingleFilter<'ctx, 'relatedId3>(path1.Name + "." + path2.Name + "." + relationship.Name, relationship.IdToDomain)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NonNullableAttribute<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NonNullableAttribute<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NullableAttribute<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NullableAttribute<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + attribute.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (attribute.toDomain ctx))

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NonNullableAttribute<'ctx, 'relatedEntity3, 'attr, string>) =
    Filter.Field(path1, path2, path3, attribute, Ok)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NonNullableAttribute<'ctx, 'relatedEntity3, 'attr, bool>) =
    Filter.Field(path1, path2, path3, attribute, parseBool)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NonNullableAttribute<'ctx, 'relatedEntity3, 'attr, int>) =
    Filter.Field(path1, path2, path3, attribute, parseInt)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NonNullableAttribute<'ctx, 'relatedEntity3, 'attr, float>) =
    Filter.Field(path1, path2, path3, attribute, parseFloat)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NullableAttribute<'ctx, 'relatedEntity3, 'attr, string>) =
    Filter.FieldAsNonNullable(path1, path2, path3, attribute, Ok)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NullableAttribute<'ctx, 'relatedEntity3, 'attr, bool>) =
    Filter.FieldAsNonNullable(path1, path2, path3, attribute, parseBool)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NullableAttribute<'ctx, 'relatedEntity3, 'attr, int>) =
    Filter.FieldAsNonNullable(path1, path2, path3, attribute, parseInt)

  static member FieldAsNonNullable(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, attribute: NullableAttribute<'ctx, 'relatedEntity3, 'attr, float>) =
    Filter.FieldAsNonNullable(path1, path2, path3, attribute, parseFloat)

  static member Field(path1: RelationshipQueryIdParser<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: RelationshipQueryIdParser<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: RelationshipQueryIdParser<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, relationship: RelationshipQueryIdParser<'ctx, 'relatedEntity3, 'relatedEntity4, 'relatedId4>) =
    SingleFilter<'ctx, 'relatedId4>(path1.Name + "." + path2.Name + "." + path3.Name + "." + relationship.Name, relationship.IdToDomain)

  static member ParsedJobRes(name, parse: 'ctx -> string -> Job<Result<'a, Error list>>) =
    SingleFilter<'ctx, 'a>(name, parse)

  static member ParsedJobRes(name, parse: string -> Job<Result<'a, Error list>>) =
    Filter.ParsedJobRes(name, fun _ s -> parse s)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Filter.ParsedJobRes(name, Job.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, Error list>>) =
    Filter.ParsedJobRes(name, Job.liftAsync parse)

  static member ParsedJobOpt(name, parse: 'ctx -> string -> Job<'a option>) =
    Filter.ParsedJobRes(name, fun ctx s -> parse ctx s |> Job.map (Result.requireSome [queryInvalidParsedNoneUnnamed s]))

  static member ParsedJobOpt(name, parse: string -> Job<'a option>) =
    Filter.ParsedJobRes(name, fun _ s -> parse s |> Job.map (Result.requireSome [queryInvalidParsedNoneUnnamed s]))

  static member ParsedAsyncOpt(name, parse: 'ctx -> string -> Async<'a option>) =
    Filter.ParsedJobOpt(name, Job.liftAsync2 parse)

  static member ParsedAsyncOpt(name, parse: string -> Async<'a option>) =
    Filter.ParsedJobOpt(name, Job.liftAsync parse)

  static member ParsedJob(name, parse: 'ctx -> string -> Job<'a>) =
    Filter.ParsedJobRes(name, fun ctx s -> parse ctx s |> Job.map Ok)

  static member ParsedJob(name, parse: string -> Job<'a>) =
    Filter.ParsedJobRes(name, fun _ s -> parse s |> Job.map Ok)

  static member ParsedAsync(name, parse: 'ctx -> string -> Async<'a>) =
    Filter.ParsedJob(name, Job.liftAsync2 parse)

  static member ParsedAsync(name, parse: string -> Async<'a>) =
    Filter.ParsedJob(name, Job.liftAsync parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, Error list>) =
    Filter.ParsedJobRes(name, Job.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, Error list>) =
    Filter.ParsedJobRes(name, Job.lift parse)

  static member ParsedOpt(name, parse: 'ctx -> string -> 'a option) =
    Filter.ParsedJobOpt(name, Job.lift2 parse)

  static member ParsedOpt(name, parse: string -> 'a option) =
    Filter.ParsedJobOpt(name, Job.lift parse)

  static member Parsed(name: string, parse: string -> 'a) =
    Filter.ParsedJobRes(name, JobResult.lift parse)




[<AutoOpen>]
module FilterExtensions =


  type Filter with

    static member Parsed(name: string, parse: 'ctx -> string -> 'a) =
      Filter.ParsedJobRes(name, JobResult.lift2 parse)



type Sort =

  static member ParsedJobRes(parse: 'ctx -> string -> Job<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    SingleSort<'ctx, 'a>(parse)

  static member ParsedJobRes(parse: string -> Job<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(fun _ s -> parse s)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(Job.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(Job.liftAsync parse)

  static member ParsedJobOpt(parse: 'ctx -> string -> Job<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(fun ctx s -> parse ctx s |> Job.map (Result.requireSome [queryInvalidParsedNone "sort" s]))

  static member ParsedJobOpt(parse: string -> Job<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobOpt(fun _ s -> parse s)

  static member ParsedAsyncOpt(parse: 'ctx -> string -> Async<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobOpt(Job.liftAsync2 parse)

  static member ParsedAsyncOpt(parse: string -> Async<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobOpt(Job.liftAsync parse)

  static member ParsedJob(parse: 'ctx -> string -> Job<'a>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(fun ctx s -> parse ctx s |> Job.map Ok)

  static member ParsedJob(parse: string -> Job<'a>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(fun _ s -> parse s |> Job.map Ok)

  static member ParsedAsync(parse: 'ctx -> string -> Async<'a>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJob(Job.liftAsync2 parse)

  static member ParsedAsync(parse: string -> Async<'a>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJob(Job.liftAsync parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, Error list>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(Job.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, Error list>) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(Job.lift parse)

  static member ParsedOpt(parse: 'ctx -> string -> 'a option) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobOpt(Job.lift2 parse)

  static member ParsedOpt(parse: string -> 'a option) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobOpt(Job.lift parse)

  static member Parsed(parse: string -> 'a) : SingleSort<'ctx, 'a> =
    Sort.ParsedJobRes(JobResult.lift parse)

  static member Enum(sortMap: (string * 'a) list) : SingleSort<'ctx, 'a> =
    let d = dict sortMap
    let allowed = sortMap |> List.map fst |> List.distinct
    let parseSort s =
      match d.TryGetValue s with
      | true, v -> Ok v
      | false, _ -> Error [queryInvalidEnum "sort" s allowed]
    SingleSort<'ctx, 'a>(fun _ s -> parseSort s |> Job.result)



[<AutoOpen>]
module SortExtensions =


  type Sort with

    static member Parsed(parse: 'ctx -> string -> 'a) : SingleSort<'ctx, 'a> =
      Sort.ParsedJobRes(JobResult.lift2 parse)



type Page<'ctx> =

  static member Offset = PageParam<'ctx>("offset", min=0)

  static member Limit = PageParam<'ctx>("limit", min=1)

  static member Number = PageParam<'ctx>("number", min=0)

  static member Size = PageParam<'ctx>("size", min=1)



type Query =

  static member ParsedJobRes(queryParamName, parse: 'ctx -> string -> Job<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, parse)

  static member ParsedJobRes(queryParamName, parse: string -> Job<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, Job.liftAsync parse)

  static member ParsedJobOpt(queryParamName, parse: 'ctx -> string -> Job<'a option>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, fun ctx s -> parse ctx s |> Job.map (Result.requireSome [queryInvalidParsedNone queryParamName s]))

  static member ParsedJobOpt(queryParamName, parse: string -> Job<'a option>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobOpt(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncOpt(queryParamName, parse: 'ctx -> string -> Async<'a option>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobOpt(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsyncOpt(queryParamName, parse: string -> Async<'a option>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobOpt(queryParamName, Job.liftAsync parse)

  static member ParsedJob(queryParamName, parse: 'ctx -> string -> Job<'a>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, fun ctx s -> parse ctx s |> Job.map Ok)

  static member ParsedJob(queryParamName, parse: string -> Job<'a>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, fun _ s -> parse s |> Job.map Ok)

  static member ParsedAsync(queryParamName, parse: 'ctx -> string -> Async<'a>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJob(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsync(queryParamName, parse: string -> Async<'a>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJob(queryParamName, Job.liftAsync parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, Error list>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, Job.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, Error list>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, Job.lift parse)

  static member ParsedOpt(queryParamName, parse: 'ctx -> string -> 'a option) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobOpt(queryParamName, Job.lift2 parse)

  static member ParsedOpt(queryParamName, parse: string -> 'a option) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobOpt(queryParamName, Job.lift parse)

  static member Parsed(queryParamName, parse: string -> 'a) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedJobRes(queryParamName, JobResult.lift parse)

  static member String(queryParamName) : CustomQueryParam<'ctx, string> =
    CustomQueryParam<'ctx, string>(queryParamName, fun _ s -> s |> Ok |> Job.result)

  static member Bool(queryParamName) : CustomQueryParam<'ctx, bool> =
    CustomQueryParam<'ctx, bool>(queryParamName, fun _ -> parseBool >> Job.result)

  static member Int(queryParamName) : CustomQueryParam<'ctx, int> =
    CustomQueryParam<'ctx, int>(queryParamName, fun _ -> parseInt >> Job.result)

  static member Float(queryParamName) : CustomQueryParam<'ctx, float> =
    CustomQueryParam<'ctx, float>(queryParamName, fun _ -> parseFloat >> Job.result)

  static member Enum(queryParamName, enumMap: (string * 'a) list) =
    let d = dict enumMap
    let allowed = enumMap |> List.map fst |> List.distinct
    let parse s =
      match d.TryGetValue s with
      | true, v -> Ok v
      | false, _ -> Error [queryInvalidEnum queryParamName s allowed]
    CustomQueryParam<'ctx, 'a>(queryParamName, fun _ s -> parse s |> Job.result)



[<AutoOpen>]
module QueryExtensions =


  type Query with

    static member Parsed(queryParamName, parse: 'ctx -> string -> 'a) : CustomQueryParam<'ctx, 'a> =
      Query.ParsedJobRes(queryParamName, JobResult.lift2 parse)



type Header =

  static member String(headerName) =
    Header<'ctx, string>(headerName, fun _ s -> s |> Ok |> Job.result)

  static member ParsedJobRes(headerName, parse: 'ctx -> string -> Job<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, parse)

  static member ParsedJobRes(headerName, parse: string -> Job<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, fun _ s -> parse s)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, Job.liftAsync parse)

  static member ParsedJobOpt(headerName, parse: 'ctx -> string -> Job<'a option>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, fun ctx s -> parse ctx s |> Job.map (Result.requireSome [headerInvalidParsedNone headerName s]))

  static member ParsedJobOpt(headerName, parse: string -> Job<'a option>) : Header<'ctx, 'a> =
    Header.ParsedJobOpt(headerName, fun _ s -> parse s)

  static member ParsedAsyncOpt(headerName, parse: 'ctx -> string -> Async<'a option>) : Header<'ctx, 'a> =
    Header.ParsedJobOpt(headerName, Job.liftAsync2 parse)

  static member ParsedAsyncOpt(headerName, parse: string -> Async<'a option>) : Header<'ctx, 'a> =
    Header.ParsedJobOpt(headerName, Job.liftAsync parse)

  static member ParsedJob(headerName, parse: 'ctx -> string -> Job<'a>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, fun ctx s -> parse ctx s |> Job.map Ok)

  static member ParsedJob(headerName, parse: string -> Job<'a>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, fun _ s -> parse s |> Job.map Ok)

  static member ParsedAsync(headerName, parse: 'ctx -> string -> Async<'a>) : Header<'ctx, 'a> =
    Header.ParsedJob(headerName, Job.liftAsync2 parse)

  static member ParsedAsync(headerName, parse: string -> Async<'a>) : Header<'ctx, 'a> =
    Header.ParsedJob(headerName, Job.liftAsync parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, Error list>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, Job.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, Error list>) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, Job.lift parse)

  static member ParsedOpt(headerName, parse: 'ctx -> string -> 'a option) : Header<'ctx, 'a> =
    Header.ParsedJobOpt(headerName, Job.lift2 parse)

  static member ParsedOpt(headerName, parse: string -> 'a option) : Header<'ctx, 'a> =
    Header.ParsedJobOpt(headerName, Job.lift parse)

  static member Parsed(headerName, parse: string -> 'a) : Header<'ctx, 'a> =
    Header.ParsedJobRes(headerName, JobResult.lift parse)



[<AutoOpen>]
module HeaderExtensions =


  type Header with

    static member Parsed(headerName, parse: 'ctx -> string -> 'a) : Header<'ctx, 'a> =
      Header.ParsedJobRes(headerName, JobResult.lift2 parse)
