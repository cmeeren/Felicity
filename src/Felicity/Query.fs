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

  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTime.
  let parseDateTime (str: string) =
    try System.Text.Json.JsonSerializer.Deserialize<DateTime> str |> Ok
    with _ -> Error [queryInvalidDateTimeUnnamed str]

  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTimeOffset.
  let parseDateTimeOffset (str: string) =
    try System.Text.Json.JsonSerializer.Deserialize<DateTimeOffset> str |> Ok
    with _ -> Error [queryInvalidDateTimeUnnamed str]



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

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(field.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, string>) =
    Filter.Field(field, Ok)

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, bool>) =
    Filter.Field(field, parseBool)

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, int>) =
    Filter.Field(field, parseInt)

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, float>) =
    Filter.Field(field, parseFloat)

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, DateTime>) =
    Filter.Field(field, parseDateTime)

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, DateTimeOffset>) =
    Filter.Field(field, parseDateTimeOffset)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + field.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, string>) =
    Filter.Field(path, field, Ok)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, bool>) =
    Filter.Field(path, field, parseBool)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, int>) =
    Filter.Field(path, field, parseInt)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, float>) =
    Filter.Field(path, field, parseFloat)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, DateTime>) =
    Filter.Field(path, field, parseDateTime)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, DateTimeOffset>) =
    Filter.Field(path, field, parseDateTimeOffset)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + field.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str]  |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, string>) =
    Filter.Field(path1, path2, field, Ok)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, bool>) =
    Filter.Field(path1, path2, field, parseBool)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, int>) =
    Filter.Field(path1, path2, field, parseInt)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, float>) =
    Filter.Field(path1, path2, field, parseFloat)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, DateTime>) =
    Filter.Field(path1, path2, field, parseDateTime)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, DateTimeOffset>) =
    Filter.Field(path1, path2, field, parseDateTimeOffset)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + field.Name, fun ctx str -> toSerialized str |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Job.result |> JobResult.bind (field.ToDomain ctx))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, string>) =
    Filter.Field(path1, path2, path3, field, Ok)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, bool>) =
    Filter.Field(path1, path2, path3, field, parseBool)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, int>) =
    Filter.Field(path1, path2, path3, field, parseInt)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, float>) =
    Filter.Field(path1, path2, path3, field, parseFloat)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, DateTime>) =
    Filter.Field(path1, path2, path3, field, parseDateTime)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, DateTimeOffset>) =
    Filter.Field(path1, path2, path3, field, parseDateTimeOffset)

  static member ParsedJobRes(name, parse: 'ctx -> string -> Job<Result<'a, Error list>>) =
    SingleFilter<'ctx, 'a>(name, parse)

  static member ParsedJobRes(name, parse: string -> Job<Result<'a, Error list>>) =
    Filter.ParsedJobRes(name, fun _ s -> parse s)

  static member ParsedJobRes(name, parse: 'ctx -> string -> Job<Result<'a, string>>) =
    Filter.ParsedJobRes(name, fun ctx s -> parse ctx s |> JobResult.mapError (queryInvalidParsedErrMsgUnnamed s >> List.singleton))

  static member ParsedJobRes(name, parse: string -> Job<Result<'a, string>>) =
    Filter.ParsedJobRes(name, fun _ s -> parse s)

  static member ParsedJobRes(name, parse: 'ctx -> string -> Job<Result<'a, string list>>) =
    SingleFilter<'ctx, 'a>(name, fun ctx s -> parse ctx s |> JobResult.mapError (List.map (queryInvalidParsedErrMsgUnnamed s)))

  static member ParsedJobRes(name, parse: string -> Job<Result<'a, string list>>) =
    Filter.ParsedJobRes(name, fun _ s -> parse s)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Filter.ParsedJobRes(name, Job.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, Error list>>) =
    Filter.ParsedJobRes(name, Job.liftAsync parse)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Filter.ParsedJobRes(name, Job.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, string>>) =
    Filter.ParsedJobRes(name, Job.liftAsync parse)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Filter.ParsedJobRes(name, Job.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, string list>>) =
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
    SingleFilter<'ctx, 'a>(name, JobResult.liftJob2 parse)

  static member ParsedJob(name, parse: string -> Job<'a>) =
    SingleFilter<'ctx, 'a>(name, JobResult.liftJob2 (fun _ s -> parse s))

  static member ParsedAsync(name, parse: 'ctx -> string -> Async<'a>) =
    Filter.ParsedJob(name, Job.liftAsync2 parse)

  static member ParsedAsync(name, parse: string -> Async<'a>) =
    Filter.ParsedJob(name, Job.liftAsync parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, Error list>) =
    Filter.ParsedJobRes(name, Job.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, Error list>) =
    Filter.ParsedJobRes(name, Job.lift parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, string>) =
    Filter.ParsedJobRes(name, Job.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, string>) =
    Filter.ParsedJobRes(name, Job.lift parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, string list>) =
    Filter.ParsedJobRes(name, Job.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, string list>) =
    Filter.ParsedJobRes(name, Job.lift parse)

  static member ParsedOpt(name, parse: 'ctx -> string -> 'a option) =
    Filter.ParsedJobOpt(name, Job.lift2 parse)

  static member ParsedOpt(name, parse: string -> 'a option) =
    Filter.ParsedJobOpt(name, Job.lift parse)

  static member Parsed(name: string, parse: string -> 'a) =
    SingleFilter<'ctx, 'a>(name, JobResult.lift2 (fun _ s -> parse s))




[<AutoOpen>]
module FilterExtensions =


  type Filter with

    static member Parsed(name: string, parse: 'ctx -> string -> 'a) =
      SingleFilter<'ctx, 'a>(name, JobResult.lift2 parse)



type Sort =

  static member ParsedJobRes(parse: 'ctx -> string -> Job<Result<'a, Error list>>) =
    SingleSort<'ctx, 'a>(parse)

  static member ParsedJobRes(parse: string -> Job<Result<'a, Error list>>) =
    Sort.ParsedJobRes(fun _ s -> parse s)

  static member ParsedJobRes(parse: 'ctx -> string -> Job<Result<'a, string>>) =
    Sort.ParsedJobRes(fun ctx s -> parse ctx s |> JobResult.mapError (queryInvalidParsedErrMsg "sort" s >> List.singleton))

  static member ParsedJobRes(parse: string -> Job<Result<'a, string>>) =
    Sort.ParsedJobRes(fun _ s -> parse s)

  static member ParsedJobRes(parse: 'ctx -> string -> Job<Result<'a, string list>>) =
    Sort.ParsedJobRes(fun ctx s -> parse ctx s |> JobResult.mapError (List.map (queryInvalidParsedErrMsg "sort" s)))

  static member ParsedJobRes(parse: string -> Job<Result<'a, string list>>) =
    Sort.ParsedJobRes(fun _ s -> parse s)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Sort.ParsedJobRes(Job.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, Error list>>) =
    Sort.ParsedJobRes(Job.liftAsync parse)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Sort.ParsedJobRes(Job.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, string>>) =
    Sort.ParsedJobRes(Job.liftAsync parse)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Sort.ParsedJobRes(Job.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, string list>>) =
    Sort.ParsedJobRes(Job.liftAsync parse)

  static member ParsedJobOpt(parse: 'ctx -> string -> Job<'a option>) =
    Sort.ParsedJobRes(fun ctx s -> parse ctx s |> Job.map (Result.requireSome [queryInvalidParsedNone "sort" s]))

  static member ParsedJobOpt(parse: string -> Job<'a option>) =
    Sort.ParsedJobOpt(fun _ s -> parse s)

  static member ParsedAsyncOpt(parse: 'ctx -> string -> Async<'a option>) =
    Sort.ParsedJobOpt(Job.liftAsync2 parse)

  static member ParsedAsyncOpt(parse: string -> Async<'a option>) =
    Sort.ParsedJobOpt(Job.liftAsync parse)

  static member ParsedJob(parse: 'ctx -> string -> Job<'a>) =
    SingleSort<'ctx, 'a>(JobResult.liftJob2 parse)

  static member ParsedJob(parse: string -> Job<'a>) =
    SingleSort<'ctx, 'a>(JobResult.liftJob2 (fun _ s -> parse s))

  static member ParsedAsync(parse: 'ctx -> string -> Async<'a>) =
    Sort.ParsedJob(Job.liftAsync2 parse)

  static member ParsedAsync(parse: string -> Async<'a>) =
    Sort.ParsedJob(Job.liftAsync parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, Error list>) =
    Sort.ParsedJobRes(Job.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, Error list>) =
    Sort.ParsedJobRes(Job.lift parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, string>) =
    Sort.ParsedJobRes(Job.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, string>) =
    Sort.ParsedJobRes(Job.lift parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, string list>) =
    Sort.ParsedJobRes(Job.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, string list>) =
    Sort.ParsedJobRes(Job.lift parse)

  static member ParsedOpt(parse: 'ctx -> string -> 'a option) =
    Sort.ParsedJobOpt(Job.lift2 parse)

  static member ParsedOpt(parse: string -> 'a option) =
    Sort.ParsedJobOpt(Job.lift parse)

  static member Parsed(parse: string -> 'a) =
    SingleSort<'ctx, 'a>(JobResult.lift2 (fun _ s -> parse s))

  static member Enum(sortMap: (string * 'a) list) =
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

    static member Parsed(parse: 'ctx -> string -> 'a) =
      SingleSort<'ctx, 'a>(JobResult.lift2 parse)



type Page<'ctx> =

  static member Offset = PageParam<'ctx>("offset", min=0)

  static member Limit = PageParam<'ctx>("limit", min=1)

  static member Number = PageParam<'ctx>("number", min=0)

  static member Size = PageParam<'ctx>("size", min=1)



type Query =

  static member ParsedJobRes(queryParamName, parse: 'ctx -> string -> Job<Result<'a, Error list>>) =
    CustomQueryParam<'ctx, 'a>(queryParamName, parse)

  static member ParsedJobRes(queryParamName, parse: string -> Job<Result<'a, Error list>>) =
    Query.ParsedJobRes(queryParamName, fun _ s -> parse s)

  static member ParsedJobRes(queryParamName, parse: 'ctx -> string -> Job<Result<'a, string>>) =
    Query.ParsedJobRes(queryParamName, fun ctx s -> parse ctx s |> JobResult.mapError (queryInvalidParsedErrMsg queryParamName s >> List.singleton))

  static member ParsedJobRes(queryParamName, parse: string -> Job<Result<'a, string>>) =
    Query.ParsedJobRes(queryParamName, fun _ s -> parse s)

  static member ParsedJobRes(queryParamName, parse: 'ctx -> string -> Job<Result<'a, string list>>) =
    Query.ParsedJobRes(queryParamName, fun ctx s -> parse ctx s |> JobResult.mapError (List.map (queryInvalidParsedErrMsg queryParamName s)))

  static member ParsedJobRes(queryParamName, parse: string -> Job<Result<'a, string list>>) =
    Query.ParsedJobRes(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Query.ParsedJobRes(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, Error list>>) =
    Query.ParsedJobRes(queryParamName, Job.liftAsync parse)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Query.ParsedJobRes(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, string>>) =
    Query.ParsedJobRes(queryParamName, Job.liftAsync parse)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Query.ParsedJobRes(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, string list>>) =
    Query.ParsedJobRes(queryParamName, Job.liftAsync parse)

  static member ParsedJobOpt(queryParamName, parse: 'ctx -> string -> Job<'a option>) =
    Query.ParsedJobRes(queryParamName, fun ctx s -> parse ctx s |> Job.map (Result.requireSome [queryInvalidParsedNone queryParamName s]))

  static member ParsedJobOpt(queryParamName, parse: string -> Job<'a option>) =
    Query.ParsedJobOpt(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncOpt(queryParamName, parse: 'ctx -> string -> Async<'a option>) =
    Query.ParsedJobOpt(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsyncOpt(queryParamName, parse: string -> Async<'a option>) =
    Query.ParsedJobOpt(queryParamName, Job.liftAsync parse)

  static member ParsedJob(queryParamName, parse: 'ctx -> string -> Job<'a>) =
    CustomQueryParam<'ctx, 'a>(queryParamName, JobResult.liftJob2 parse)

  static member ParsedJob(queryParamName, parse: string -> Job<'a>) =
    CustomQueryParam<'ctx, 'a>(queryParamName, JobResult.liftJob2 (fun _ s -> parse s))

  static member ParsedAsync(queryParamName, parse: 'ctx -> string -> Async<'a>) =
    Query.ParsedJob(queryParamName, Job.liftAsync2 parse)

  static member ParsedAsync(queryParamName, parse: string -> Async<'a>) =
    Query.ParsedJob(queryParamName, Job.liftAsync parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, Error list>) =
    Query.ParsedJobRes(queryParamName, Job.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, Error list>) =
    Query.ParsedJobRes(queryParamName, Job.lift parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, string>) =
    Query.ParsedJobRes(queryParamName, Job.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, string>) =
    Query.ParsedJobRes(queryParamName, Job.lift parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, string list>) =
    Query.ParsedJobRes(queryParamName, Job.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, string list>) =
    Query.ParsedJobRes(queryParamName, Job.lift parse)

  static member ParsedOpt(queryParamName, parse: 'ctx -> string -> 'a option) =
    Query.ParsedJobOpt(queryParamName, Job.lift2 parse)

  static member ParsedOpt(queryParamName, parse: string -> 'a option) =
    Query.ParsedJobOpt(queryParamName, Job.lift parse)

  static member Parsed(queryParamName, parse: string -> 'a) =
    CustomQueryParam<'ctx, 'a>(queryParamName, JobResult.lift2 (fun _ s -> parse s))

  static member String(queryParamName) : CustomQueryParam<'ctx, string> =
    CustomQueryParam<'ctx, string>(queryParamName, fun _ s -> s |> Ok |> Job.result)

  static member Bool(queryParamName) : CustomQueryParam<'ctx, bool> =
    CustomQueryParam<'ctx, bool>(queryParamName, fun _ -> parseBool >> Job.result)

  static member Int(queryParamName) : CustomQueryParam<'ctx, int> =
    CustomQueryParam<'ctx, int>(queryParamName, fun _ -> parseInt >> Job.result)

  static member Float(queryParamName) : CustomQueryParam<'ctx, float> =
    CustomQueryParam<'ctx, float>(queryParamName, fun _ -> parseFloat >> Job.result)

  static member DateTime(queryParamName) : CustomQueryParam<'ctx, DateTime> =
    CustomQueryParam<'ctx, DateTime>(queryParamName, fun _ -> parseDateTime >> Job.result)

  static member DateTimeOffset(queryParamName) : CustomQueryParam<'ctx, DateTimeOffset> =
    CustomQueryParam<'ctx, DateTimeOffset>(queryParamName, fun _ -> parseDateTimeOffset >> Job.result)

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

    static member Parsed(queryParamName, parse: 'ctx -> string -> 'a) =
      CustomQueryParam<'ctx, 'a>(queryParamName, JobResult.lift2 parse)



type Header =

  static member String(headerName) =
    Header<'ctx, string>(headerName, fun _ s -> s |> Ok |> Job.result)

  static member ParsedJobRes(headerName, parse: 'ctx -> string -> Job<Result<'a, Error list>>) =
    Header<'ctx, 'a>(headerName, parse)

  static member ParsedJobRes(headerName, parse: string -> Job<Result<'a, Error list>>) =
    Header.ParsedJobRes(headerName, fun _ s -> parse s)

  static member ParsedJobRes(headerName, parse: 'ctx -> string -> Job<Result<'a, string>>) =
    Header.ParsedJobRes(headerName, fun ctx s -> parse ctx s |> JobResult.mapError (headerInvalidParsedErrMsg headerName s >> List.singleton))

  static member ParsedJobRes(headerName, parse: string -> Job<Result<'a, string>>) =
    Header.ParsedJobRes(headerName, fun _ s -> parse s)

  static member ParsedJobRes(headerName, parse: 'ctx -> string -> Job<Result<'a, string list>>) =
    Header.ParsedJobRes(headerName, fun ctx s -> parse ctx s |> JobResult.mapError (List.map (headerInvalidParsedErrMsg headerName s)))

  static member ParsedJobRes(headerName, parse: string -> Job<Result<'a, string list>>) =
    Header.ParsedJobRes(headerName, fun _ s -> parse s)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Header.ParsedJobRes(headerName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, Error list>>) =
    Header.ParsedJobRes(headerName, Job.liftAsync parse)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Header.ParsedJobRes(headerName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, string>>) =
    Header.ParsedJobRes(headerName, Job.liftAsync parse)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Header.ParsedJobRes(headerName, Job.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, string list>>) =
    Header.ParsedJobRes(headerName, Job.liftAsync parse)

  static member ParsedJobOpt(headerName, parse: 'ctx -> string -> Job<'a option>) =
    Header.ParsedJobRes(headerName, fun ctx s -> parse ctx s |> Job.map (Result.requireSome [headerInvalidParsedNone headerName s]))

  static member ParsedJobOpt(headerName, parse: string -> Job<'a option>) =
    Header.ParsedJobOpt(headerName, fun _ s -> parse s)

  static member ParsedAsyncOpt(headerName, parse: 'ctx -> string -> Async<'a option>) =
    Header.ParsedJobOpt(headerName, Job.liftAsync2 parse)

  static member ParsedAsyncOpt(headerName, parse: string -> Async<'a option>) =
    Header.ParsedJobOpt(headerName, Job.liftAsync parse)

  static member ParsedJob(headerName, parse: 'ctx -> string -> Job<'a>) =
    Header<'ctx, 'a>(headerName, JobResult.liftJob2 (fun ctx s -> parse ctx s))

  static member ParsedJob(headerName, parse: string -> Job<'a>) =
    Header<'ctx, 'a>(headerName, JobResult.liftJob2 (fun _ s -> parse s))

  static member ParsedAsync(headerName, parse: 'ctx -> string -> Async<'a>) =
    Header.ParsedJob(headerName, Job.liftAsync2 parse)

  static member ParsedAsync(headerName, parse: string -> Async<'a>) =
    Header.ParsedJob(headerName, Job.liftAsync parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, Error list>) =
    Header.ParsedJobRes(headerName, Job.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, Error list>) =
    Header.ParsedJobRes(headerName, Job.lift parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, string>) =
    Header.ParsedJobRes(headerName, Job.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, string>) =
    Header.ParsedJobRes(headerName, Job.lift parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, string list>) =
    Header.ParsedJobRes(headerName, Job.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, string list>) =
    Header.ParsedJobRes(headerName, Job.lift parse)

  static member ParsedOpt(headerName, parse: 'ctx -> string -> 'a option) =
    Header.ParsedJobOpt(headerName, Job.lift2 parse)

  static member ParsedOpt(headerName, parse: string -> 'a option) =
    Header.ParsedJobOpt(headerName, Job.lift parse)

  static member Parsed(headerName, parse: string -> 'a) =
    Header<'ctx, 'a>(headerName, JobResult.lift2 (fun _ s -> parse s))



[<AutoOpen>]
module HeaderExtensions =


  type Header with

    static member Parsed(headerName, parse: 'ctx -> string -> 'a) =
      Header<'ctx, 'a>(headerName, JobResult.lift2 parse)
