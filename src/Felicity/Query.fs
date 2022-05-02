namespace Felicity

open System
open System.Threading.Tasks
open Errors


[<AutoOpen>]
module private QueryParseHelpers =

  open System.Text.RegularExpressions

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
    try System.Text.Json.JsonSerializer.Deserialize<DateTime> ("\"" + str + "\"") |> Ok
    with _ -> Error [queryInvalidParsedErrMsgUnnamed str "Expected a valid ISO 8601-1:2019 date-time"]

  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTimeOffset.
  let parseDateTimeOffsetAllowMissingOffset (str: string) =
    try System.Text.Json.JsonSerializer.Deserialize<DateTimeOffset> ("\"" + str + "\"") |> Ok
    with _ -> Error [queryInvalidParsedErrMsgUnnamed str "Expected a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"]


  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTimeOffset.
  let parseDateTimeOffset =
    let r = Regex("(?>Z|(?>\+|-)\d\d:\d\d)$", RegexOptions.Compiled)
    let errMsg = "Expected a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
    fun (str: string) ->
      try
        let res = System.Text.Json.JsonSerializer.Deserialize<DateTimeOffset> ("\"" + str + "\"")
        if r.IsMatch str then Ok res
        else Error [queryInvalidParsedErrMsgUnnamed str errMsg]
      with _ -> Error [queryInvalidParsedErrMsgUnnamed str errMsg]



type ListFilter<'ctx, 'a> internal (fieldName: string, parse: 'ctx -> string -> Task<Result<'a, Error list>>, ?operator: string) =

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
          | false, _ -> Ok None |> Task.result
          | true, "" ->  Ok (Some []) |> Task.result
          | true, str ->
              str.Split(',')
              |> Array.toList
              |> List.traverseTaskResultA (
                  parse ctx
                  >> TaskResult.mapError (List.map (Error.setSourceParam queryParamName))
              )
              |> TaskResult.map Some
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
      |> TaskResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

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
    ListFilter<'ctx, bool>(fieldName, (fun _ s -> parseBool s |> Task.result), ?operator=operator)



type SingleFilter<'ctx, 'a> internal (fieldName: string, parse: 'ctx -> string -> Task<Result<'a, Error list>>, ?operator: string, ?allowCommas: bool) =

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
          let allowCommas = defaultArg allowCommas false

          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Task.result
          | true, str ->
              if not allowCommas && (str.Contains ",") then
                let numElements = str.Split(',').Length
                Error [queryNotSingular queryParamName numElements] |> Task.result
              else
                parse ctx str
                |> TaskResult.mapError (List.map (Error.setSourceParam queryParamName))
                |> TaskResult.map Some
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
      |> TaskResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]

  member _.Operator(operator) =
    SingleFilter<'ctx, 'a>(fieldName, parse, operator)

  member _.AllowCommas =
    SingleFilter<'ctx, 'a>(fieldName, parse, ?operator=operator, allowCommas=true)

  member _.List =
    ListFilter<'ctx, 'a>(fieldName, parse, ?operator=operator)

  member _.Bool =
    SingleFilter<'ctx, bool>(fieldName, (fun _ s -> parseBool s |> Task.result), ?operator=operator, ?allowCommas=allowCommas)


type ListSort<'ctx, 'a> internal (parse: 'ctx -> string -> Task<Result<'a, Error list>>) =

  let queryParamName = "sort"
  
  member _.Optional =
    { new RequestGetter<'ctx, ('a * bool) list option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Task.result
          | true, "" -> Ok (Some []) |> Task.result
          | true, str ->
              str.Split(',')
              |> Array.toList
              |> List.map (fun str ->
                   if str.StartsWith("-", StringComparison.Ordinal)
                   then str.Substring(1), true
                   else str, false
              )
              // If a sort column appears more than once, only the first occurrence will
              // influence the ordering (if the server is well-behaved). Furthermore,
              // duplicate sort columns may cause errors in certain databases (e.g. SQL
              // Server). Therefore, use List.distinctBy to only keep the first occurrence
              // of each sort column.
              |> List.distinctBy fst
              |> List.traverseTaskResultA (fun (sort, isDescending) ->
                   parse ctx sort
                   |> TaskResult.mapError (List.map (Error.setSourceParam queryParamName))
                   |> TaskResult.map (fun s -> s, isDescending)
              )
              |> TaskResult.map Some
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
      |> TaskResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]



type SingleSort<'ctx, 'a> internal (parse: 'ctx -> string -> Task<Result<'a, Error list>>) =

  let queryParamName = "sort"

  member _.Optional =
    { new RequestGetter<'ctx, ('a * bool) option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Task.result
          | true, str ->
              if str.Contains "," then
                let numElements = str.Split(',').Length
                Error [queryNotSingular queryParamName numElements] |> Task.result
              else
                let sort, isDescending =
                  if str.StartsWith("-", StringComparison.Ordinal)
                  then str.Substring(1), true
                  else str, false
                parse ctx sort
                |> TaskResult.mapError (List.map (Error.setSourceParam queryParamName))
                |> TaskResult.map (fun s -> Some (s, isDescending))
    }

  interface OptionalRequestGetter<'ctx, 'a * bool> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)

  interface RequestGetter<'ctx, 'a * bool> with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member this.Get(ctx, req, includedTypeAndId) =
      this.Optional.Get(ctx, req, includedTypeAndId)
      |> TaskResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

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
          | false, _ -> Ok None |> Task.result
          | true, i ->
              match Int32.TryParse i with
              | false, _ -> Error [queryInvalidInt queryParamName i] |> Task.result
              | true, i ->
                  match min, max with
                  | Some min, _ when i < min -> Error [queryIntTooSmall queryParamName i min] |> Task.result
                  | _, Some max when i > max -> Error [queryIntTooLarge queryParamName i max] |> Task.result
                  | _ -> Ok (Some i) |> Task.result
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
      |> TaskResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

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


type CustomQueryParam<'ctx, 'a> internal (queryParamName, parse: 'ctx -> string -> Task<Result<'a, Error list>>) =

  member _.Optional =
    { new RequestGetter<'ctx, 'a option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Task.result
          | true, str ->
              parse ctx str
              |> TaskResult.mapError (List.map (Error.setSourceParam queryParamName))
              |> TaskResult.map Some
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
      |> TaskResult.requireSome [reqParserMissingRequiredQueryParam queryParamName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = Some queryParamName
    member _.GetErrors(req, _) =
      match req.Query.TryGetValue queryParamName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedQueryParam queryParamName]


type Header<'ctx, 'a> internal (headerName: string, parse: 'ctx -> string -> Task<Result<'a, Error list>>) =

  member _.Optional =
    { new RequestGetter<'ctx, 'a option> with
        member _.FieldName = None
        member _.QueryParamName = None
        member _.Get(ctx, req, _) =
          match req.Headers.TryGetValue headerName with
          | false, _ -> Ok None |> Task.result
          | true, str -> parse ctx str |> TaskResult.map Some
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
      |> TaskResult.requireSome [reqParserMissingRequiredHeader headerName]

  interface ProhibitedRequestGetter with
    member _.FieldName = None
    member _.QueryParamName = None
    member _.GetErrors(req, _) =
      match req.Headers.TryGetValue headerName with
      | false, _ -> []
      | true, _ -> [reqParserProhibitedHeader headerName]



type Filter =

  static member Field(id: Id<'ctx, 'entity, 'id>) =
    SingleFilter<'ctx, 'id>("id", id.toDomain)

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(field.Name, fun ctx str -> toSerialized str |> Task.result |> TaskResult.bind (field.ToDomain ctx))

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Task.result |> TaskResult.bind (field.ToDomain ctx))

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
    // Note: We allow missing offset here because it's checked in the attribute, if relevant.
    Filter.Field(field, parseDateTimeOffsetAllowMissingOffset)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + field.Name, fun ctx str -> toSerialized str |> Task.result |> TaskResult.bind (field.ToDomain ctx))

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path.Name + "." + field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Task.result |> TaskResult.bind (field.ToDomain ctx))

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
    // Note: We allow missing offset here because it's checked in the attribute, if relevant.
    Filter.Field(path, field, parseDateTimeOffsetAllowMissingOffset)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + field.Name, fun ctx str -> toSerialized str |> Task.result |> TaskResult.bind (field.ToDomain ctx))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str]  |> Task.result |> TaskResult.bind (field.ToDomain ctx))

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
    // Note: We allow missing offset here because it's checked in the attribute, if relevant.
    Filter.Field(path1, path2, field, parseDateTimeOffsetAllowMissingOffset)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + field.Name, fun ctx str -> toSerialized str |> Task.result |> TaskResult.bind (field.ToDomain ctx))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + field.Name, fun ctx str -> toSerialized str |> Result.requireSome [queryInvalidParsedNoneUnnamed str] |> Task.result |> TaskResult.bind (field.ToDomain ctx))

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
    // Note: We allow missing offset here because it's checked in the attribute, if relevant.
    Filter.Field(path1, path2, path3, field, parseDateTimeOffsetAllowMissingOffset)

  static member ParsedTaskRes(name, parse: 'ctx -> string -> Task<Result<'a, Error list>>) =
    SingleFilter<'ctx, 'a>(name, parse)

  static member ParsedTaskRes(name, parse: string -> Task<Result<'a, Error list>>) =
    Filter.ParsedTaskRes(name, fun _ s -> parse s)

  static member ParsedTaskRes(name, parse: 'ctx -> string -> Task<Result<'a, string>>) =
    Filter.ParsedTaskRes(name, fun ctx s -> parse ctx s |> TaskResult.mapError (queryInvalidParsedErrMsgUnnamed s >> List.singleton))

  static member ParsedTaskRes(name, parse: string -> Task<Result<'a, string>>) =
    Filter.ParsedTaskRes(name, fun _ s -> parse s)

  static member ParsedTaskRes(name, parse: 'ctx -> string -> Task<Result<'a, string list>>) =
    SingleFilter<'ctx, 'a>(name, fun ctx s -> parse ctx s |> TaskResult.mapError (List.map (queryInvalidParsedErrMsgUnnamed s)))

  static member ParsedTaskRes(name, parse: string -> Task<Result<'a, string list>>) =
    Filter.ParsedTaskRes(name, fun _ s -> parse s)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Filter.ParsedTaskRes(name, Task.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, Error list>>) =
    Filter.ParsedTaskRes(name, Task.liftAsync parse)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Filter.ParsedTaskRes(name, Task.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, string>>) =
    Filter.ParsedTaskRes(name, Task.liftAsync parse)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Filter.ParsedTaskRes(name, Task.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, string list>>) =
    Filter.ParsedTaskRes(name, Task.liftAsync parse)

  static member ParsedTaskOpt(name, parse: 'ctx -> string -> Task<'a option>) =
    Filter.ParsedTaskRes(name, fun ctx s -> parse ctx s |> Task.map (Result.requireSome [queryInvalidParsedNoneUnnamed s]))

  static member ParsedTaskOpt(name, parse: string -> Task<'a option>) =
    Filter.ParsedTaskRes(name, fun _ s -> parse s |> Task.map (Result.requireSome [queryInvalidParsedNoneUnnamed s]))

  static member ParsedAsyncOpt(name, parse: 'ctx -> string -> Async<'a option>) =
    Filter.ParsedTaskOpt(name, Task.liftAsync2 parse)

  static member ParsedAsyncOpt(name, parse: string -> Async<'a option>) =
    Filter.ParsedTaskOpt(name, Task.liftAsync parse)

  static member ParsedTask(name, parse: 'ctx -> string -> Task<'a>) =
    SingleFilter<'ctx, 'a>(name, TaskResult.liftTask2 parse)

  static member ParsedTask(name, parse: string -> Task<'a>) =
    SingleFilter<'ctx, 'a>(name, TaskResult.liftTask2 (fun _ s -> parse s))

  static member ParsedAsync(name, parse: 'ctx -> string -> Async<'a>) =
    Filter.ParsedTask(name, Task.liftAsync2 parse)

  static member ParsedAsync(name, parse: string -> Async<'a>) =
    Filter.ParsedTask(name, Task.liftAsync parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, Error list>) =
    Filter.ParsedTaskRes(name, Task.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, Error list>) =
    Filter.ParsedTaskRes(name, Task.lift parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, string>) =
    Filter.ParsedTaskRes(name, Task.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, string>) =
    Filter.ParsedTaskRes(name, Task.lift parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, string list>) =
    Filter.ParsedTaskRes(name, Task.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, string list>) =
    Filter.ParsedTaskRes(name, Task.lift parse)

  static member ParsedOpt(name, parse: 'ctx -> string -> 'a option) =
    Filter.ParsedTaskOpt(name, Task.lift2 parse)

  static member ParsedOpt(name, parse: string -> 'a option) =
    Filter.ParsedTaskOpt(name, Task.lift parse)

  static member Parsed(name: string, parse: string -> 'a) =
    SingleFilter<'ctx, 'a>(name, TaskResult.lift2 (fun _ s -> parse s))

  static member String(name) =
    SingleFilter<'ctx, string>(name, fun _ s -> s |> Ok |> Task.result)

  static member Bool(name) =
    SingleFilter<'ctx, bool>(name, fun _ -> parseBool >> Task.result)

  static member Int(name) =
    SingleFilter<'ctx, int>(name, fun _ -> parseInt >> Task.result)

  static member Float(name) =
    SingleFilter<'ctx, float>(name, fun _ -> parseFloat >> Task.result)

  static member DateTime(name) =
    SingleFilter<'ctx, DateTime>(name, fun _ -> parseDateTime >> Task.result)

  static member DateTimeOffset(name) =
    SingleFilter<'ctx, DateTimeOffset>(name, fun _ -> parseDateTimeOffset >> Task.result)

  static member DateTimeOffsetAllowMissingOffset(name) =
    SingleFilter<'ctx, DateTimeOffset>(name, fun _ -> parseDateTimeOffsetAllowMissingOffset >> Task.result)

  static member Enum(name, enumMap: (string * 'a) list) =
    let d = dict enumMap
    let allowed = enumMap |> List.map fst |> List.distinct
    let parse s =
      match d.TryGetValue s with
      | true, v -> Ok v
      | false, _ -> Error [queryInvalidEnum $"filter[%s{name}]" s allowed]
    SingleFilter<'ctx, 'a>(name, fun _ s -> parse s |> Task.result)




[<AutoOpen>]
module FilterExtensions =


  type Filter with

    static member Parsed(name: string, parse: 'ctx -> string -> 'a) =
      SingleFilter<'ctx, 'a>(name, TaskResult.lift2 parse)



type Sort =

  static member ParsedTaskRes(parse: 'ctx -> string -> Task<Result<'a, Error list>>) =
    SingleSort<'ctx, 'a>(parse)

  static member ParsedTaskRes(parse: string -> Task<Result<'a, Error list>>) =
    Sort.ParsedTaskRes(fun _ s -> parse s)

  static member ParsedTaskRes(parse: 'ctx -> string -> Task<Result<'a, string>>) =
    Sort.ParsedTaskRes(fun ctx s -> parse ctx s |> TaskResult.mapError (queryInvalidParsedErrMsg "sort" s >> List.singleton))

  static member ParsedTaskRes(parse: string -> Task<Result<'a, string>>) =
    Sort.ParsedTaskRes(fun _ s -> parse s)

  static member ParsedTaskRes(parse: 'ctx -> string -> Task<Result<'a, string list>>) =
    Sort.ParsedTaskRes(fun ctx s -> parse ctx s |> TaskResult.mapError (List.map (queryInvalidParsedErrMsg "sort" s)))

  static member ParsedTaskRes(parse: string -> Task<Result<'a, string list>>) =
    Sort.ParsedTaskRes(fun _ s -> parse s)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Sort.ParsedTaskRes(Task.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, Error list>>) =
    Sort.ParsedTaskRes(Task.liftAsync parse)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Sort.ParsedTaskRes(Task.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, string>>) =
    Sort.ParsedTaskRes(Task.liftAsync parse)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Sort.ParsedTaskRes(Task.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, string list>>) =
    Sort.ParsedTaskRes(Task.liftAsync parse)

  static member ParsedTaskOpt(parse: 'ctx -> string -> Task<'a option>) =
    Sort.ParsedTaskRes(fun ctx s -> parse ctx s |> Task.map (Result.requireSome [queryInvalidParsedNone "sort" s]))

  static member ParsedTaskOpt(parse: string -> Task<'a option>) =
    Sort.ParsedTaskOpt(fun _ s -> parse s)

  static member ParsedAsyncOpt(parse: 'ctx -> string -> Async<'a option>) =
    Sort.ParsedTaskOpt(Task.liftAsync2 parse)

  static member ParsedAsyncOpt(parse: string -> Async<'a option>) =
    Sort.ParsedTaskOpt(Task.liftAsync parse)

  static member ParsedTask(parse: 'ctx -> string -> Task<'a>) =
    SingleSort<'ctx, 'a>(TaskResult.liftTask2 parse)

  static member ParsedTask(parse: string -> Task<'a>) =
    SingleSort<'ctx, 'a>(TaskResult.liftTask2 (fun _ s -> parse s))

  static member ParsedAsync(parse: 'ctx -> string -> Async<'a>) =
    Sort.ParsedTask(Task.liftAsync2 parse)

  static member ParsedAsync(parse: string -> Async<'a>) =
    Sort.ParsedTask(Task.liftAsync parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, Error list>) =
    Sort.ParsedTaskRes(Task.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, Error list>) =
    Sort.ParsedTaskRes(Task.lift parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, string>) =
    Sort.ParsedTaskRes(Task.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, string>) =
    Sort.ParsedTaskRes(Task.lift parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, string list>) =
    Sort.ParsedTaskRes(Task.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, string list>) =
    Sort.ParsedTaskRes(Task.lift parse)

  static member ParsedOpt(parse: 'ctx -> string -> 'a option) =
    Sort.ParsedTaskOpt(Task.lift2 parse)

  static member ParsedOpt(parse: string -> 'a option) =
    Sort.ParsedTaskOpt(Task.lift parse)

  static member Parsed(parse: string -> 'a) =
    SingleSort<'ctx, 'a>(TaskResult.lift2 (fun _ s -> parse s))

  static member Enum(sortMap: (string * 'a) list) =
    let d = dict sortMap
    let allowed = sortMap |> List.map fst |> List.distinct
    let parseSort s =
      match d.TryGetValue s with
      | true, v -> Ok v
      | false, _ -> Error [queryInvalidEnum "sort" s allowed]
    SingleSort<'ctx, 'a>(fun _ s -> parseSort s |> Task.result)



[<AutoOpen>]
module SortExtensions =


  type Sort with

    static member Parsed(parse: 'ctx -> string -> 'a) =
      SingleSort<'ctx, 'a>(TaskResult.lift2 parse)



type Page<'ctx> =

  static member Offset = PageParam<'ctx>("offset", min=0)

  static member Limit = PageParam<'ctx>("limit", min=1)

  static member Number = PageParam<'ctx>("number", min=0)

  static member Size = PageParam<'ctx>("size", min=1)



type Query =

  static member ParsedTaskRes(queryParamName, parse: 'ctx -> string -> Task<Result<'a, Error list>>) =
    CustomQueryParam<'ctx, 'a>(queryParamName, parse)

  static member ParsedTaskRes(queryParamName, parse: string -> Task<Result<'a, Error list>>) =
    Query.ParsedTaskRes(queryParamName, fun _ s -> parse s)

  static member ParsedTaskRes(queryParamName, parse: 'ctx -> string -> Task<Result<'a, string>>) =
    Query.ParsedTaskRes(queryParamName, fun ctx s -> parse ctx s |> TaskResult.mapError (queryInvalidParsedErrMsg queryParamName s >> List.singleton))

  static member ParsedTaskRes(queryParamName, parse: string -> Task<Result<'a, string>>) =
    Query.ParsedTaskRes(queryParamName, fun _ s -> parse s)

  static member ParsedTaskRes(queryParamName, parse: 'ctx -> string -> Task<Result<'a, string list>>) =
    Query.ParsedTaskRes(queryParamName, fun ctx s -> parse ctx s |> TaskResult.mapError (List.map (queryInvalidParsedErrMsg queryParamName s)))

  static member ParsedTaskRes(queryParamName, parse: string -> Task<Result<'a, string list>>) =
    Query.ParsedTaskRes(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, Error list>>) =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync parse)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, string>>) =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync parse)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, string list>>) =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync parse)

  static member ParsedTaskOpt(queryParamName, parse: 'ctx -> string -> Task<'a option>) =
    Query.ParsedTaskRes(queryParamName, fun ctx s -> parse ctx s |> Task.map (Result.requireSome [queryInvalidParsedNone queryParamName s]))

  static member ParsedTaskOpt(queryParamName, parse: string -> Task<'a option>) =
    Query.ParsedTaskOpt(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncOpt(queryParamName, parse: 'ctx -> string -> Async<'a option>) =
    Query.ParsedTaskOpt(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncOpt(queryParamName, parse: string -> Async<'a option>) =
    Query.ParsedTaskOpt(queryParamName, Task.liftAsync parse)

  static member ParsedTask(queryParamName, parse: 'ctx -> string -> Task<'a>) =
    CustomQueryParam<'ctx, 'a>(queryParamName, TaskResult.liftTask2 parse)

  static member ParsedTask(queryParamName, parse: string -> Task<'a>) =
    CustomQueryParam<'ctx, 'a>(queryParamName, TaskResult.liftTask2 (fun _ s -> parse s))

  static member ParsedAsync(queryParamName, parse: 'ctx -> string -> Async<'a>) =
    Query.ParsedTask(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsync(queryParamName, parse: string -> Async<'a>) =
    Query.ParsedTask(queryParamName, Task.liftAsync parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, Error list>) =
    Query.ParsedTaskRes(queryParamName, Task.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, Error list>) =
    Query.ParsedTaskRes(queryParamName, Task.lift parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, string>) =
    Query.ParsedTaskRes(queryParamName, Task.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, string>) =
    Query.ParsedTaskRes(queryParamName, Task.lift parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, string list>) =
    Query.ParsedTaskRes(queryParamName, Task.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, string list>) =
    Query.ParsedTaskRes(queryParamName, Task.lift parse)

  static member ParsedOpt(queryParamName, parse: 'ctx -> string -> 'a option) =
    Query.ParsedTaskOpt(queryParamName, Task.lift2 parse)

  static member ParsedOpt(queryParamName, parse: string -> 'a option) =
    Query.ParsedTaskOpt(queryParamName, Task.lift parse)

  static member Parsed(queryParamName, parse: string -> 'a) =
    CustomQueryParam<'ctx, 'a>(queryParamName, TaskResult.lift2 (fun _ s -> parse s))

  // Note: When adding more overloads, consider adding them to Filter, too.

  static member String(queryParamName) : CustomQueryParam<'ctx, string> =
    CustomQueryParam<'ctx, string>(queryParamName, fun _ s -> s |> Ok |> Task.result)

  static member Bool(queryParamName) : CustomQueryParam<'ctx, bool> =
    CustomQueryParam<'ctx, bool>(queryParamName, fun _ -> parseBool >> Task.result)

  static member Int(queryParamName) : CustomQueryParam<'ctx, int> =
    CustomQueryParam<'ctx, int>(queryParamName, fun _ -> parseInt >> Task.result)

  static member Float(queryParamName) : CustomQueryParam<'ctx, float> =
    CustomQueryParam<'ctx, float>(queryParamName, fun _ -> parseFloat >> Task.result)

  static member DateTime(queryParamName) : CustomQueryParam<'ctx, DateTime> =
    CustomQueryParam<'ctx, DateTime>(queryParamName, fun _ -> parseDateTime >> Task.result)

  static member DateTimeOffset(queryParamName) : CustomQueryParam<'ctx, DateTimeOffset> =
    CustomQueryParam<'ctx, DateTimeOffset>(queryParamName, fun _ -> parseDateTimeOffset >> Task.result)

  static member DateTimeOffsetAllowMissingOffset(queryParamName) : CustomQueryParam<'ctx, DateTimeOffset> =
    CustomQueryParam<'ctx, DateTimeOffset>(queryParamName, fun _ -> parseDateTimeOffsetAllowMissingOffset >> Task.result)

  // Note: When adding more overloads, consider adding them to Filter, too.

  static member Enum(queryParamName, enumMap: (string * 'a) list) =
    let d = dict enumMap
    let allowed = enumMap |> List.map fst |> List.distinct
    let parse s =
      match d.TryGetValue s with
      | true, v -> Ok v
      | false, _ -> Error [queryInvalidEnum queryParamName s allowed]
    CustomQueryParam<'ctx, 'a>(queryParamName, fun _ s -> parse s |> Task.result)



[<AutoOpen>]
module QueryExtensions =


  type Query with

    static member Parsed(queryParamName, parse: 'ctx -> string -> 'a) =
      CustomQueryParam<'ctx, 'a>(queryParamName, TaskResult.lift2 parse)



type Header =

  static member String(headerName) =
    Header<'ctx, string>(headerName, fun _ s -> s |> Ok |> Task.result)

  static member ParsedTaskRes(headerName, parse: 'ctx -> string -> Task<Result<'a, Error list>>) =
    Header<'ctx, 'a>(headerName, parse)

  static member ParsedTaskRes(headerName, parse: string -> Task<Result<'a, Error list>>) =
    Header.ParsedTaskRes(headerName, fun _ s -> parse s)

  static member ParsedTaskRes(headerName, parse: 'ctx -> string -> Task<Result<'a, string>>) =
    Header.ParsedTaskRes(headerName, fun ctx s -> parse ctx s |> TaskResult.mapError (headerInvalidParsedErrMsg headerName s >> List.singleton))

  static member ParsedTaskRes(headerName, parse: string -> Task<Result<'a, string>>) =
    Header.ParsedTaskRes(headerName, fun _ s -> parse s)

  static member ParsedTaskRes(headerName, parse: 'ctx -> string -> Task<Result<'a, string list>>) =
    Header.ParsedTaskRes(headerName, fun ctx s -> parse ctx s |> TaskResult.mapError (List.map (headerInvalidParsedErrMsg headerName s)))

  static member ParsedTaskRes(headerName, parse: string -> Task<Result<'a, string list>>) =
    Header.ParsedTaskRes(headerName, fun _ s -> parse s)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) =
    Header.ParsedTaskRes(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, Error list>>) =
    Header.ParsedTaskRes(headerName, Task.liftAsync parse)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, string>>) =
    Header.ParsedTaskRes(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, string>>) =
    Header.ParsedTaskRes(headerName, Task.liftAsync parse)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, string list>>) =
    Header.ParsedTaskRes(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, string list>>) =
    Header.ParsedTaskRes(headerName, Task.liftAsync parse)

  static member ParsedTaskOpt(headerName, parse: 'ctx -> string -> Task<'a option>) =
    Header.ParsedTaskRes(headerName, fun ctx s -> parse ctx s |> Task.map (Result.requireSome [headerInvalidParsedNone headerName s]))

  static member ParsedTaskOpt(headerName, parse: string -> Task<'a option>) =
    Header.ParsedTaskOpt(headerName, fun _ s -> parse s)

  static member ParsedAsyncOpt(headerName, parse: 'ctx -> string -> Async<'a option>) =
    Header.ParsedTaskOpt(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncOpt(headerName, parse: string -> Async<'a option>) =
    Header.ParsedTaskOpt(headerName, Task.liftAsync parse)

  static member ParsedTask(headerName, parse: 'ctx -> string -> Task<'a>) =
    Header<'ctx, 'a>(headerName, TaskResult.liftTask2 parse)

  static member ParsedTask(headerName, parse: string -> Task<'a>) =
    Header<'ctx, 'a>(headerName, TaskResult.liftTask2 (fun _ s -> parse s))

  static member ParsedAsync(headerName, parse: 'ctx -> string -> Async<'a>) =
    Header.ParsedTask(headerName, Task.liftAsync2 parse)

  static member ParsedAsync(headerName, parse: string -> Async<'a>) =
    Header.ParsedTask(headerName, Task.liftAsync parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, Error list>) =
    Header.ParsedTaskRes(headerName, Task.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, Error list>) =
    Header.ParsedTaskRes(headerName, Task.lift parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, string>) =
    Header.ParsedTaskRes(headerName, Task.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, string>) =
    Header.ParsedTaskRes(headerName, Task.lift parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, string list>) =
    Header.ParsedTaskRes(headerName, Task.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, string list>) =
    Header.ParsedTaskRes(headerName, Task.lift parse)

  static member ParsedOpt(headerName, parse: 'ctx -> string -> 'a option) =
    Header.ParsedTaskOpt(headerName, Task.lift2 parse)

  static member ParsedOpt(headerName, parse: string -> 'a option) =
    Header.ParsedTaskOpt(headerName, Task.lift parse)

  static member Parsed(headerName, parse: string -> 'a) =
    Header<'ctx, 'a>(headerName, TaskResult.lift2 (fun _ s -> parse s))



[<AutoOpen>]
module HeaderExtensions =


  type Header with

    static member Parsed(headerName, parse: 'ctx -> string -> 'a) =
      Header<'ctx, 'a>(headerName, TaskResult.lift2 parse)
