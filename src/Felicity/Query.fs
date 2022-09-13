namespace Felicity

open System
open System.Threading.Tasks
open Errors


[<AutoOpen>]
module private QueryParseHelpers =

  open System.Text.RegularExpressions

  let parseBool (data: ParsedValueFromQueryData) =
    match data.Value with
    | "true" -> Ok true
    | "false" -> Ok false
    | invalid -> Error [invalidEnum (FromQuery data) invalid ["true"; "false"]]

  let parseInt (data: ParsedValueFromQueryData) =
    match Int32.TryParse data.Value with
    | true, x -> Ok x
    | false, _ -> Error [invalidParsedErrMsg (FromQuery data) "The value must be a valid integer"]

  let parseFloat (data: ParsedValueFromQueryData) =
    match Double.TryParse data.Value with
    | true, x -> Ok x
    | false, _ -> Error [invalidParsedErrMsg (FromQuery data) "The value must be a valid number"]

  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTime.
  let parseDateTime (data: ParsedValueFromQueryData) =
    try System.Text.Json.JsonSerializer.Deserialize<DateTime> ("\"" + data.Value + "\"") |> Ok
    with _ -> Error [invalidParsedErrMsg (FromQuery data) "The value must be a valid ISO 8601-1:2019 date-time"]

  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTimeOffset.
  let parseDateTimeOffsetAllowMissingOffset (data: ParsedValueFromQueryData) =
    try System.Text.Json.JsonSerializer.Deserialize<DateTimeOffset> ("\"" + data.Value + "\"") |> Ok
    with _ -> Error [invalidParsedErrMsg (FromQuery data) "The value must be a valid ISO 8601-1:2019 date-time"]


  /// Converts a string conforming to the ISO 8601-1:2019 format to a DateTimeOffset.
  let parseDateTimeOffset =
    let offsetRegex = Regex("(?>Z|(?>\+|-)\d\d:\d\d)$", RegexOptions.Compiled)  // Matches e.g.'Z' or '+01:00'
    fun (data: ParsedValueFromQueryData) ->
      let errMsg = "The value must be a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
      try
        let res = System.Text.Json.JsonSerializer.Deserialize<DateTimeOffset> ("\"" + data.Value + "\"")
        if offsetRegex.IsMatch data.Value then Ok res
        else Error [invalidParsedErrMsg (FromQuery data) errMsg]
      with _ -> Error [invalidParsedErrMsg (FromQuery data) errMsg]



type ListFilter<'ctx, 'a> internal (fieldName: string, parse: 'ctx -> ParsedValueFromQueryData -> Task<Result<'a, Error list>>, ?operator: string) =

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
              |> fun values ->
                  values
                  |> Array.traverseTaskResultAIndexed (fun i value ->
                      let valueData = {
                        Name = queryParamName
                        Value = value
                        NumValues = values.Length
                        ValueIndex = i
                      }
                      parse ctx valueData
                  )
                  |> TaskResult.map (Array.toList >> Some)
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


  member _.Operator(operator) : ListFilter<'ctx, 'a> =
    ListFilter<'ctx, 'a>(fieldName, parse, operator)

  member _.Bool : ListFilter<'ctx, bool> =
    ListFilter<'ctx, bool>(fieldName, (fun _ i -> parseBool i |> Task.result), ?operator=operator)



type SingleFilter<'ctx, 'a> internal (fieldName: string, parse: 'ctx -> ParsedValueFromQueryData -> Task<Result<'a, Error list>>, ?operator: string, ?allowCommas: bool) =

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
                let valueData = {
                  Name = queryParamName
                  Value = str
                  NumValues = 1
                  ValueIndex = 0
                }
                parse ctx valueData
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

  member _.Operator(operator) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(fieldName, parse, operator)

  member _.AllowCommas : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(fieldName, parse, ?operator=operator, allowCommas=true)

  member _.List : ListFilter<'ctx, 'a> =
    ListFilter<'ctx, 'a>(fieldName, parse, ?operator=operator)

  member _.Bool : SingleFilter<'ctx, bool> =
    SingleFilter<'ctx, bool>(fieldName, (fun _ i -> parseBool i |> Task.result), ?operator=operator, ?allowCommas=allowCommas)


type ListSort<'ctx, 'a> internal (parse: 'ctx -> ParsedValueFromQueryData -> Task<Result<'a, Error list>>) =

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
              |> fun values ->
                  values
                  |> Array.map (fun str ->
                       if str.StartsWith("-", StringComparison.Ordinal)
                       then str.Substring(1), true
                       else str, false
                  )
                  // If a sort column appears more than once, only the first occurrence will
                  // influence the ordering (if the server is well-behaved). Furthermore,
                  // duplicate sort columns may cause errors in certain databases (e.g. SQL
                  // Server). Therefore, use List.distinctBy to only keep the first occurrence
                  // of each sort column.
                  |> Array.distinctBy fst
                  |> Array.traverseTaskResultAIndexed (fun i (value, isDescending) ->
                       let valueData = {
                         Name = queryParamName
                         Value = value
                         NumValues = values.Length
                         ValueIndex = i
                       }
                       parse ctx valueData
                       |> TaskResult.map (fun s -> s, isDescending)
                  )
                  |> TaskResult.map (Array.toList >> Some)
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



type SingleSort<'ctx, 'a> internal (parse: 'ctx -> ParsedValueFromQueryData -> Task<Result<'a, Error list>>) =

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
                let value, isDescending =
                  if str.StartsWith("-", StringComparison.Ordinal)
                  then str.Substring(1), true
                  else str, false

                let valueData = {
                  Name = queryParamName
                  Value = value
                  NumValues = 1
                  ValueIndex = 0
                }

                parse ctx valueData
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

  member _.List : ListSort<'ctx, 'a> =
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
          | true, value ->
              let valueData = {
                Name = queryParamName
                Value = value
                NumValues = 1
                ValueIndex = 0
              }
              match Int32.TryParse value with
              | false, _ -> Error [invalidParsedErrMsg (FromQuery valueData) "The value must be a valid integer"] |> Task.result
              | true, i ->
                  match min, max with
                  | Some min, _ when i < min -> Error [invalidParsedErrMsg (FromQuery valueData) $"The value must be greater than or equal to %i{min}"] |> Task.result
                  | _, Some max when i > max -> Error [invalidParsedErrMsg (FromQuery valueData) $"The value must be less than or equal to %i{max}"] |> Task.result
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

  member _.Min(minValue: int) : PageParam<'ctx> =
    PageParam<'ctx>(pageName, min = minValue, ?max = max)

  member _.Max(maxValue: int) : PageParam<'ctx> =
    PageParam<'ctx>(pageName, ?min = min, max = maxValue)


type CustomQueryParam<'ctx, 'a> internal (queryParamName, parse: 'ctx -> ParsedValueFromQueryData -> Task<Result<'a, Error list>>) =

  member _.Optional =
    { new RequestGetter<'ctx, 'a option> with
        member _.FieldName = None
        member _.QueryParamName = Some queryParamName
        member _.Get(ctx, req, _) =
          match req.Query.TryGetValue queryParamName with
          | false, _ -> Ok None |> Task.result
          | true, str ->
              let valueData = {
                Name = queryParamName
                Value = str
                NumValues = 1
                ValueIndex = 0
              }
              parse ctx valueData
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


type Header<'ctx, 'a> internal (headerName: string, parse: 'ctx -> ParsedValueFromHeaderData -> Task<Result<'a, Error list>>) =

  member _.Optional =
    { new RequestGetter<'ctx, 'a option> with
        member _.FieldName = None
        member _.QueryParamName = None
        member _.Get(ctx, req, _) =
          match req.Headers.TryGetValue headerName with
          | false, _ -> Ok None |> Task.result
          | true, value ->
              let valueData: ParsedValueFromHeaderData = {
                Name = headerName
                Value = value
              }
              parse ctx valueData |> TaskResult.map Some
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
    SingleFilter<'ctx, 'id>("id", fun ctx d -> id.toDomain ctx d.Value |> TaskResult.mapError (List.map (fun f -> f (FromQuery d))))

  static member private Field(field: FieldQueryParser<'ctx, 'entity, 'attr, 'serialized>, toSerialized: ParsedValueFromQueryData -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(field.Name, fun ctx d -> toSerialized d |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) =
    SingleFilter<'ctx, 'attr>(field.Name, fun ctx d -> toSerialized d.Value |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(field: FieldQueryParser<'ctx, 'entity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) =
    SingleFilter<'ctx, 'attr>(field.Name, fun ctx d -> toSerialized d.Value |> Result.requireSome [invalidParsedNone (FromQuery d)] |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

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

  static member private Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: ParsedValueFromQueryData -> Result<'serialized, Error list>) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path.Name + "." + field.Name, fun ctx d -> toSerialized d |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path.Name + "." + field.Name, fun ctx d -> toSerialized d.Value |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, 'serialized>, toSerialized: string -> 'serialized option) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path.Name + "." + field.Name, fun ctx d -> toSerialized d.Value |> Result.requireSome [invalidParsedNone (FromQuery d)] |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, string>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path, field, Ok)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, bool>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path, field, parseBool)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, int>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path, field, parseInt)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, float>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path, field, parseFloat)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, DateTime>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path, field, parseDateTime)

  static member Field(path: Relationship<'ctx, 'entity, 'relatedEntity, 'relatedId>, field: FieldQueryParser<'ctx, 'relatedEntity, 'attr, DateTimeOffset>) : SingleFilter<'ctx, 'attr> =
    // Note: We allow missing offset here because it's checked in the attribute, if relevant.
    Filter.Field(path, field, parseDateTimeOffsetAllowMissingOffset)

  static member private Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: ParsedValueFromQueryData -> Result<'serialized, Error list>) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + field.Name, fun ctx d -> toSerialized d |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + field.Name, fun ctx d -> toSerialized d.Value |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, 'serialized>, toSerialized: string -> 'serialized option) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + field.Name, fun ctx d -> toSerialized d.Value |> Result.requireSome [invalidParsedNone (FromQuery d)]  |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, string>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, field, Ok)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, bool>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, field, parseBool)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, int>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, field, parseInt)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, float>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, field, parseFloat)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, DateTime>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, field, parseDateTime)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, field: FieldQueryParser<'ctx, 'relatedEntity2, 'attr, DateTimeOffset>) : SingleFilter<'ctx, 'attr> =
    // Note: We allow missing offset here because it's checked in the attribute, if relevant.
    Filter.Field(path1, path2, field, parseDateTimeOffsetAllowMissingOffset)

  static member private Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: ParsedValueFromQueryData -> Result<'serialized, Error list>) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + field.Name, fun ctx d -> toSerialized d |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> Result<'serialized, Error list>) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + field.Name, fun ctx d -> toSerialized d.Value |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, 'serialized>, toSerialized: string -> 'serialized option) : SingleFilter<'ctx, 'attr> =
    SingleFilter<'ctx, 'attr>(path1.Name + "." + path2.Name + "." + path3.Name + "." + field.Name, fun ctx d -> toSerialized d.Value |> Result.requireSome [invalidParsedNone (FromQuery d)] |> Task.result |> TaskResult.bind (field.ToDomain ctx (FromQuery d)))

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, string>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, path3, field, Ok)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, bool>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, path3, field, parseBool)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, int>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, path3, field, parseInt)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, float>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, path3, field, parseFloat)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, DateTime>) : SingleFilter<'ctx, 'attr> =
    Filter.Field(path1, path2, path3, field, parseDateTime)

  static member Field(path1: Relationship<'ctx, 'entity, 'relatedEntity1, 'relatedId1>, path2: Relationship<'ctx, 'relatedEntity1, 'relatedEntity2, 'relatedId2>, path3: Relationship<'ctx, 'relatedEntity2, 'relatedEntity3, 'relatedId3>, field: FieldQueryParser<'ctx, 'relatedEntity3, 'attr, DateTimeOffset>) : SingleFilter<'ctx, 'attr> =
    // Note: We allow missing offset here because it's checked in the attribute, if relevant.
    Filter.Field(path1, path2, path3, field, parseDateTimeOffsetAllowMissingOffset)

  static member ParsedTaskRes(name, parse: 'ctx -> string -> Task<Result<'a, Error list>>) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(name, fun ctx d -> parse ctx d.Value)

  static member ParsedTaskRes(name, parse: string -> Task<Result<'a, Error list>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, fun _ s -> parse s)

  static member ParsedTaskRes(name, parse: 'ctx -> string -> Task<Result<'a, string>>) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(name, fun ctx d -> parse ctx d.Value |> TaskResult.mapError (invalidParsedErrMsg (FromQuery d) >> List.singleton))

  static member ParsedTaskRes(name, parse: string -> Task<Result<'a, string>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, fun _ s -> parse s)

  static member ParsedTaskRes(name, parse: 'ctx -> string -> Task<Result<'a, string list>>) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(name, fun ctx d -> parse ctx d.Value |> TaskResult.mapError (List.map (invalidParsedErrMsg (FromQuery d))))

  static member ParsedTaskRes(name, parse: string -> Task<Result<'a, string list>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, fun _ s -> parse s)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, Error list>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, Error list>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.liftAsync parse)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, string>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, string>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.liftAsync parse)

  static member ParsedAsyncRes(name, parse: 'ctx -> string -> Async<Result<'a, string list>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.liftAsync2 parse)

  static member ParsedAsyncRes(name, parse: string -> Async<Result<'a, string list>>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.liftAsync parse)

  static member ParsedTaskOpt(name, parse: 'ctx -> string -> Task<'a option>) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(name, fun ctx d -> parse ctx d.Value |> Task.map (Result.requireSome [invalidParsedNone (FromQuery d)]))

  static member ParsedTaskOpt(name, parse: string -> Task<'a option>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskOpt(name, fun _ s -> parse s)

  static member ParsedAsyncOpt(name, parse: 'ctx -> string -> Async<'a option>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskOpt(name, Task.liftAsync2 parse)

  static member ParsedAsyncOpt(name, parse: string -> Async<'a option>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskOpt(name, Task.liftAsync parse)

  static member ParsedTask(name, parse: 'ctx -> string -> Task<'a>) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(name, fun ctx d -> parse ctx d.Value |> Task.map Ok)

  static member ParsedTask(name, parse: string -> Task<'a>) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(name, fun _ d -> parse d.Value |> Task.map Ok)

  static member ParsedAsync(name, parse: 'ctx -> string -> Async<'a>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTask(name, Task.liftAsync2 parse)

  static member ParsedAsync(name, parse: string -> Async<'a>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTask(name, Task.liftAsync parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, Error list>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, Error list>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.lift parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, string>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, string>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.lift parse)

  static member ParsedRes(name, parse: 'ctx -> string -> Result<'a, string list>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.lift2 parse)

  static member ParsedRes(name, parse: string -> Result<'a, string list>) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskRes(name, Task.lift parse)

  static member ParsedOpt(name, parse: 'ctx -> string -> 'a option) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskOpt(name, Task.lift2 parse)

  static member ParsedOpt(name, parse: string -> 'a option) : SingleFilter<'ctx, 'a> =
    Filter.ParsedTaskOpt(name, Task.lift parse)

  static member Parsed(name: string, parse: string -> 'a) : SingleFilter<'ctx, 'a> =
    SingleFilter<'ctx, 'a>(name, TaskResult.lift2 (fun _ d -> parse d.Value))

  static member String(name) : SingleFilter<'ctx, string> =
    SingleFilter<'ctx, string>(name, fun _ d -> d.Value |> Ok |> Task.result)

  static member Bool(name) : SingleFilter<'ctx, bool> =
    SingleFilter<'ctx, bool>(name, fun _ -> parseBool >> Task.result)

  static member Int(name) : SingleFilter<'ctx, int> =
    SingleFilter<'ctx, int>(name, fun _ -> parseInt >> Task.result)

  static member Float(name) : SingleFilter<'ctx, double> =
    SingleFilter<'ctx, float>(name, fun _ -> parseFloat >> Task.result)

  static member DateTime(name) : SingleFilter<'ctx, DateTime> =
    SingleFilter<'ctx, DateTime>(name, fun _ -> parseDateTime >> Task.result)

  static member DateTimeOffset(name) : SingleFilter<'ctx, DateTimeOffset> =
    SingleFilter<'ctx, DateTimeOffset>(name, fun _ -> parseDateTimeOffset >> Task.result)

  static member DateTimeOffsetAllowMissingOffset(name) : SingleFilter<'ctx, DateTimeOffset> =
    SingleFilter<'ctx, DateTimeOffset>(name, fun _ -> parseDateTimeOffsetAllowMissingOffset >> Task.result)

  static member Enum(name, enumMap: (string * 'a) list) : SingleFilter<'ctx, 'a> =
    let d = dict enumMap
    let allowed = enumMap |> List.map fst |> List.distinct
    let parse (data: ParsedValueFromQueryData) =
      match d.TryGetValue data.Value with
      | true, v -> Ok v
      | false, _ -> Error [invalidEnum (FromQuery data) data.Value allowed]
    SingleFilter<'ctx, 'a>(name, fun _ d -> parse d |> Task.result)




[<AutoOpen>]
module FilterExtensions =


  type Filter with

    static member Parsed(name: string, parse: 'ctx -> string -> 'a) : SingleFilter<'ctx, 'a> =
      SingleFilter<'ctx, 'a>(name, fun ctx d -> parse ctx d.Value |> Ok |> Task.result)



type Sort =

  static member private ParsedTaskRes'(parse: 'ctx -> ParsedValueFromQueryData -> Task<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    SingleSort<'ctx, 'a>(parse)

  static member ParsedTaskRes(parse: 'ctx -> string -> Task<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes'(fun ctx d -> parse ctx d.Value)

  static member ParsedTaskRes(parse: string -> Task<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(fun _ s -> parse s)

  static member ParsedTaskRes(parse: 'ctx -> string -> Task<Result<'a, string>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes'(fun ctx d -> parse ctx d.Value |> TaskResult.mapError (invalidParsedErrMsg (FromQuery d) >> List.singleton))

  static member ParsedTaskRes(parse: string -> Task<Result<'a, string>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(fun _ s -> parse s)

  static member ParsedTaskRes(parse: 'ctx -> string -> Task<Result<'a, string list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes'(fun ctx d -> parse ctx d.Value |> TaskResult.mapError (List.map (invalidParsedErrMsg (FromQuery d))))

  static member ParsedTaskRes(parse: string -> Task<Result<'a, string list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(fun _ s -> parse s)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, Error list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.liftAsync parse)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, string>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, string>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.liftAsync parse)

  static member ParsedAsyncRes(parse: 'ctx -> string -> Async<Result<'a, string list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.liftAsync2 parse)

  static member ParsedAsyncRes(parse: string -> Async<Result<'a, string list>>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.liftAsync parse)

  static member ParsedTaskOpt(parse: 'ctx -> string -> Task<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes'(fun ctx d -> parse ctx d.Value |> Task.map (Result.requireSome [invalidParsedNone (FromQuery d)]))

  static member ParsedTaskOpt(parse: string -> Task<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskOpt(fun _ s -> parse s)

  static member ParsedAsyncOpt(parse: 'ctx -> string -> Async<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskOpt(Task.liftAsync2 parse)

  static member ParsedAsyncOpt(parse: string -> Async<'a option>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskOpt(Task.liftAsync parse)

  static member ParsedTask(parse: 'ctx -> string -> Task<'a>) : SingleSort<'ctx, 'a> =
    SingleSort<'ctx, 'a>(fun ctx d -> parse ctx d.Value |> Task.map Ok)

  static member ParsedTask(parse: string -> Task<'a>) : SingleSort<'ctx, 'a> =
    SingleSort<'ctx, 'a>(TaskResult.liftTask2 (fun _ d -> parse d.Value))

  static member ParsedAsync(parse: 'ctx -> string -> Async<'a>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTask(Task.liftAsync2 parse)

  static member ParsedAsync(parse: string -> Async<'a>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTask(Task.liftAsync parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, Error list>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, Error list>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.lift parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, string>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, string>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.lift parse)

  static member ParsedRes(parse: 'ctx -> string -> Result<'a, string list>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.lift2 parse)

  static member ParsedRes(parse: string -> Result<'a, string list>) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskRes(Task.lift parse)

  static member ParsedOpt(parse: 'ctx -> string -> 'a option) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskOpt(Task.lift2 parse)

  static member ParsedOpt(parse: string -> 'a option) : SingleSort<'ctx, 'a> =
    Sort.ParsedTaskOpt(Task.lift parse)

  static member Parsed(parse: string -> 'a) : SingleSort<'ctx, 'a> =
    SingleSort<'ctx, 'a>(fun _ d -> parse d.Value |> Ok |> Task.result)

  static member Enum(sortMap: (string * 'a) list) : SingleSort<'ctx, 'a> =
    let d = dict sortMap
    let allowed = sortMap |> List.map fst |> List.distinct
    let parse (data: ParsedValueFromQueryData) =
      match d.TryGetValue data.Value with
      | true, v -> Ok v
      | false, _ -> Error [invalidEnum (FromQuery data) data.Value allowed]
    SingleSort<'ctx, 'a>(fun _ d -> parse d |> Task.result)



[<AutoOpen>]
module SortExtensions =


  type Sort with

    static member Parsed(parse: 'ctx -> string -> 'a) : SingleSort<'ctx, 'a> =
      SingleSort<'ctx, 'a>(fun ctx d -> parse ctx d.Value |> Ok |> Task.result)



type Page<'ctx> =

  static member Offset : PageParam<'ctx> = PageParam<'ctx>("offset", min=0)

  static member Limit : PageParam<'ctx> = PageParam<'ctx>("limit", min=1)

  static member Number : PageParam<'ctx> = PageParam<'ctx>("number", min=0)

  static member Size : PageParam<'ctx> = PageParam<'ctx>("size", min=1)



type Query =

  static member ParsedTaskRes(queryParamName, parse: 'ctx -> string -> Task<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, fun ctx d -> parse ctx d.Value)

  static member ParsedTaskRes(queryParamName, parse: string -> Task<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, fun _ s -> parse s)

  static member ParsedTaskRes(queryParamName, parse: 'ctx -> string -> Task<Result<'a, string>>) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, fun ctx d -> parse ctx d.Value |> TaskResult.mapError (invalidParsedErrMsg (FromQuery d) >> List.singleton))

  static member ParsedTaskRes(queryParamName, parse: string -> Task<Result<'a, string>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, fun _ s -> parse s)

  static member ParsedTaskRes(queryParamName, parse: 'ctx -> string -> Task<Result<'a, string list>>) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, fun ctx d -> parse ctx d.Value |> TaskResult.mapError (List.map (invalidParsedErrMsg (FromQuery d))))

  static member ParsedTaskRes(queryParamName, parse: string -> Task<Result<'a, string list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, Error list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync parse)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, string>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, string>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync parse)

  static member ParsedAsyncRes(queryParamName, parse: 'ctx -> string -> Async<Result<'a, string list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(queryParamName, parse: string -> Async<Result<'a, string list>>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.liftAsync parse)

  static member ParsedTaskOpt(queryParamName, parse: 'ctx -> string -> Task<'a option>) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, fun ctx d -> parse ctx d.Value |> Task.map (Result.requireSome [invalidParsedNone (FromQuery d)]))

  static member ParsedTaskOpt(queryParamName, parse: string -> Task<'a option>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskOpt(queryParamName, fun _ s -> parse s)

  static member ParsedAsyncOpt(queryParamName, parse: 'ctx -> string -> Async<'a option>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskOpt(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsyncOpt(queryParamName, parse: string -> Async<'a option>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskOpt(queryParamName, Task.liftAsync parse)

  static member ParsedTask(queryParamName, parse: 'ctx -> string -> Task<'a>) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, fun ctx d -> parse ctx d.Value |> Task.map Ok)

  static member ParsedTask(queryParamName, parse: string -> Task<'a>) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, fun _ d -> parse d.Value |> Task.map Ok)

  static member ParsedAsync(queryParamName, parse: 'ctx -> string -> Async<'a>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTask(queryParamName, Task.liftAsync2 parse)

  static member ParsedAsync(queryParamName, parse: string -> Async<'a>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTask(queryParamName, Task.liftAsync parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, Error list>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, Error list>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.lift parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, string>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, string>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.lift parse)

  static member ParsedRes(queryParamName, parse: 'ctx -> string -> Result<'a, string list>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.lift2 parse)

  static member ParsedRes(queryParamName, parse: string -> Result<'a, string list>) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskRes(queryParamName, Task.lift parse)

  static member ParsedOpt(queryParamName, parse: 'ctx -> string -> 'a option) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskOpt(queryParamName, Task.lift2 parse)

  static member ParsedOpt(queryParamName, parse: string -> 'a option) : CustomQueryParam<'ctx, 'a> =
    Query.ParsedTaskOpt(queryParamName, Task.lift parse)

  static member Parsed(queryParamName, parse: string -> 'a) : CustomQueryParam<'ctx, 'a> =
    CustomQueryParam<'ctx, 'a>(queryParamName, fun _ d -> parse d.Value |> Ok |> Task.result)

  // Note: When adding more overloads, consider adding them to Filter, too.

  static member String(queryParamName) : CustomQueryParam<'ctx, string> =
    CustomQueryParam<'ctx, string>(queryParamName, fun _ d -> d.Value |> Ok |> Task.result)

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

  static member Enum(queryParamName, enumMap: (string * 'a) list) : CustomQueryParam<'ctx, 'a> =
    let d = dict enumMap
    let allowed = enumMap |> List.map fst |> List.distinct
    let parse (data: ParsedValueFromQueryData) =
      match d.TryGetValue data.Value with
      | true, v -> Ok v
      | false, _ -> Error [invalidEnum (FromQuery data) data.Value allowed]
    CustomQueryParam<'ctx, 'a>(queryParamName, fun _ s -> parse s |> Task.result)



[<AutoOpen>]
module QueryExtensions =


  type Query with

    static member Parsed(queryParamName, parse: 'ctx -> string -> 'a) : CustomQueryParam<'ctx, 'a> =
      CustomQueryParam<'ctx, 'a>(queryParamName, fun ctx d -> parse ctx d.Value |> Ok |> Task.result)



type Header =

  static member String(headerName) : Header<'ctx, string> =
    Header<'ctx, string>(headerName, fun _ d -> d.Value |> Ok |> Task.result)

  static member ParsedTaskRes(headerName, parse: 'ctx -> string -> Task<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, fun ctx d -> parse ctx d.Value)

  static member ParsedTaskRes(headerName, parse: string -> Task<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, fun _ s -> parse s)

  static member ParsedTaskRes(headerName, parse: 'ctx -> string -> Task<Result<'a, string>>) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, fun ctx d -> parse ctx d.Value |> TaskResult.mapError (invalidParsedErrMsg (FromHeader d) >> List.singleton))

  static member ParsedTaskRes(headerName, parse: string -> Task<Result<'a, string>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, fun _ s -> parse s)

  static member ParsedTaskRes(headerName, parse: 'ctx -> string -> Task<Result<'a, string list>>) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, fun ctx d -> parse ctx d.Value |> TaskResult.mapError (List.map (invalidParsedErrMsg (FromHeader d))))

  static member ParsedTaskRes(headerName, parse: string -> Task<Result<'a, string list>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, fun _ s -> parse s)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, Error list>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.liftAsync parse)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, string>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, string>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.liftAsync parse)

  static member ParsedAsyncRes(headerName, parse: 'ctx -> string -> Async<Result<'a, string list>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncRes(headerName, parse: string -> Async<Result<'a, string list>>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.liftAsync parse)

  static member ParsedTaskOpt(headerName, parse: 'ctx -> string -> Task<'a option>) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, fun ctx d -> parse ctx d.Value |> Task.map (Result.requireSome [invalidParsedNone (FromHeader d)]))

  static member ParsedTaskOpt(headerName, parse: string -> Task<'a option>) : Header<'ctx, 'a> =
    Header.ParsedTaskOpt(headerName, fun _ s -> parse s)

  static member ParsedAsyncOpt(headerName, parse: 'ctx -> string -> Async<'a option>) : Header<'ctx, 'a> =
    Header.ParsedTaskOpt(headerName, Task.liftAsync2 parse)

  static member ParsedAsyncOpt(headerName, parse: string -> Async<'a option>) : Header<'ctx, 'a> =
    Header.ParsedTaskOpt(headerName, Task.liftAsync parse)

  static member ParsedTask(headerName, parse: 'ctx -> string -> Task<'a>) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, fun ctx d -> parse ctx d.Value |> Task.map Ok)

  static member ParsedTask(headerName, parse: string -> Task<'a>) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, fun _ d -> parse d.Value |> Task.map Ok)

  static member ParsedAsync(headerName, parse: 'ctx -> string -> Async<'a>) : Header<'ctx, 'a> =
    Header.ParsedTask(headerName, Task.liftAsync2 parse)

  static member ParsedAsync(headerName, parse: string -> Async<'a>) : Header<'ctx, 'a> =
    Header.ParsedTask(headerName, Task.liftAsync parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, Error list>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, Error list>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.lift parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, string>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, string>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.lift parse)

  static member ParsedRes(headerName, parse: 'ctx -> string -> Result<'a, string list>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.lift2 parse)

  static member ParsedRes(headerName, parse: string -> Result<'a, string list>) : Header<'ctx, 'a> =
    Header.ParsedTaskRes(headerName, Task.lift parse)

  static member ParsedOpt(headerName, parse: 'ctx -> string -> 'a option) : Header<'ctx, 'a> =
    Header.ParsedTaskOpt(headerName, Task.lift2 parse)

  static member ParsedOpt(headerName, parse: string -> 'a option) : Header<'ctx, 'a> =
    Header.ParsedTaskOpt(headerName, Task.lift parse)

  static member Parsed(headerName, parse: string -> 'a) : Header<'ctx, 'a> =
    Header<'ctx, 'a>(headerName, fun _ d -> parse d.Value |> Ok |> Task.result)



[<AutoOpen>]
module HeaderExtensions =


  type Header with

    static member Parsed(headerName, parse: 'ctx -> string -> 'a) : Header<'ctx, 'a> =
      Header<'ctx, 'a>(headerName, fun ctx d -> parse ctx d.Value |> Ok |> Task.result)
