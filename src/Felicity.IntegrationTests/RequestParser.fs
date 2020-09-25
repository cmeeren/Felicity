module ``RequestParser``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type A = A
type B = B
type C = C
type D = D
type X = X

type NonEmptyString = NonEmptyString of string with
  static member create x = if x = "" then None else Some (NonEmptyString x)
  static member value (NonEmptyString x) = x

type NonNegativeInt = NonNegativeInt of int with
  static member create x = if x < 0 then None else Some (NonNegativeInt x)
  static member value (NonNegativeInt x) = x

type NonNegativeFloat = NonNegativeFloat of float with
  static member create x = if x < 0. then None else Some (NonNegativeFloat x)
  static member value (NonNegativeFloat x) = x

type TrueBool = TrueBool of bool with
  static member create x = if x = false then None else Some (TrueBool x)
  static member value (TrueBool x) = x


type Ctx =
  { GetReqParser: RequestParserHelper<Ctx> -> RequestParser<Ctx, unit> }
  static member Create getReqParser =
    { GetReqParser = getReqParser }


module D =

  let define = Define<Ctx, D, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("d", resId)
  let lookup = define.Operation.Lookup(fun _ -> Some D)
  let attr = define.Attribute.SimpleString().Get(fun _ -> "foo")


module C =

  let define = Define<Ctx, C, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("c", resId)
  let lookup = define.Operation.Lookup(fun _ -> Some C)
  let d = define.Relationship.ToOne(D.resDef).Get(fun _ _ -> D)
  let attr = define.Attribute.SimpleString().Get(fun _ -> "foo")


module B =

  let define = Define<Ctx, B, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("b", resId)
  let lookup = define.Operation.Lookup(fun _ -> Some B)
  let c = define.Relationship.ToOne(C.resDef).Get(fun _ _ -> C)
  let attr = define.Attribute.SimpleString().Get(fun _ -> "foo")


module A =

  let define = Define<Ctx, A, NonEmptyString>()
  let resId = define.Id.ParsedOpt(NonEmptyString.value, NonEmptyString.create, fun _ -> NonEmptyString "someId")
  let resDef = define.Resource("a", resId)
  let lookup = define.Operation.Lookup(fun _ -> Some A)
  let b = define.Relationship.ToOne(B.resDef).Get(fun _ _ -> B)
  let attr = define.Attribute.SimpleString().Get(fun _ -> "foo")


module X =

  let define = Define<Ctx, X, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("x", resId).CollectionName("xs")

  let nonEmptyString =
    define.Attribute
      .ParsedOpt(NonEmptyString.value, NonEmptyString.create)
      .Get(fun _ -> NonEmptyString "foo")

  let nonNegativeInt =
    define.Attribute
      .ParsedOpt(NonNegativeInt.value, NonNegativeInt.create)
      .Get(fun _ -> NonNegativeInt 1)

  let nonNegativeFloat =
    define.Attribute
      .ParsedOpt(NonNegativeFloat.value, NonNegativeFloat.create)
      .Get(fun _ -> NonNegativeFloat 1.)

  let trueBool =
    define.Attribute
      .ParsedOpt(TrueBool.value, TrueBool.create)
      .Get(fun _ -> TrueBool true)

  let nullableNonEmptyString =
    define.Attribute
      .Nullable
      .ParsedOpt(NonEmptyString.value, NonEmptyString.create)
      .Get(fun _ -> None)

  let alternativeALookup = Define<Ctx,string,NonEmptyString>().Operation.Lookup(fun (NonEmptyString id) -> Some id)

  let a = define.Relationship.ToOne(A.resDef).Get(fun _ _ -> A)

  let getColl = define.Operation.GetCollection(fun ctx parser -> (ctx.GetReqParser parser).Map(fun () -> []))
  let post = define.Operation.Post(fun ctx parser -> (ctx.GetReqParser parser).Map(fun () -> X)).AfterCreate(ignore)


type XSearchArgs =
  { nonEmptyString: NonEmptyString
    nonNegativeIntLe: NonNegativeInt option
    nonNegativeIntGe: NonNegativeInt option
    nullableNonEmptyString: NonEmptyString option }
  static member Create nonEmptyString =
    { nonEmptyString = nonEmptyString
      nonNegativeIntLe = None
      nonNegativeIntGe = None
      nullableNonEmptyString = None }
  static member setNonNegativeIntLe i args =
    { args with nonNegativeIntLe = Some i }
  static member setNonNegativeIntGe i args =
    { args with nonNegativeIntGe = Some i }
  static member setNullableNonEmptyString s args =
    { args with nullableNonEmptyString = Some s }



[<Tests>]
let tests =
  testList "RequestParser" [

    testJob "Can parse a required single string filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.nonEmptyString))
      )
      let! response = Request.get ctx "/xs?filter[nonEmptyString]=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val") @>
    }

    testJob "Can parse a required single int filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.nonNegativeInt))
      )
      let! response = Request.get ctx "/xs?filter[nonNegativeInt]=2" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonNegativeInt 2) @>
    }

    testJob "Can parse a required single float filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.nonNegativeFloat))
      )
      let! response = Request.get ctx "/xs?filter[nonNegativeFloat]=2" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonNegativeFloat 2.) @>
    }

    testJob "Can parse a required single bool filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.trueBool))
      )
      let! response = Request.get ctx "/xs?filter[trueBool]=true" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (TrueBool true) @>
    }

    testJob "Can parse a required single relationship ID filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.a))
      )
      let! response = Request.get ctx "/xs?filter[a]=someId" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "someId") @>
    }

    testJob "Can parse a required filter with operator" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.nonEmptyString).Operator("eq"))
      )
      let! response = Request.get ctx "/xs?filter[nonEmptyString][eq]=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val") @>
    }

    testJob "Can parse a required bool-override filter with operator" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.nonEmptyString).Operator("isEmpty").Bool)
      )
      let! response = Request.get ctx "/xs?filter[nonEmptyString][isEmpty]=false" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some false @>
    }

    testJob "Can parse a required list filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.nonEmptyString).List)
      )
      let! response = Request.get ctx "/xs?filter[nonEmptyString]=val1,val2" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some ([NonEmptyString "val1"; NonEmptyString "val2"]) @>
    }

    testJob "Can parse a required 1-level resource path filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.a, A.attr))
      )
      let! response = Request.get ctx "/xs?filter[a.attr]=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some "val" @>
    }

    testJob "Can parse a required 2-level resource path filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.a, A.b, B.attr))
      )
      let! response = Request.get ctx "/xs?filter[a.b.attr]=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some "val" @>
    }

    testJob "Can parse a required 3-level resource path filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.a, A.b, B.c, C.attr))
      )
      let! response = Request.get ctx "/xs?filter[a.b.c.attr]=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some "val" @>
    }

    testJob "Can parse a required nullable filter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Field(X.nullableNonEmptyString))
      )
      let! response = Request.get ctx "/xs?filter[nullableNonEmptyString]=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val") @>
    }

    testJob "Can parse a required nullable field" {
      let mutable calledWith = ValueNone
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString)
      )
      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {| data = {| ``type`` = "x"; attributes = {| nullableNonEmptyString = "val" |} |} |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = ValueSome (Some (NonEmptyString "val")) @>
    }

    testJob "Can parse a required nullable field set to null" {
      let mutable calledWith = ValueNone
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString)
      )
      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {| data = {| ``type`` = "x"; attributes = {| nullableNonEmptyString = null |} |} |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = ValueSome None @>
    }

    testJob "Returns 400 if required nullable field is missing" {
      let ctx = Ctx.Create (fun parser -> parser.For(ignore, X.nullableNonEmptyString))
      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {| data = {| ``type`` = "x"; attributes = obj() |} |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Attribute 'nullableNonEmptyString' is required for this operation" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Can parse an optional nullable field" {
      let mutable calledWith = ValueNone
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.Optional)
      )
      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {| data = {| ``type`` = "x"; attributes = {| nullableNonEmptyString = "val" |} |} |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = ValueSome (Some (Some (NonEmptyString "val"))) @>
    }

    testJob "Can parse an optional nullable field set to null" {
      let mutable calledWith = ValueNone
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.Optional)
      )
      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {| data = {| ``type`` = "x"; attributes = {| nullableNonEmptyString = null |} |} |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = ValueSome (Some None) @>
    }

    testJob "Can parse an optional nullable field that is not present" {
      let mutable calledWith = ValueNone
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.Optional)
      )
      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {| data = {| ``type`` = "x"; attributes = obj () |} |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = ValueSome None @>
    }

    testJob "Can parse a related resource using an alternative lookup" {
      let mutable calledWith = ValueNone

      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- ValueSome x), X.a.Related(X.alternativeALookup))
      )
      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {| data = {| ``type`` = "x"; relationships = {| a = {| data = {| ``type`` = "a"; id = "someId" |} |} |} |} |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = ValueSome "someId" @>
    }

    testJob "Returns 400 if required single filter value contains comma" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Filter.Field(X.nonEmptyString))
      )
      let! response = Request.get ctx "/xs?filter[nonEmptyString]=val1,val2" |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'filter[nonEmptyString]' only accepts a single value, but got 2 comma-separated values" @>
      test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Can parse a required single parsed sort ascending" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Sort.Parsed(fun x -> NonEmptyString x))
      )
      let! response = Request.get ctx "/xs?sort=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val", false) @>
    }

    testJob "Can parse a required single parsed sort descending" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Sort.Parsed(fun x -> NonEmptyString x))
      )
      let! response = Request.get ctx "/xs?sort=-val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val", true) @>
    }

    testJob "Can parse a required single enum sort" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Sort.Enum(["1", 1]))
      )
      let! response = Request.get ctx "/xs?sort=1" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (1, false) @>
    }

    testJob "Can parse a required list sort" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Sort.Enum(["1", 1; "2", 2]).List)
      )
      let! response = Request.get ctx "/xs?sort=1,-2" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some [(1, false); (2, true)] @>
    }

    testJob "Returns 400 if enum sort has invalid value" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Sort.Enum(["1", 1; "2", 2]).List)
      )
      let! response = Request.get ctx "/xs?sort=3,-4" |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'sort' got invalid value '3'; expected one of '1', '2'" @>
      test <@ json |> getPath "errors[0].source.parameter" = "sort" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Query parameter 'sort' got invalid value '4'; expected one of '1', '2'" @>
      test <@ json |> getPath "errors[1].source.parameter" = "sort" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 if single sort value contains comma" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Sort.Enum(["1", 1; "2", 2]))
      )
      let! response = Request.get ctx "/xs?sort=1,2,3" |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'sort' only accepts a single value, but got 3 comma-separated values" @>
      test <@ json |> getPath "errors[0].source.parameter" = "sort" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Can parse page parameters" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun offset limit number size -> calledWith <- Some {| Offset = offset; Limit = limit; Number = number; Size = size |}), Page.Offset, Page.Limit, Page.Number, Page.Size)
      )
      let! response = Request.get ctx "/xs?page[offset]=4&page[limit]=5&page[number]=6&page[size]=7" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some {| Offset = 4; Limit = 5; Number = 6; Size = 7 |} @>
    }

    testJob "Returns 400 if page has invalid value" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Offset)
      )
      let! response = Request.get ctx "/xs?page[offset]=invalid" |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'page[offset]' expected an integer, but got 'invalid'" @>
      test <@ json |> getPath "errors[0].source.parameter" = "page[offset]" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 if page parameter is too small" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Offset.Min(10))
      )
      let! response = Request.get ctx "/xs?page[offset]=9" |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'page[offset]' has minimum value 10, but got 9" @>
      test <@ json |> getPath "errors[0].source.parameter" = "page[offset]" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 if page parameter is too large" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Offset.Max(10))
      )
      let! response = Request.get ctx "/xs?page[offset]=11" |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'page[offset]' has maximum value 10, but got 11" @>
      test <@ json |> getPath "errors[0].source.parameter" = "page[offset]" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns OK if page parameter is at minimum" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Offset.Min(10))
      )
      let! response = Request.get ctx "/xs?page[offset]=10" |> getResponse

      response |> testSuccessStatusCode
    }

    testJob "Returns OK if page parameter is at maximum" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Offset.Max(10))
      )
      let! response = Request.get ctx "/xs?page[offset]=10" |> getResponse

      response |> testSuccessStatusCode
    }

    testJob "page[offset] has correct default min/max values" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Offset)
      )

      let! response = Request.get ctx "/xs?page[offset]=0" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[offset]=2147483647" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[offset]=-1" |> getResponse
      response |> testStatusCode 400
    }

    testJob "page[limit] has correct default min/max values" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Limit)
      )

      let! response = Request.get ctx "/xs?page[limit]=1" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[limit]=2147483647" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[limit]=0" |> getResponse
      response |> testStatusCode 400
    }

    testJob "page[number] has correct default min/max values" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Number)
      )

      let! response = Request.get ctx "/xs?page[number]=0" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[number]=2147483647" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[number]=-1" |> getResponse
      response |> testStatusCode 400
    }

    testJob "page[size] has correct default min/max values" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(ignore, Page.Size)
      )

      let! response = Request.get ctx "/xs?page[size]=1" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[size]=2147483647" |> getResponse
      response |> testSuccessStatusCode

      let! response = Request.get ctx "/xs?page[size]=0" |> getResponse
      response |> testStatusCode 400
    }

    testJob "Can parse custom query parameters" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Query.Parsed("customParam", NonEmptyString))
      )
      let! response = Request.get ctx "/xs?customParam=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val") @>
    }

    testJob "Can parse a custom query filter parameter" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Filter.Parsed("custom", NonEmptyString))
      )
      let! response = Request.get ctx "/xs?filter[custom]=val" |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val") @>
    }

    testJob "Can parse headers" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun x -> calledWith <- Some x), Header.Parsed("HeaderName", NonEmptyString))
      )
      let! response =
        Request.get ctx "/xs"
        |> Request.setHeader (Custom ("HeaderName", "val"))
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val") @>
    }

    testJob "Does not fail if optional parameters are missing" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(())
          .Add((fun _ () -> ()), X.a)
          .Add((fun _ () -> ()), X.nonEmptyString)
          .Add((fun _ () -> ()), Filter.Field(X.a))
          .Add((fun _ () -> ()), Sort.Enum([]))
          .Add((fun _ () -> ()), Page.Offset)
          .Add((fun _ () -> ()), Query.String("customParam"))
          .Add((fun _ () -> ()), Header.String("customHeader"))
      )
      let! response =
        Request.get ctx "/xs"
        |> getResponse

      response |> testSuccessStatusCode
    }

    testJob "Returns 400 for each required parameter that is missing or invalid and each optional parameter that is invalid" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun _ _ _ _ -> ()), Filter.Field(X.nonEmptyString), X.nonNegativeFloat, X.a, Query.String("customParam1"))
          .Add((fun _ () -> ()), Filter.Field(X.nonNegativeInt))
          .Add((fun _ () -> ()), Page.Offset)
          .Add((fun _ () -> ()), Header.ParsedRes("HeaderName", fun _ -> Error [Error.create 422 |> Error.setCode "custom"]))
      )
      let! response =
        Request.get ctx "/xs?filter[nonEmptyString]=&filter[nonNegativeInt]=-1&page[offset]=-1"
        |> Request.setHeader (Custom ("HeaderName", "val"))
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "This is not a valid value for attribute 'nonEmptyString'" @>
      test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Attribute 'nonNegativeFloat' is required for this operation" @>
      test <@ json |> getPath "errors[1].source.pointer" = "" @>
      test <@ json |> getPath "errors[2].status" = "400" @>
      test <@ json |> getPath "errors[2].detail" = "Relationship 'a' is required for this operation" @>
      test <@ json |> getPath "errors[2].source.pointer" = "" @>
      test <@ json |> getPath "errors[3].status" = "400" @>
      test <@ json |> getPath "errors[3].detail" = "Query parameter 'customParam1' is required for this operation" @>
      test <@ json |> getPath "errors[3].source.parameter" = "customParam1" @>
      test <@ json |> getPath "errors[4].status" = "400" @>
      test <@ json |> getPath "errors[4].detail" = "This is not a valid value for attribute 'nonNegativeInt'" @>
      test <@ json |> getPath "errors[4].source.parameter" = "filter[nonNegativeInt]" @>
      test <@ json |> getPath "errors[5].status" = "400" @>
      test <@ json |> getPath "errors[5].detail" = "Query parameter 'page[offset]' has minimum value 0, but got -1" @>
      test <@ json |> getPath "errors[5].source.parameter" = "page[offset]" @>
      test <@ json |> getPath "errors[6].status" = "422" @>
      test <@ json |> getPath "errors[6].code" = "custom" @>
      test <@ json |> hasNoPath "errors[6].source" @>
      test <@ json |> hasNoPath "errors[7]" @>
    }

    testJob "Names are case sensitive" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For((fun _ _ -> ()), Filter.Field(X.nonEmptyString), X.nonNegativeFloat)
      )
      let! response =
        Request.post ctx "/xs?filter[NonEmptyString]=foo"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "x"
                  attributes = {| NonNegativeFloat = 1. |}
                |}
            |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'filter[nonEmptyString]' is required for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Attribute 'nonNegativeFloat' is required for this operation" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/attributes" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 for each prohibited query parameter, field, and header that is present" {
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(())
          .Prohibit(Filter.Field(X.nonEmptyString))
          .Prohibit(X.nonNegativeInt)
          .Prohibit(X.a)
          .Prohibit(Header.String("HeaderName"))
      )
      let! response =
        Request.post ctx "/xs?filter[nonEmptyString]=val"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "x"
                  attributes = {| nonNegativeInt = 2 |}
                  relationships = {| a = {| data = {| ``type`` = "a"; id = "someId" |} |} |}
                |}
            |}
        |> Request.setHeader (Custom ("HeaderName", "val"))
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Query parameter 'filter[nonEmptyString]' is not allowed for this operation" @>
      test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Attribute 'nonNegativeInt' is not allowed for this operation" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/attributes/nonNegativeInt" @>
      test <@ json |> getPath "errors[2].status" = "400" @>
      test <@ json |> getPath "errors[2].detail" = "Relationship 'a' is not allowed for this operation" @>
      test <@ json |> getPath "errors[2].source.pointer" = "/data/relationships/a" @>
      test <@ json |> getPath "errors[3].status" = "400" @>
      test <@ json |> getPath "errors[3].detail" = "Header 'HeaderName' is not allowed for this operation" @>
      test <@ json |> hasNoPath "errors[3].source" @>
      test <@ json |> hasNoPath "errors[4]" @>
    }

    testJob "Successfully parses GET collection example" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(XSearchArgs.Create, Filter.Field(X.nonEmptyString))
          .Add(XSearchArgs.setNonNegativeIntLe, Filter.Field(X.nonNegativeInt).Operator("le"))
          .Add(XSearchArgs.setNonNegativeIntGe, Filter.Field(X.nonNegativeInt).Operator("ge"))
          .Add(XSearchArgs.setNullableNonEmptyString, Filter.Field(X.nullableNonEmptyString))
          .Map(fun args -> calledWith <- Some args; ())
      )

      let! response = Request.get ctx "/xs?filter[nonEmptyString]=foo&filter[nonNegativeInt][ge]=2&filter[nullableNonEmptyString]=bar" |> getResponse

      let expected =
        { nonEmptyString = NonEmptyString "foo"
          nonNegativeIntLe = None
          nonNegativeIntGe = Some (NonNegativeInt 2)
          nullableNonEmptyString = Some (NonEmptyString "bar") }

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some expected @>
    }

    testJob "Successfully parses POST collection example" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(id, X.nonEmptyString)
          .Map(fun args -> calledWith <- Some args; ())
      )

      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "x"
                  attributes = {| nonEmptyString = "val" |}
                |}
            |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some (NonEmptyString "val") @>
    }

    testJob "Can use optional getters as required option-wrapped params" {
      let mutable calledWith = None
      let ctx = Ctx.Create (fun parser ->
        parser
          .For(id, X.nonEmptyString.Optional)
          .Map(fun args -> calledWith <- Some args; ())
      )

      let! response =
        Request.post ctx "/xs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "x" |}
            |}
        |> getResponse

      response |> testSuccessStatusCode
      let calledWith' = calledWith
      test <@ calledWith' = Some None @>
    }

    testJob "Can resolve non-context overloads" {
      // Compile-time tests
      let f () =
        let parser : RequestParserHelper<unit> = failwith "Not called"
        let setString (_: string) () = ()
        let setStringBool (_: string * bool) () = ()
        let parseRes : string -> Result<_, Error list> = failwith "Not called"
        let parseResString : string -> Result<_, string> = failwith "Not called"
        parser
          .For(())
          .Add(setString, Filter.Parsed("", id))
          .Add(setString, Filter.ParsedOpt("", Some))
          .Add(setString, Filter.ParsedRes("", parseRes))
          .Add(setString, Filter.ParsedRes("", parseResString))
          .Add(setStringBool, Sort.Parsed(id))
          .Add(setStringBool, Sort.ParsedOpt(Some))
          .Add(setStringBool, Sort.ParsedRes(parseRes))
          .Add(setStringBool, Sort.ParsedRes(parseResString))
          .Add(setString, Query.Parsed("", id))
          .Add(setString, Query.ParsedOpt("", Some))
          .Add(setString, Query.ParsedRes("", parseRes))
          .Add(setString, Query.ParsedRes("", parseResString))
          .Add(setString, Header.Parsed("", id))
          .Add(setString, Header.ParsedOpt("", Some))
          .Add(setString, Header.ParsedRes("", parseRes))
          .Add(setString, Header.ParsedRes("", parseResString))
        |> ignore
      ignore f
    }

  ]
