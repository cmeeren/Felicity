module RequestParser

open System
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type A = A
type B = B
type C = C
type D = D
type X = X

type NonEmptyString =
    | NonEmptyString of string

    static member create x =
        if x = "" then None else Some(NonEmptyString x)

    static member value(NonEmptyString x) = x

type NonNegativeInt =
    | NonNegativeInt of int

    static member create x =
        if x < 0 then None else Some(NonNegativeInt x)

    static member value(NonNegativeInt x) = x

type NonNegativeFloat =
    | NonNegativeFloat of float

    static member create x =
        if x < 0. then None else Some(NonNegativeFloat x)

    static member value(NonNegativeFloat x) = x

type TrueBool =
    | TrueBool of bool

    static member create x =
        if x = false then None else Some(TrueBool x)

    static member value(TrueBool x) = x


type Ctx = {
    GetReqParser: RequestParserHelper<Ctx> -> RequestParser<Ctx, unit>
} with

    static member Create getReqParser = { GetReqParser = getReqParser }


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

    let resId =
        define.Id.ParsedOpt(NonEmptyString.value, NonEmptyString.create, (fun _ -> NonEmptyString "someId"))

    let resDef = define.Resource("a", resId)
    let lookup = define.Operation.Lookup(fun _ -> Some A)
    let b = define.Relationship.ToOne(B.resDef).Get(fun _ _ -> B)
    let attr = define.Attribute.SimpleString().Get(fun _ -> "foo")


module X =

    let define = Define<Ctx, X, string>()
    let resId = define.Id.Simple(fun _ -> "someId")
    let resDef = define.Resource("x", resId).CollectionName("xs")

    let nonEmptyString =
        define.Attribute.ParsedOpt(NonEmptyString.value, NonEmptyString.create).Get(fun _ -> NonEmptyString "foo")

    let nonNegativeInt =
        define.Attribute.ParsedOpt(NonNegativeInt.value, NonNegativeInt.create).Get(fun _ -> NonNegativeInt 1)

    let nonNegativeFloat =
        define.Attribute.ParsedOpt(NonNegativeFloat.value, NonNegativeFloat.create).Get(fun _ -> NonNegativeFloat 1)

    let trueBool =
        define.Attribute.ParsedOpt(TrueBool.value, TrueBool.create).Get(fun _ -> TrueBool true)

    let nullableNonEmptyString =
        define.Attribute.Nullable.ParsedOpt(NonEmptyString.value, NonEmptyString.create).Get(fun _ -> None)

    let dateTimeOffset = define.Attribute.SimpleDateTimeOffset()

    let dateTimeOffsetAllowMissingOffset =
        define.Attribute.SimpleDateTimeOffsetAllowMissingOffset()

    let alternativeALookup =
        Define<Ctx, string, NonEmptyString>().Operation.Lookup(fun (NonEmptyString id) -> Some id)

    let a = define.Relationship.ToOne(A.resDef).Get(fun _ _ -> A)

    let getColl =
        define.Operation.GetCollection(fun ctx parser -> (ctx.GetReqParser parser).Map(fun () -> []))

    let post =
        define.Operation.Post(fun ctx parser -> (ctx.GetReqParser parser).Map(fun () -> X)).AfterCreate(ignore)


type XSearchArgs = {
    nonEmptyString: NonEmptyString
    nonNegativeIntLe: NonNegativeInt option
    nonNegativeIntGe: NonNegativeInt option
    nullableNonEmptyString: NonEmptyString option
} with

    static member Create nonEmptyString = {
        nonEmptyString = nonEmptyString
        nonNegativeIntLe = None
        nonNegativeIntGe = None
        nullableNonEmptyString = None
    }

    static member setNonNegativeIntLe i args = { args with nonNegativeIntLe = Some i }
    static member setNonNegativeIntGe i args = { args with nonNegativeIntGe = Some i }

    static member setNullableNonEmptyString s args = {
        args with
            nullableNonEmptyString = Some s
    }



module Y =


    let define = Define<Ctx, X, string>()
    let resId = define.Id.Simple(fun _ -> "someId")
    let resDef = define.Resource("y", resId).CollectionName("ys")

    let internal resIdOpt =
        define.Id.ParsedOpt((fun _ -> ""), (fun _ -> None), (fun _ -> ""))

    let internal resDefOpt = define.Resource("y", resIdOpt)

    let internal resIdRes =
        define.Id.ParsedRes((fun _ -> ""), (fun _ -> Error "Custom message"), (fun _ -> ""))

    let internal resDefRes = define.Resource("y", resIdRes)

    let nonNullableOpt =
        define.Attribute.ParsedOpt((fun _ -> ""), (fun _ _ -> (None: string option)))

    let nonNullableRes =
        define.Attribute.ParsedRes((fun _ -> ""), (fun _ _ -> (Error "Custom message": Result<string, string>)))

    let nonNullableEnum = define.Attribute.Enum((fun _ -> ""), [ "a", 1; "b", 2 ])
    let nonNullableBool = define.Attribute.SimpleBool()
    let nonNullableInt = define.Attribute.SimpleInt()
    let nonNullableFloat = define.Attribute.SimpleFloat()
    let nonNullableDateTime = define.Attribute.SimpleDateTime()

    let nonNullableDateTimeOffsetAllowMissingOffset =
        define.Attribute.SimpleDateTimeOffsetAllowMissingOffset()

    let nonNullableDateTimeOffset = define.Attribute.SimpleDateTimeOffset()

    let nullableOpt =
        define.Attribute.Nullable.ParsedOpt((fun _ -> ""), (fun _ _ -> (None: string option)))

    let nullableRes =
        define.Attribute.Nullable.ParsedRes((fun _ -> ""), fun _ _ -> (Error "Custom message": Result<string, string>))

    let nullableEnum = define.Attribute.Nullable.Enum((fun _ -> ""), [ "a", 1; "b", 2 ])
    let nullableBool = define.Attribute.Nullable.SimpleBool()
    let nullableInt = define.Attribute.Nullable.SimpleInt()
    let nullableFloat = define.Attribute.Nullable.SimpleFloat()
    let nullableDateTime = define.Attribute.Nullable.SimpleDateTime()

    let nullableDateTimeOffsetAllowMissingOffset =
        define.Attribute.Nullable.SimpleDateTimeOffsetAllowMissingOffset()

    let nullableDateTimeOffset = define.Attribute.Nullable.SimpleDateTimeOffset()

    let toOneOpt = define.Relationship.ToOne(resDefOpt)
    let toOneRes = define.Relationship.ToOne(resDefRes)

    let toOneNullableOpt = define.Relationship.ToOneNullable(resDefOpt)
    let toOneNullableRes = define.Relationship.ToOneNullable(resDefRes)

    let toManyOpt = define.Relationship.ToMany(resDefOpt)
    let toManyRes = define.Relationship.ToMany(resDefRes)

    let getColl =
        define.Operation.GetCollection(fun ctx parser -> (ctx.GetReqParser parser).Map(fun () -> []))

    let post =
        define.Operation.Post(fun ctx parser -> (ctx.GetReqParser parser).Map(fun () -> X)).AfterCreate(ignore)



[<Tests>]
let tests =
    testList "RequestParser" [

        testJob "Can parse a required single string filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.nonEmptyString)))

            let! response = Request.get ctx "/xs?filter[nonEmptyString]=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val") @>
        }

        testJob "Can parse a required single int filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.nonNegativeInt)))

            let! response = Request.get ctx "/xs?filter[nonNegativeInt]=2" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonNegativeInt 2) @>
        }

        testJob "Can parse a required single float filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.nonNegativeFloat)))

            let! response = Request.get ctx "/xs?filter[nonNegativeFloat]=2" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonNegativeFloat 2) @>
        }

        testJob "Can parse a required single bool filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.trueBool)))

            let! response = Request.get ctx "/xs?filter[trueBool]=true" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(TrueBool true) @>
        }

        testJob "Can parse a required single relationship ID filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.a)))

            let! response = Request.get ctx "/xs?filter[a]=someId" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "someId") @>
        }

        testJob "Can parse a required filter with operator" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Filter.Field(X.nonEmptyString).Operator("eq"))
                )

            let! response = Request.get ctx "/xs?filter[nonEmptyString][eq]=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val") @>
        }

        testJob "Can parse a required bool-override filter with operator" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For(
                        (fun x -> calledWith <- Some x),
                        Filter.Field(X.nonEmptyString).Operator("isEmpty").Bool
                    )
                )

            let! response = Request.get ctx "/xs?filter[nonEmptyString][isEmpty]=false" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some false @>
        }

        testJob "Can parse a required list filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Filter.Field(X.nonEmptyString).List)
                )

            let! response = Request.get ctx "/xs?filter[nonEmptyString]=val1,val2" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some [ NonEmptyString "val1"; NonEmptyString "val2" ] @>
        }

        testJob "Can parse a required 1-level resource path filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.a, A.attr)))

            let! response = Request.get ctx "/xs?filter[a.attr]=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some "val" @>
        }

        testJob "Can parse a required 2-level resource path filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.a, A.b, B.attr)))

            let! response = Request.get ctx "/xs?filter[a.b.attr]=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some "val" @>
        }

        testJob "Can parse a required 3-level resource path filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Filter.Field(X.a, A.b, B.c, C.attr))
                )

            let! response = Request.get ctx "/xs?filter[a.b.c.attr]=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some "val" @>
        }

        testJob "Can parse a required nullable filter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Filter.Field(X.nullableNonEmptyString))
                )

            let! response = Request.get ctx "/xs?filter[nullableNonEmptyString]=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val") @>
        }

        testJob "Can parse a DateTimeOffset filter with Z for a field that requires offset" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.dateTimeOffset)))

            let! response = Request.get ctx "/xs?filter[dateTimeOffset]=2000-01-01T15:49:52Z" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(DateTimeOffset(2000, 1, 1, 15, 49, 52, TimeSpan.Zero)) @>
        }

        testJob "Can parse a DateTimeOffset filter with offset for a field that requires offset" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Filter.Field(X.dateTimeOffset)))

            let! response =
                Request.get ctx "/xs?filter[dateTimeOffset]=2000-01-01T15:49:52%2B04:00"
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(DateTimeOffset(2000, 1, 1, 15, 49, 52, TimeSpan.FromHours 4)) @>
        }

        testJob "Returns 400 for a DateTimeOffset filter without offset for a field that requires offset" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(X.dateTimeOffset)))

            let! response = Request.get ctx "/xs?filter[dateTimeOffset]=2000-01-01T15:49:52" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'filter[dateTimeOffset]' got invalid value '2000-01-01T15:49:52': The value must be a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "filter[dateTimeOffset]" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Can parse a DateTimeOffset filter without offset for a field that does not require offset" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(X.dateTimeOffsetAllowMissingOffset)))

            let! response =
                Request.get ctx "/xs?filter[dateTimeOffsetAllowMissingOffset]=2000-01-01T15:49:52"
                |> getResponse

            response |> testSuccessStatusCode
        }

        testJob "Can parse a required nullable attribute" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString))

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = "val" |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome(Some(NonEmptyString "val")) @>
        }

        testJob "Can parse a required nullable attribute set to null" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString))

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = null |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome None @>
        }

        testJob "Returns 400 if required nullable attribute is missing" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, X.nullableNonEmptyString))

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = obj ()
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nullableNonEmptyString' is required for this operation"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Can parse a required nullable attribute as non-nullable" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.AsNonNullable)
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = "val" |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome(NonEmptyString "val") @>
        }

        testJob "Returns 403 if passing null to a required nullable attribute as non-nullable" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, X.nullableNonEmptyString.AsNonNullable))

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = null |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'nullableNonEmptyString' may not be set to null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableNonEmptyString" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if required nullable attribute as non-nullable is missing" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, X.nullableNonEmptyString.AsNonNullable))

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = obj ()
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nullableNonEmptyString' is required for this operation"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Can parse an optional nullable attribute" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.Optional)
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = "val" |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome(Some(Some(NonEmptyString "val"))) @>
        }

        testJob "Can parse an optional nullable attribute set to null" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.Optional)
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = null |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome(Some None) @>
        }

        testJob "Can parse an optional nullable attribute that is not present" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.Optional)
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = obj ()
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome None @>
        }

        testJob "Can parse an optional nullable attribute as non-nullable" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.AsNonNullableOptional)
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = "val" |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome(Some(NonEmptyString "val")) @>
        }

        testJob "Returns 403 when passing null to an optional nullable attribute as non-nullable" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, X.nullableNonEmptyString.AsNonNullableOptional))

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nullableNonEmptyString = null |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'nullableNonEmptyString' may not be set to null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableNonEmptyString" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Can parse an optional nullable attribute non-nullable that is not present" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), X.nullableNonEmptyString.AsNonNullableOptional)
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = obj ()
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome None @>
        }

        testJob "Can parse a related resource using an alternative lookup" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), X.a.Related(X.alternativeALookup))
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        relationships = {|
                            a = {|
                                data = {| ``type`` = "a"; id = "someId" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome "someId" @>
        }

        testJob "Returns 400 if required single filter value contains comma" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(X.nonEmptyString)))

            let! response = Request.get ctx "/xs?filter[nonEmptyString]=val1,val2" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'filter[nonEmptyString]' only accepts a single value, but got 2 comma-separated values"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Can parse a single filter value containing comma if commas are allowed" {
            let mutable calledWith = ValueNone

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- ValueSome x), Filter.Field(X.nonEmptyString).AllowCommas)
                )

            let! response = Request.get ctx "/xs?filter[nonEmptyString]=val1,val2" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = ValueSome(NonEmptyString "val1,val2") @>
        }

        testJob "Can parse a required single parsed sort ascending" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Sort.Parsed(NonEmptyString)))

            let! response = Request.get ctx "/xs?sort=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val", false) @>
        }

        testJob "Can parse a required single parsed sort descending" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Sort.Parsed(NonEmptyString)))

            let! response = Request.get ctx "/xs?sort=-val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val", true) @>
        }

        testJob "Can parse a required single enum sort" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Sort.Enum([ "1", 1 ])))

            let! response = Request.get ctx "/xs?sort=1" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(1, false) @>
        }

        testJob "Can parse a required list sort" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Sort.Enum([ "1", 1; "2", 2 ]).List)
                )

            let! response = Request.get ctx "/xs?sort=1,-2" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some [ (1, false); (2, true) ] @>
        }

        testJob "List sort discards duplicate columns" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Sort.Enum([ "1", 1; "2", 2 ]).List)
                )

            let! response = Request.get ctx "/xs?sort=1,-1,-2,1,-2,2" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some [ (1, false); (2, true) ] @>
        }

        testJob "Returns 400 if enum sort has invalid value" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, Sort.Enum([ "1", 1; "2", 2 ]).List))

            let! response = Request.get ctx "/xs?sort=3,-4" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Comma-separated query parameter 'sort' got invalid value '3' for item 1; expected one of '1', '2'"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "sort" @>
            test <@ json |> getPath "errors[1].status" = "400" @>

            test
                <@
                    json |> getPath "errors[1].detail" = "Comma-separated query parameter 'sort' got invalid value '4' for item 2; expected one of '1', '2'"
                @>

            test <@ json |> getPath "errors[1].source.parameter" = "sort" @>
            test <@ json |> hasNoPath "errors[2]" @>
        }

        testJob "Returns 400 if single sort value contains comma" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, Sort.Enum([ "1", 1; "2", 2 ])))

            let! response = Request.get ctx "/xs?sort=1,2,3" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'sort' only accepts a single value, but got 3 comma-separated values"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "sort" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Can parse page parameters" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For(
                        (fun offset limit number size ->
                            calledWith <-
                                Some {|
                                    Offset = offset
                                    Limit = limit
                                    Number = number
                                    Size = size
                                |}
                        ),
                        Page.Offset,
                        Page.Limit,
                        Page.Number,
                        Page.Size
                    )
                )

            let! response =
                Request.get ctx "/xs?page[offset]=4&page[limit]=5&page[number]=6&page[size]=7"
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith

            test
                <@
                    calledWith' = Some {|
                        Offset = 4
                        Limit = 5
                        Number = 6
                        Size = 7
                    |}
                @>
        }

        testJob "Returns 400 if page has invalid value" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Offset))
            let! response = Request.get ctx "/xs?page[offset]=invalid" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'page[offset]' got invalid value 'invalid': The value must be a valid integer"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "page[offset]" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if page parameter is too small" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Offset.Min(10)))
            let! response = Request.get ctx "/xs?page[offset]=9" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'page[offset]' got invalid value '9': The value must be greater than or equal to 10"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "page[offset]" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if page parameter is too large" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Offset.Max(10)))
            let! response = Request.get ctx "/xs?page[offset]=11" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'page[offset]' got invalid value '11': The value must be less than or equal to 10"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "page[offset]" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns OK if page parameter is at minimum" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Offset.Min(10)))
            let! response = Request.get ctx "/xs?page[offset]=10" |> getResponse

            response |> testSuccessStatusCode
        }

        testJob "Returns OK if page parameter is at maximum" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Offset.Max(10)))
            let! response = Request.get ctx "/xs?page[offset]=10" |> getResponse

            response |> testSuccessStatusCode
        }

        testJob "page[offset] has correct default min/max values" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Offset))

            let! response = Request.get ctx "/xs?page[offset]=0" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[offset]=2147483647" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[offset]=-1" |> getResponse
            response |> testStatusCode 400
        }

        testJob "page[limit] has correct default min/max values" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Limit))

            let! response = Request.get ctx "/xs?page[limit]=1" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[limit]=2147483647" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[limit]=0" |> getResponse
            response |> testStatusCode 400
        }

        testJob "page[number] has correct default min/max values" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Number))

            let! response = Request.get ctx "/xs?page[number]=0" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[number]=2147483647" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[number]=-1" |> getResponse
            response |> testStatusCode 400
        }

        testJob "page[size] has correct default min/max values" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Page.Size))

            let! response = Request.get ctx "/xs?page[size]=1" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[size]=2147483647" |> getResponse
            response |> testSuccessStatusCode

            let! response = Request.get ctx "/xs?page[size]=0" |> getResponse
            response |> testStatusCode 400
        }

        testJob "Can parse custom query parameters" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Query.Parsed("customParam", NonEmptyString))
                )

            let! response = Request.get ctx "/xs?customParam=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val") @>
        }

        testJob "Can parse DateTime" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser -> parser.For((fun x -> calledWith <- Some x), Query.DateTime("customParam")))

            let! response = Request.get ctx "/xs?customParam=2020-01-01T00:00:00" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(DateTime(2020, 1, 1, 0, 0, 0)) @>
        }

        testJob "Can parse DateTimeOffset with Z" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Query.DateTimeOffset("customParam"))
                )

            let! response = Request.get ctx "/xs?customParam=2020-01-01T00:00:00Z" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(DateTimeOffset(2020, 1, 1, 0, 0, 0, TimeSpan.Zero)) @>
        }

        testJob "Can parse DateTimeOffset with offset" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Query.DateTimeOffset("customParam"))
                )

            let! response = Request.get ctx "/xs?customParam=2020-01-01T00:00:00%2B03:00" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(DateTimeOffset(2020, 1, 1, 0, 0, 0, TimeSpan.FromHours 3.)) @>
        }

        testJob "Returns error if DateTimeOffset does not have offset" {
            let ctx =
                Ctx.Create(fun parser -> parser.For(ignore, Query.DateTimeOffset("customParam")))

            let! response = Request.get ctx "/xs?customParam=2020-01-01T00:00:00" |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'customParam' got invalid value '2020-01-01T00:00:00': The value must be a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "customParam" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Can parse a custom query filter parameter" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Filter.Parsed("custom", NonEmptyString))
                )

            let! response = Request.get ctx "/xs?filter[custom]=val" |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val") @>
        }

        testJob "Can parse headers case insensitively" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun x -> calledWith <- Some x), Header.Parsed("HeaderName", NonEmptyString))
                )

            let! response =
                Request.get ctx "/xs"
                |> Request.setHeader (Custom("headerNAME", "val"))
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val") @>
        }

        testJob "Does not fail if optional parameters are missing" {
            let ctx =
                Ctx.Create(fun parser ->
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

            let! response = Request.get ctx "/xs" |> getResponse

            response |> testSuccessStatusCode
        }

        testJob
            "Returns 400 for each required parameter that is missing or invalid and each optional parameter that is invalid" {
            let ctx =
                Ctx.Create(fun parser ->
                    parser
                        .For(
                            (fun _ _ _ _ -> ()),
                            Filter.Field(X.nonEmptyString),
                            X.nonNegativeFloat,
                            X.a,
                            Query.String("customParam1")
                        )
                        .Add((fun _ () -> ()), Filter.Field(X.nonNegativeInt))
                        .Add((fun _ () -> ()), Page.Offset)
                        .Add(
                            (fun _ () -> ()),
                            Header.ParsedRes(
                                "HeaderName",
                                fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                            )
                        )
                )

            let! response =
                Request.get ctx "/xs?filter[nonEmptyString]=&filter[nonNegativeInt]=-1&page[offset]=-1"
                |> Request.setHeader (Custom("HeaderName", "val"))
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@ json |> getPath "errors[0].detail" = "Query parameter 'filter[nonEmptyString]' got invalid value ''" @>

            test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
            test <@ json |> getPath "errors[1].status" = "400" @>

            test
                <@ json |> getPath "errors[1].detail" = "Attribute 'nonNegativeFloat' is required for this operation" @>

            test <@ json |> getPath "errors[1].source.pointer" = "" @>
            test <@ json |> getPath "errors[2].status" = "400" @>
            test <@ json |> getPath "errors[2].detail" = "Relationship 'a' is required for this operation" @>
            test <@ json |> getPath "errors[2].source.pointer" = "" @>
            test <@ json |> getPath "errors[3].status" = "400" @>

            test
                <@ json |> getPath "errors[3].detail" = "Query parameter 'customParam1' is required for this operation" @>

            test <@ json |> getPath "errors[3].source.parameter" = "customParam1" @>
            test <@ json |> getPath "errors[4].status" = "400" @>

            test
                <@
                    json |> getPath "errors[4].detail" = "Query parameter 'filter[nonNegativeInt]' got invalid value '-1'"
                @>

            test <@ json |> getPath "errors[4].source.parameter" = "filter[nonNegativeInt]" @>
            test <@ json |> getPath "errors[5].status" = "400" @>

            test
                <@
                    json |> getPath "errors[5].detail" = "Query parameter 'page[offset]' got invalid value '-1': The value must be greater than or equal to 0"
                @>

            test <@ json |> getPath "errors[5].source.parameter" = "page[offset]" @>
            test <@ json |> getPath "errors[6].status" = "422" @>
            test <@ json |> getPath "errors[6].code" = "custom" @>
            test <@ json |> hasNoPath "errors[6].source" @>
            test <@ json |> hasNoPath "errors[7]" @>
        }

        testJob "Names are case sensitive when not using strict mode" {
            let ctx =
                Ctx.Create(fun parser ->
                    parser.For((fun _ _ -> ()), Filter.Field(X.nonEmptyString), X.nonNegativeFloat)
                )

            let! response =
                Request.postWithoutStrictMode ctx "/xs?filter[NonEmptyString]=foo"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| NonNegativeFloat = 1. |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'filter[nonEmptyString]' is required for this operation"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
            test <@ json |> getPath "errors[1].status" = "400" @>

            test
                <@ json |> getPath "errors[1].detail" = "Attribute 'nonNegativeFloat' is required for this operation" @>

            test <@ json |> getPath "errors[1].source.pointer" = "/data/attributes" @>
            test <@ json |> hasNoPath "errors[2]" @>
        }

        testJob "Returns 400 for each prohibited query parameter, field, and header that is present" {
            let ctx =
                Ctx.Create(fun parser ->
                    parser
                        .For(())
                        .Prohibit(Filter.Field(X.nonEmptyString))
                        .Prohibit(X.nonNegativeInt)
                        .Prohibit(X.a)
                        .Prohibit(Header.String("HeaderName"))
                )

            let! response =
                Request.post ctx "/xs?filter[nonEmptyString]=val"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nonNegativeInt = 2 |}
                        relationships = {|
                            a = {|
                                data = {| ``type`` = "a"; id = "someId" |}
                            |}
                        |}
                    |}
                |}
                |> Request.setHeader (Custom("HeaderName", "val"))
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Query parameter 'filter[nonEmptyString]' is not allowed for this operation"
                @>

            test <@ json |> getPath "errors[0].source.parameter" = "filter[nonEmptyString]" @>
            test <@ json |> getPath "errors[1].status" = "400" @>

            test
                <@ json |> getPath "errors[1].detail" = "Attribute 'nonNegativeInt' is not allowed for this operation" @>

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

            let ctx =
                Ctx.Create(fun parser ->
                    parser
                        .For(XSearchArgs.Create, Filter.Field(X.nonEmptyString))
                        .Add(XSearchArgs.setNonNegativeIntLe, Filter.Field(X.nonNegativeInt).Operator("le"))
                        .Add(XSearchArgs.setNonNegativeIntGe, Filter.Field(X.nonNegativeInt).Operator("ge"))
                        .Add(XSearchArgs.setNullableNonEmptyString, Filter.Field(X.nullableNonEmptyString))
                        .Map(fun args ->
                            calledWith <- Some args
                            ()
                        )
                )

            let! response =
                Request.get
                    ctx
                    "/xs?filter[nonEmptyString]=foo&filter[nonNegativeInt][ge]=2&filter[nullableNonEmptyString]=bar"
                |> getResponse

            let expected = {
                nonEmptyString = NonEmptyString "foo"
                nonNegativeIntLe = None
                nonNegativeIntGe = Some(NonNegativeInt 2)
                nullableNonEmptyString = Some(NonEmptyString "bar")
            }

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some expected @>
        }

        testJob "Successfully parses POST collection example" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser
                        .For(id, X.nonEmptyString)
                        .Map(fun args ->
                            calledWith <- Some args
                            ()
                        )
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "x"
                        attributes = {| nonEmptyString = "val" |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some(NonEmptyString "val") @>
        }

        testJob "Can use optional getters as required option-wrapped params" {
            let mutable calledWith = None

            let ctx =
                Ctx.Create(fun parser ->
                    parser
                        .For(id, X.nonEmptyString.Optional)
                        .Map(fun args ->
                            calledWith <- Some args
                            ()
                        )
                )

            let! response =
                Request.post ctx "/xs"
                |> Request.bodySerialized {| data = {| ``type`` = "x" |} |}
                |> getResponse

            response |> testSuccessStatusCode
            let calledWith' = calledWith
            test <@ calledWith' = Some None @>
        }

        testJob "Can resolve non-context overloads" {
            // Compile-time tests
            let f () =
                let parser: RequestParserHelper<unit> = failwith "Not called"
                let setString (_: string) () = ()
                let setStringBool (_: string * bool) () = ()
                let parseRes: string -> Result<_, Error list> = failwith "Not called"
                let parseResString: string -> Result<_, string> = failwith "Not called"

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

        let returnsNoneTestData = [
            "ID filter", "filter[id]", Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.resIdOpt)))
            "Non-nullable attribute filter",
            "filter[nonNullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableOpt)))
            "Non-nullable attribute filter with custom toSerialized",
            "filter[nonNullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableOpt, (fun _ -> None))))
            "Nullable attribute filter",
            "filter[nullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableOpt)))
            "Nullable attribute filter with custom toSerialized",
            "filter[nullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableOpt, (fun _ -> None))))
            "To-one relationship filter",
            "filter[toOneOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneOpt)))
            "To-one nullable relationship filter",
            "filter[toOneNullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneNullableOpt)))
            "To-many relationship filter",
            "filter[toManyOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toManyOpt)))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.ParsedOpt("custom", (fun _ -> None))))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.ParsedOpt("custom", (fun _ -> None)).List))
            "Custom sort", "sort", Ctx.Create(fun parser -> parser.For(ignore, Sort.ParsedOpt(fun _ -> None)))
            "Custom sort list", "sort", Ctx.Create(fun parser -> parser.For(ignore, Sort.ParsedOpt(fun _ -> None).List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser -> parser.For(ignore, Query.ParsedOpt("customQueryParam", (fun _ -> None))))
        ]

        for suffix, paramName, ctx in returnsNoneTestData do
            testJob $"Returns expected error for query parameters where parser returns None: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue'"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        let returnsErrorTestData = [
            "ID filter", "filter[id]", Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.resIdRes)))
            "Non-nullable attribute filter",
            "filter[nonNullableRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableRes)))
            "Nullable attribute filter",
            "filter[nullableRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableRes)))
            "To-one relationship filter",
            "filter[toOneRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneRes)))
            "To-one nullable relationship filter",
            "filter[toOneNullableRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneNullableRes)))
            "To-many relationship filter",
            "filter[toManyRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toManyRes)))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.ParsedRes("custom", (fun _ -> Error "Custom message"))))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser ->
                parser.For(ignore, Filter.ParsedRes("custom", (fun _ -> Error "Custom message")).List)
            )
            "Custom sort",
            "sort",
            Ctx.Create(fun parser -> parser.For(ignore, Sort.ParsedRes(fun _ -> Error "Custom message")))
            "Custom sort list",
            "sort",
            Ctx.Create(fun parser -> parser.For(ignore, Sort.ParsedRes(fun _ -> Error "Custom message").List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser ->
                parser.For(ignore, Query.ParsedRes("customQueryParam", (fun _ -> Error "Custom message")))
            )
        ]

        for suffix, paramName, ctx in returnsErrorTestData do
            testJob $"Returns expected error for query parameters where parser returns Error: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue': Custom message"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        let enumTestData = [
            "Non-nullable attribute filter",
            "filter[nonNullableEnum]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableEnum)))
            "Nullable attribute filter",
            "filter[nullableEnum]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableEnum)))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Enum("custom", [ "a", 1; "b", 2 ])))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Enum("custom", [ "a", 1; "b", 2 ]).List))
            "Custom sort", "sort", Ctx.Create(fun parser -> parser.For(ignore, Sort.Enum([ "a", 1; "b", 2 ])))
            "Custom sort list", "sort", Ctx.Create(fun parser -> parser.For(ignore, Sort.Enum([ "a", 1; "b", 2 ]).List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser -> parser.For(ignore, Query.Enum("customQueryParam", [ "a", 1; "b", 2 ])))
        ]

        for suffix, paramName, ctx in enumTestData do
            testJob $"Returns expected error for query parameters where parser is enum: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue'; expected one of 'a', 'b'"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        let boolTestData = [
            "Non-nullable attribute filter",
            "filter[nonNullableBool]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableBool)))
            "Nullable attribute filter",
            "filter[nullableBool]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableBool)))
            "Custom filter", "filter[custom]", Ctx.Create(fun parser -> parser.For(ignore, Filter.Bool("custom")))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Bool("custom").List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser -> parser.For(ignore, Query.Bool("customQueryParam")))
        ]

        for suffix, paramName, ctx in boolTestData do
            testJob $"Returns expected error for bool query parameters: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue'; expected one of 'true', 'false'"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        let intTestData = [
            "Non-nullable attribute filter",
            "filter[nonNullableInt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableInt)))
            "Nullable attribute filter",
            "filter[nullableInt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableInt)))
            "Custom filter", "filter[custom]", Ctx.Create(fun parser -> parser.For(ignore, Filter.Int("custom")))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Int("custom").List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser -> parser.For(ignore, Query.Int("customQueryParam")))
        ]

        for suffix, paramName, ctx in intTestData do
            testJob $"Returns expected error for int query parameters: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue': The value must be a valid integer"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        let floatTestData = [
            "Non-nullable attribute filter",
            "filter[nonNullableFloat]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableFloat)))
            "Nullable attribute filter",
            "filter[nullableFloat]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableFloat)))
            "Custom filter", "filter[custom]", Ctx.Create(fun parser -> parser.For(ignore, Filter.Float("custom")))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Float("custom").List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser -> parser.For(ignore, Query.Float("customQueryParam")))
        ]

        for suffix, paramName, ctx in floatTestData do
            testJob $"Returns expected error for float query parameters: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue': The value must be a valid number"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        let dateTimeOffsetAllowMissingOffsetTestData = [
            "Non-nullable attribute filter",
            "filter[nonNullableDateTimeOffsetAllowMissingOffset]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableDateTimeOffsetAllowMissingOffset)))
            "Nullable attribute filter",
            "filter[nullableDateTimeOffsetAllowMissingOffset]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableDateTimeOffsetAllowMissingOffset)))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.DateTimeOffsetAllowMissingOffset("custom")))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.DateTimeOffsetAllowMissingOffset("custom").List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser -> parser.For(ignore, Query.DateTimeOffsetAllowMissingOffset("customQueryParam")))
        ]

        for suffix, paramName, ctx in dateTimeOffsetAllowMissingOffsetTestData do
            testJob $"Returns expected error for DateTimeOffsetAllowMissingOffset query parameters: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue': The value must be a valid ISO 8601-1:2019 date-time"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        let dateTimeOffsetTestData = [
            "Non-nullable attribute filter",
            "filter[nonNullableDateTimeOffset]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableDateTimeOffset)))
            "Nullable attribute filter",
            "filter[nullableDateTimeOffset]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableDateTimeOffset)))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.DateTimeOffset("custom")))
            "Custom list filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.DateTimeOffset("custom").List))
            "Custom query param",
            "customQueryParam",
            Ctx.Create(fun parser -> parser.For(ignore, Query.DateTimeOffset("customQueryParam")))
        ]

        for suffix, paramName, ctx in dateTimeOffsetTestData do
            testJob $"Returns expected error for DateTimeOffset query parameters: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=invalidValue" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Query parameter '{paramName}' got invalid value 'invalidValue': The value must be a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[1]" @>
            }

        testJob "Returns expected error for headers where parser returns None" {
            let ctx =
                Ctx.Create(fun parser ->
                    parser.For(ignore, Header.ParsedOpt("CustomHeader", (fun _ -> (None: string option))))
                )

            let! response =
                Request.get ctx "/ys"
                |> Request.setHeader (RequestHeader.Custom("CustomHeader", "invalidValue"))
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Header 'CustomHeader' got invalid value 'invalidValue'" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for headers where parser returns Error" {
            let ctx =
                Ctx.Create(fun parser ->
                    parser.For(
                        ignore,
                        Header.ParsedRes("CustomHeader", (fun _ -> (Error "Custom message": Result<string, string>)))
                    )
                )

            let! response =
                Request.get ctx "/ys"
                |> Request.setHeader (RequestHeader.Custom("CustomHeader", "invalidValue"))
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Header 'CustomHeader' got invalid value 'invalidValue': Custom message"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        let commaSeparatedReturnsNoneTestData = [
            "ID filter", "filter[id]", Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.resIdOpt).List))
            "Non-nullable attribute filter",
            "filter[nonNullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableOpt).List))
            "Non-nullable attribute filter with custom toSerialized",
            "filter[nonNullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableOpt, (fun _ -> None)).List))
            "Nullable attribute filter",
            "filter[nullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableOpt).List))
            "Nullable attribute filter with custom toSerialized",
            "filter[nullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableOpt, (fun _ -> None)).List))
            "To-one relationship filter",
            "filter[toOneOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneOpt).List))
            "To-one nullable relationship filter",
            "filter[toOneNullableOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneNullableOpt).List))
            "To-many relationship filter",
            "filter[toManyOpt]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toManyOpt).List))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.ParsedOpt("custom", (fun _ -> None)).List))
            "Custom sort", "sort", Ctx.Create(fun parser -> parser.For(ignore, Sort.ParsedOpt(fun _ -> None).List))
        ]

        for suffix, paramName, ctx in commaSeparatedReturnsNoneTestData do
            testJob $"Returns expected error for comma-separated query parameters where parser returns None: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=foo,bar" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Comma-separated query parameter '{paramName}' got invalid value 'foo' for item 1"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> getPath "errors[1].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[1].detail" = $"Comma-separated query parameter '{paramName}' got invalid value 'bar' for item 2"
                    @>

                test <@ json |> getPath "errors[1].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[2]" @>
            }

        let commaSeparatedReturnsErrorTestData = [
            "ID filter", "filter[id]", Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.resIdRes).List))
            "Non-nullable attribute filter",
            "filter[nonNullableRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableRes).List))
            "Nullable attribute filter",
            "filter[nullableRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableRes).List))
            "To-one relationship filter",
            "filter[toOneRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneRes).List))
            "To-one nullable relationship filter",
            "filter[toOneNullableRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toOneNullableRes).List))
            "To-many relationship filter",
            "filter[toManyRes]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.toManyRes).List))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser ->
                parser.For(ignore, Filter.ParsedRes("custom", (fun _ -> Error "Custom message")).List)
            )
            "Custom sort",
            "sort",
            Ctx.Create(fun parser -> parser.For(ignore, Sort.ParsedRes(fun _ -> Error "Custom message").List))
        ]

        for suffix, paramName, ctx in commaSeparatedReturnsErrorTestData do
            testJob $"Returns expected error for comma-separated query parameters where parser returns Error: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=foo,bar" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Comma-separated query parameter '{paramName}' got invalid value 'foo' for item 1: Custom message"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> getPath "errors[1].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[1].detail" = $"Comma-separated query parameter '{paramName}' got invalid value 'bar' for item 2: Custom message"
                    @>

                test <@ json |> getPath "errors[1].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[2]" @>
            }

        let commaSeparatedEnumTestData = [
            "Non-nullable attribute filter",
            "filter[nonNullableEnum]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nonNullableEnum).List))
            "Nullable attribute filter",
            "filter[nullableEnum]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Field(Y.nullableEnum).List))
            "Custom filter",
            "filter[custom]",
            Ctx.Create(fun parser -> parser.For(ignore, Filter.Enum("custom", [ "a", 1; "b", 2 ]).List))
            "Custom sort", "sort", Ctx.Create(fun parser -> parser.For(ignore, Sort.Enum([ "a", 1; "b", 2 ]).List))
        ]

        for suffix, paramName, ctx in commaSeparatedEnumTestData do
            testJob $"Returns expected error for comma-separated query parameters where parser is enum: {suffix}" {
                let! response = Request.get ctx $"/ys?{paramName}=foo,bar" |> getResponse

                response |> testStatusCode 400
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[0].detail" = $"Comma-separated query parameter '{paramName}' got invalid value 'foo' for item 1; expected one of 'a', 'b'"
                    @>

                test <@ json |> getPath "errors[0].source.parameter" = paramName @>
                test <@ json |> getPath "errors[1].status" = "400" @>

                test
                    <@
                        json |> getPath "errors[1].detail" = $"Comma-separated query parameter '{paramName}' got invalid value 'bar' for item 2; expected one of 'a', 'b'"
                    @>

                test <@ json |> getPath "errors[1].source.parameter" = paramName @>
                test <@ json |> hasNoPath "errors[2]" @>
            }

        testJob "Returns expected error for invalid ID in body where parser returns None" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.resIdOpt))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        id = "invalidValue"
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Got invalid resource ID value 'invalidValue'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid ID in body where parser returns Error" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.resIdRes))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        id = "invalidValue"
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@ json |> getPath "errors[0].detail" = "Got invalid resource ID value 'invalidValue': Custom message" @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid non-nullable attribute in body where parser returns None" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nonNullableOpt))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {| nonNullableOpt = "invalidValue" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@ json |> getPath "errors[0].detail" = "Attribute 'nonNullableOpt' got invalid value 'invalidValue'" @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nonNullableOpt" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid nullable attribute in body where parser returns None" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nullableOpt))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {| nullableOpt = "invalidValue" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'nullableOpt' got invalid value 'invalidValue'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableOpt" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid non-nullable attribute in body where parser returns Error" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nonNullableRes))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {| nonNullableRes = "invalidValue" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nonNullableRes' got invalid value 'invalidValue': Custom message"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nonNullableRes" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid nullable attribute in body where parser returns Error" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nullableRes))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {| nullableRes = "invalidValue" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nullableRes' got invalid value 'invalidValue': Custom message"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableRes" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid non-nullable attribute in body where parser is enum" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nonNullableEnum))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {| nonNullableEnum = "invalidValue" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nonNullableEnum' got invalid value 'invalidValue'; expected one of 'a', 'b'"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nonNullableEnum" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid nullable attribute in body where parser is enum" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nullableEnum))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {| nullableEnum = "invalidValue" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nullableEnum' got invalid value 'invalidValue'; expected one of 'a', 'b'"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableEnum" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid non-nullable SimpleDateTimeOffset attribute in body" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nonNullableDateTimeOffset))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {|
                            nonNullableDateTimeOffset = "invalidValue"
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nonNullableDateTimeOffset' got invalid value 'invalidValue': The value must be a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nonNullableDateTimeOffset" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns expected error for invalid nullable SimpleDateTimeOffset attribute in body" {
            let ctx = Ctx.Create(fun parser -> parser.For(ignore, Y.nullableDateTimeOffset))

            let! response =
                Request.post ctx "/ys"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "y"
                        attributes = {|
                            nullableDateTimeOffset = "invalidValue"
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Attribute 'nullableDateTimeOffset' got invalid value 'invalidValue': The value must be a valid ISO 8601-1:2019 date-time including an offset (e.g. 'Z' or '+01:00')"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableDateTimeOffset" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

    ]
