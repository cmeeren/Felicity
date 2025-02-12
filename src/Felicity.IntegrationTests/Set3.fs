module Set3

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity

type MappedCtx = {
    SetNonNull: string * int * byte -> string -> string
    SetNull123: string option * int option * byte option -> string -> string
    SetNull456: (string * int * byte) option -> string -> string
}

type Ctx = {
    SetNonNull: string * int * byte -> string -> string
    SetNull123: string option * int option * byte option -> string -> string
    SetNull456: (string * int * byte) option -> string -> string
    MapCtx: Ctx -> Result<MappedCtx, Error list>
    MapCtxWithEntity: Ctx -> string -> Result<MappedCtx, Error list>
} with

    static member Default = {
        SetNonNull = fun _ -> failwith "Must be set if used"
        SetNull123 = fun _ -> failwith "Must be set if used"
        SetNull456 = fun _ -> failwith "Must be set if used"
        MapCtx =
            fun ctx ->
                Ok {
                    SetNonNull = ctx.SetNonNull
                    SetNull123 = ctx.SetNull123
                    SetNull456 = ctx.SetNull456
                }
        MapCtxWithEntity =
            fun ctx _ ->
                Ok {
                    SetNonNull = ctx.SetNonNull
                    SetNull123 = ctx.SetNull123
                    SetNull456 = ctx.SetNull456
                }
    }

module A =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("a", resId).CollectionName("as")

    let nonNull1 = define.Attribute.SimpleString()

    let nonNull2 = define.Attribute.SimpleInt()

    let nonNull3 = define.Attribute.SimpleByte()

    let null1 = define.Attribute.Nullable.SimpleString()

    let null2 = define.Attribute.Nullable.SimpleInt()

    let null3 = define.Attribute.Nullable.SimpleByte()

    let null4 = define.Attribute.Nullable.SimpleString()

    let null5 = define.Attribute.Nullable.SimpleInt()

    let null6 = define.Attribute.Nullable.SimpleByte()

    let setNonNull =
        define.Operation
            .ForContextRes(fun ctx -> ctx.MapCtx ctx)
            .Set3((fun ctx x e -> ctx.SetNonNull x e), nonNull1, nonNull2, nonNull3)

    let setNull =
        define.Operation
            .ForContextRes(fun ctx e -> ctx.MapCtxWithEntity ctx e)
            .Set3((fun ctx x e -> ctx.SetNull123 x e), null1, null2, null3)

    let setSameNull =
        define.Operation
            .ForContextRes(fun ctx -> ctx.MapCtx ctx)
            .Set3SameNull((fun ctx x e -> ctx.SetNull456 x e), null4, null5, null6)

    let post = define.Operation.Post(fun () -> "foobar").AfterCreate(fun _ -> ())

    let lookup = define.Operation.Lookup(fun _ id -> Some id)

    let get = define.Operation.GetResource()

    let patch = define.Operation.Patch().AfterUpdate(fun (_: Ctx) _ -> ())


[<Tests>]
let tests =
    testList "Set3" [

        testJob "Runs Set3 with non-nullable fields" {
            let mutable calledWith = ValueNone

            let ctx = {
                Ctx.Default with
                    SetNonNull =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
            }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            nonNull1 = "abc"
                            nonNull2 = 123
                            nonNull3 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome("abc", 123, 42uy) @>
        }

        testJob "Runs Set3 with nullable fields" {
            let mutable calledWith = ValueNone

            let ctx = {
                Ctx.Default with
                    SetNull123 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
            }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            null1 = "abc"
                            null2 = null
                            null3 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome(Some "abc", None, Some 42uy) @>
        }

        testJob "Runs Set3SameNull when all non-null" {
            let mutable calledWith = ValueNone

            let ctx = {
                Ctx.Default with
                    SetNull456 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
            }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            null4 = "abc"
                            null5 = 123
                            null6 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome(Some("abc", 123, 42uy)) @>
        }

        testJob "Runs Set3SameNull when all null" {
            let mutable calledWith = ValueNone

            let ctx = {
                Ctx.Default with
                    SetNull456 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
            }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            null4 = null
                            null5 = null
                            null6 = null
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome None @>
        }

        testJob "Set3 returns error if one is missing" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {| nonNull2 = 123; nonNull3 = 42uy |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Fields 'nonNull1', 'nonNull2', and 'nonNull3' must be set together, but one or more fields were missing"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Set3SameNull returns error if one is missing" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {| null4 = null; null6 = null |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Fields 'null4', 'null5', and 'null6' must be set together, but one or more fields were missing"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Set3SameNull returns error if one is different null" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            null4 = "abc"
                            null5 = null
                            null6 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Fields 'null4', 'null5', and 'null6' must all be either null or non-null"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST runs Set3 with non-nullable fields" {
            let mutable calledWith = ValueNone

            let ctx = {
                Ctx.Default with
                    SetNonNull =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
            }

            let! response =
                Request.post ctx "/as"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {|
                            nonNull1 = "abc"
                            nonNull2 = 123
                            nonNull3 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 201
            let calledWith = calledWith
            test <@ calledWith = ValueSome("abc", 123, 42uy) @>
        }

        testJob "POST runs Set3SameNull when all non-null" {
            let mutable calledWith = ValueNone

            let ctx = {
                Ctx.Default with
                    SetNull456 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
            }

            let! response =
                Request.post ctx "/as"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {|
                            null4 = "abc"
                            null5 = 123
                            null6 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 201
            let calledWith = calledWith
            test <@ calledWith = ValueSome(Some("abc", 123, 42uy)) @>
        }

        testJob "Returns errors returned by mapCtx in Set3" {
            let ctx = {
                Ctx.Default with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            nonNull1 = "abc"
                            nonNull2 = 123
                            nonNull3 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns errors returned by mapCtx in Set3SameNull" {
            let ctx = {
                Ctx.Default with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            null4 = "abc"
                            null5 = 123
                            null6 = 42uy
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns errors returned by mapCtx with entity" {
            let ctx = {
                Ctx.Default with
                    MapCtxWithEntity = fun _ _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
            }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "ignoredId"
                        attributes = {|
                            null1 = "abc"
                            null2 = null
                            null3 = null
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "mapCtx with entity gets passed the entity" {
            let mutable calledWith = ValueNone
            let expected = "someResourceId"

            let ctx = {
                Ctx.Default with
                    MapCtxWithEntity =
                        fun ctx e ->
                            calledWith <- ValueSome e
                            Ctx.Default.MapCtxWithEntity ctx e
            }

            let! _response =
                Request.patch ctx "/as/someResourceId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "someResourceId"
                        attributes = {|
                            null1 = "abc"
                            null2 = null
                            null3 = null
                        |}
                    |}
                |}
                |> getResponse

            Expect.equal calledWith (ValueSome expected) ""
        }

    ]
