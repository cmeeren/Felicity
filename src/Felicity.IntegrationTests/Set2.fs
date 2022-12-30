module Set2

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity

type MappedCtx = {
    SetNonNull: string * int -> string -> string
    SetNull12: string option * int option -> string -> string
    SetNull34: (int * string) option -> string -> string
}

type Ctx =
    {
        SetNonNull: string * int -> string -> string
        SetNull12: string option * int option -> string -> string
        SetNull34: (int * string) option -> string -> string
        MapCtx: Ctx -> Result<MappedCtx, Error list>
        MapCtxWithEntity: Ctx -> string -> Result<MappedCtx, Error list>
    }

    static member Default = {
        SetNonNull = fun _ -> failwith "Must be set if used"
        SetNull12 = fun _ -> failwith "Must be set if used"
        SetNull34 = fun _ -> failwith "Must be set if used"
        MapCtx =
            fun ctx ->
                Ok
                    {
                        SetNonNull = ctx.SetNonNull
                        SetNull12 = ctx.SetNull12
                        SetNull34 = ctx.SetNull34
                    }
        MapCtxWithEntity =
            fun ctx _ ->
                Ok
                    {
                        SetNonNull = ctx.SetNonNull
                        SetNull12 = ctx.SetNull12
                        SetNull34 = ctx.SetNull34
                    }
    }

module A =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("a", resId).CollectionName("as")

    let nonNull1 = define.Attribute.SimpleString()

    let nonNull2 = define.Attribute.SimpleInt()

    let null1 = define.Attribute.Nullable.SimpleString()

    let null2 = define.Attribute.Nullable.SimpleInt()

    let null3 = define.Attribute.Nullable.SimpleInt()

    let nullRel = define.Relationship.ToOneNullable(resDef)

    let setNonNull =
        define
            .Operation
            .ForContextRes(fun ctx -> ctx.MapCtx ctx)
            .Set2((fun ctx x e -> ctx.SetNonNull x e), nonNull1, nonNull2)

    let setNull12 =
        define
            .Operation
            .ForContextRes(fun ctx e -> ctx.MapCtxWithEntity ctx e)
            .Set2((fun ctx x e -> ctx.SetNull12 x e), null1, null2)

    let setNull34 =
        define
            .Operation
            .ForContextRes(fun ctx -> ctx.MapCtx ctx)
            .Set2SameNull((fun ctx x e -> ctx.SetNull34 x e), null3, nullRel)

    let post = define.Operation.Post(fun () -> "foobar").AfterCreate(fun _ -> ())

    let lookup = define.Operation.Lookup(fun _ id -> Some id)

    let get = define.Operation.GetResource()

    let patch = define.Operation.Patch().AfterUpdate(fun (_: Ctx) _ -> ())


[<Tests>]
let tests =
    testList "Set2" [

        testJob "Runs Set2 with non-nullable fields" {
            let mutable calledWith = ValueNone

            let ctx =
                { Ctx.Default with
                    SetNonNull =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
                }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| nonNull1 = "abc"; nonNull2 = 123 |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome("abc", 123) @>
        }

        testJob "Runs Set2 with nullable fields" {
            let mutable calledWith = ValueNone

            let ctx =
                { Ctx.Default with
                    SetNull12 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
                }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null1 = "abc"; null2 = null |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome(Some "abc", None) @>
        }

        testJob "Runs Set2SameNull when both non-null" {
            let mutable calledWith = ValueNone

            let ctx =
                { Ctx.Default with
                    SetNull34 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
                }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null3 = 123 |}
                                relationships =
                                    {|
                                        nullRel =
                                            {|
                                                data = {| ``type`` = "a"; id = "abc" |}
                                            |}
                                    |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome(Some(123, "abc")) @>
        }

        testJob "Runs Set2SameNull when both null" {
            let mutable calledWith = ValueNone

            let ctx =
                { Ctx.Default with
                    SetNull34 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
                }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null3 = null |}
                                relationships = {| nullRel = {| data = null |} |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let calledWith = calledWith
            test <@ calledWith = ValueSome None @>
        }

        testJob "Set2 returns error if first is missing" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| nonNull1 = "abc" |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Field 'nonNull1' can only be set together with field 'nonNull2', which was missing"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Set2 returns error if second is missing" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| nonNull2 = 123 |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Field 'nonNull2' can only be set together with field 'nonNull1', which was missing"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Set2SameNull returns error if first is missing" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null3 = null |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Field 'null3' can only be set together with field 'nullRel', which was missing"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Set2SameNull returns error if second is missing" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                relationships = {| nullRel = {| data = null |} |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Field 'nullRel' can only be set together with field 'null3', which was missing"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Set2SameNull returns error if first is null and second is non-null" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null3 = null |}
                                relationships =
                                    {|
                                        nullRel =
                                            {|
                                                data = {| ``type`` = "a"; id = "abc" |}
                                            |}
                                    |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The fields 'null3' and 'nullRel' must both be either null or non-null"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Set2SameNull returns error if first is non-null and second is null" {
            let ctx = Ctx.Default

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null3 = 123 |}
                                relationships = {| nullRel = {| data = null |} |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The fields 'null3' and 'nullRel' must both be either null or non-null"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "POST runs Set2 with non-nullable fields" {
            let mutable calledWith = ValueNone

            let ctx =
                { Ctx.Default with
                    SetNonNull =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
                }

            let! response =
                Request.post ctx "/as"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                attributes = {| nonNull1 = "abc"; nonNull2 = 123 |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 201
            let calledWith = calledWith
            test <@ calledWith = ValueSome("abc", 123) @>
        }

        testJob "POST runs Set2SameNull when both non-null" {
            let mutable calledWith = ValueNone

            let ctx =
                { Ctx.Default with
                    SetNull34 =
                        fun x e ->
                            calledWith <- ValueSome x
                            e
                }

            let! response =
                Request.post ctx "/as"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                attributes = {| null3 = 123 |}
                                relationships =
                                    {|
                                        nullRel =
                                            {|
                                                data = {| ``type`` = "a"; id = "abc" |}
                                            |}
                                    |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 201
            let calledWith = calledWith
            test <@ calledWith = ValueSome(Some(123, "abc")) @>
        }

        testJob "Returns errors returned by mapCtx in Set2" {
            let ctx =
                { Ctx.Default with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| nonNull1 = "abc"; nonNull2 = 123 |}
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

        testJob "Returns errors returned by mapCtx in Set2SameNull" {
            let ctx =
                { Ctx.Default with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null3 = 123 |}
                                relationships =
                                    {|
                                        nullRel =
                                            {|
                                                data = {| ``type`` = "a"; id = "abc" |}
                                            |}
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
            let ctx =
                { Ctx.Default with
                    MapCtxWithEntity = fun _ _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/as/ignoredId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "ignoredId"
                                attributes = {| null1 = "abc"; null2 = null |}
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

            let ctx =
                { Ctx.Default with
                    MapCtxWithEntity =
                        fun ctx e ->
                            calledWith <- ValueSome e
                            Ctx.Default.MapCtxWithEntity ctx e
                }

            let! _response =
                Request.patch ctx "/as/someResourceId"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "someResourceId"
                                attributes = {| null1 = "abc"; null2 = null |}
                            |}
                    |}
                |> getResponse

            Expect.equal calledWith (ValueSome expected) ""
        }

    ]
