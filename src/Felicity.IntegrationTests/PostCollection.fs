module ``POST collection``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type Child = { Id: string }


type A = {
    Id: string
    A: bool
    X: string
    Nullable: string option
    NullableNotNullWhenSet: string option
    NullableChild: Child option
    NullableChildNotNullWhenCreated: Child option
}

type B = { Id: string; B: int; Y: string }


module ADomain =

    let create a child1 child2 =
        if a = false then
            Error [ Error.create 422 |> Error.setCode "custom" ]
        else
            Ok {
                Id = "1"
                A = a
                X = ""
                Nullable = None
                NullableNotNullWhenSet = None
                NullableChild = child1
                NullableChildNotNullWhenCreated = Some child2
            }

    let setA _x _a : A =
        failwith "Should not set properties that are consumed using the request parser"

    let setX x a = { a with X = x }

    let setNullable x a = { a with Nullable = x }

    let setNullableNotNullWhenSet x a = {
        a with
            NullableNotNullWhenSet = Some x
    }

module BDomain =

    let create id b = { Id = id; B = b; Y = "" }

    let setY y b = { b with Y = y }



type Db() =
    let mutable As: A list = []
    let mutable Bs: B list = []
    let mutable Children: Child list = [ { Child.Id = "c" } ]

    member _.SaveA a = As <- a :: As

    member _.SaveB b = Bs <- b :: Bs

    member _.SaveChild c = Children <- c :: Children

    member _.GetAOrFail id = As |> List.find (fun a -> a.Id = id)

    member _.GetBOrFail id = Bs |> List.find (fun b -> b.Id = id)

    member _.TryGetChild id =
        Children |> List.tryFind (fun c -> c.Id = id)



type Ctx = {
    ModifyAResponse: A -> HttpHandler
    ModifyBResponse: B -> HttpHandler
    Db: Db
} with

    static member WithDb db = {
        ModifyAResponse = fun _ -> fun next ctx -> next ctx
        ModifyBResponse = fun _ -> fun next ctx -> next ctx
        Db = db
    }


module Child =

    let define = Define<Ctx, Child, string>()
    let resId = define.Id.Simple(fun (c: Child) -> c.Id)
    let resDef = define.Resource("child", resId)
    let lookup = define.Operation.Lookup(fun ctx cid -> ctx.Db.TryGetChild cid)


module A =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("abs")
    let lookup = define.Operation.Lookup(fun _ _ -> failwith "not used")
    let get = define.Operation.GetResource()

    let a = define.Attribute.SimpleBool().Get(fun a -> a.A).Set(ADomain.setA)

    let x = define.Attribute.SimpleString().Get(fun a -> a.X).Set(ADomain.setX)

    let nullable =
        define.Attribute.Nullable.SimpleString().Get(fun a -> a.Nullable).Set(ADomain.setNullable)

    let nullableNotNullWhenSet =
        define.Attribute.Nullable
            .SimpleString()
            .Get(fun a -> a.NullableNotNullWhenSet)
            .SetNonNull(ADomain.setNullableNotNullWhenSet)

    let nullableChild = define.Relationship.ToOneNullable(Child.resDef)

    let nullableChildNotNullWhenCreated =
        define.Relationship.ToOneNullable(Child.resDef)

    let readonly = define.Attribute.SimpleString().Get(fun _ -> "test")

    let post =
        define.Operation
            .Post(fun ctx parser ->
                parser.ForRes(
                    ADomain.create,
                    a,
                    nullableChild.Related(Child.lookup),
                    nullableChildNotNullWhenCreated.Related(Child.lookup).AsNonNullable
                )
            )
            .AfterCreate(fun (ctx: Ctx) a -> ctx.Db.SaveA a)
            .ModifyResponse(fun (ctx: Ctx) -> ctx.ModifyAResponse)



module B =

    let define = Define<Ctx, B, string>()
    let resId = define.Id.Simple(fun (b: B) -> b.Id)
    let resDef = define.Resource("b", resId).CollectionName("abs")
    let get = define.Operation.GetResource()

    let b = define.Attribute.SimpleInt().Get(fun b -> b.B)

    let y = define.Attribute.SimpleString().Get(fun b -> b.Y).Set(BDomain.setY)

    let post =
        define.Operation
            .Post(fun ctx parser -> parser.For(BDomain.create, resId, b))
            .AfterCreate(fun (ctx: Ctx) b -> ctx.Db.SaveB b)
            .Return202Accepted()
            .ModifyResponse(fun (ctx: Ctx) -> ctx.ModifyBResponse)


module NotSupported =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun _ -> failwith "not used")
    let resDef = define.Resource("c", resId).CollectionName("abs")


type Ctx2 = Ctx2

type Ctx3 = Ctx3


module C =

    let define = Define<Ctx2, A, string>()
    let resId = define.Id.Simple(fun _ -> failwith "not used")
    let resDef = define.Resource("c", resId).CollectionName("cs")

    let post =
        define.Operation
            .ForContextRes(fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ])
            .Post(fun (_: Ctx3) -> failwith<A> "not used")
            .AfterCreate(ignore)


module A2 =

    let define = Define<Ctx3, A, string>()
    let resId = define.Id.Simple(fun _ -> failwith "not used")
    let resDef = define.Resource("a", resId).CollectionName("abs")


[<Tests>]
let tests =
    testList "POST collection" [

        testJob "Create A: Returns 201, runs setters and returns correct data if successful" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {|
                            a = true
                            x = "abc"
                            nullable = "foo"
                            nullableNotNullWhenSet = "bar"
                        |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 201
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "a" @>
            test <@ json |> getPath "data.id" = "1" @>
            test <@ json |> getPath "data.attributes.a" = true @>
            test <@ json |> getPath "data.attributes.x" = "abc" @>
            test <@ json |> getPath "data.attributes.nullable" = "foo" @>
            test <@ json |> getPath "data.attributes.nullableNotNullWhenSet" = "bar" @>

            test <@ response.headers[NonStandard "Foo"] = "Bar" @>
            test <@ response.headers[Location] = "http://example.com/abs/1" @>

            let a = db.GetAOrFail "1"
            test <@ a.Id = "1" @>
            test <@ a.A = true @>
            test <@ a.X = "abc" @>
            test <@ a.Nullable = Some "foo" @>
            test <@ a.NullableNotNullWhenSet = Some "bar" @>
            test <@ a.NullableChildNotNullWhenCreated = Some { Id = "c" } @>
        }

        testJob "Insensitive to trailing slashes" {
            let ctx = Ctx.WithDb(Db())

            let! response =
                Request.post ctx "/abs/"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {| a = true |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 201
        }

        testJob "Create B: Returns 202, runs setters and returns correct data if successful" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyBResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "b"
                        id = "123"
                        attributes = {| b = 2; y = "abc" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 202
            let! json = response |> Response.readBodyAsString
            test <@ json = "" @>

            test <@ response.headers[NonStandard "Foo"] = "Bar" @>

            let b = db.GetBOrFail "123"
            test <@ b.Id = "123" @>
            test <@ b.B = 2 @>
            test <@ b.Y = "abc" @>
        }

        testJob "Returns 400 when non-nullable attribute is null" {
            let db = Db()
            db.SaveChild { Child.Id = "c" }

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {| a = null |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'a' on type 'a' is not nullable" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/a" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 when nullableChildNotNullWhenCreated is null" {
            let db = Db()
            db.SaveChild { Child.Id = "c" }

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {|
                            a = true
                            x = "abc"
                            nullable = "foo"
                            nullableNotNullWhenSet = "bar"
                        |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {| data = null |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Relationship 'nullableChildNotNullWhenCreated' may not be set to null"
                @>

            test
                <@
                    json |> getPath "errors[0].source.pointer" = "/data/relationships/nullableChildNotNullWhenCreated/data"
                @>

            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns errors returned by create" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {| a = false; x = "abc" |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
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

        testJob "Returns 403 when read-only" {
            let db = Db()

            let! response =
                Request.post (Ctx.WithDb db) "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {| a = true; readonly = "foo" |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'readonly' is read-only" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/readonly" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if nullable attribute is set to null when not supported" {
            let db = Db()

            let! response =
                Request.post (Ctx.WithDb db) "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {|
                            a = true
                            nullableNotNullWhenSet = null
                        |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'nullableNotNullWhenSet' may not be set to null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableNotNullWhenSet" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when using stringified numbers" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyBResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "b"
                        id = "123"
                        attributes = {| b = "2" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Invalid JSON or incorrect data type in field 'b'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/b" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 when client-generated ID is not supported" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "foo"
                        attributes = {| a = true |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'abs' does not support creating resource type 'a' with a client-generated ID"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing body" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response = Request.post ctx "/abs" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Request to create resources must contain a single resource object as primary data"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when JSON is invalid" {
            let db = Db()

            let! response =
                Request.post (Ctx.WithDb db) "/abs"
                |> Request.bodyString
                    """
{
  "test": {
    "foo": { "bar" "baz" }
  }
}
"""
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Invalid JSON or incorrect data type at path $.test, line 3, position 19"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing data" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {| meta = obj () |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'data'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when data is null" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {| data = null |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Request to create resources must contain a single resource object as primary data"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing type" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {| attributes = {| a = true |} |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'type'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when type is null" {
            let db = Db()

            let! response =
                Request.post (Ctx.WithDb db) "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = null
                        attributes = {| a = true |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'type' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when ID is null" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {| ``type`` = "a"; id = null |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'id' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when attributes is null" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {| ``type`` = "a"; attributes = null |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'attributes' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationships is null" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.post ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        relationships = null
                    |}
                |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'relationships' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Ignores unknown members and relationships when not using strict mode" {
            let db = Db()

            let ctx = {
                Ctx.WithDb db with
                    ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar"
            }

            let! response =
                Request.postWithoutStrictMode ctx "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {|
                            a = true
                            x = "abc"
                            nullable = "foo"
                            nonExistentAttribute = "foo"
                        |}
                        relationships = {|
                            nullableChild = {| data = null |}
                            nullableChildNotNullWhenCreated = {|
                                data = {| ``type`` = "child"; id = "c" |}
                            |}
                            nonExistentRelationship = {| data = null |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 201
        }

        testJob "Returns errors returned by mapCtx" {
            let! response =
                Request.post Ctx2 "/cs"
                |> Request.bodySerialized {| data = {| ``type`` = "c" |} |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 409 if not supported by resource" {
            let db = Db()

            let! response =
                Request.post (Ctx.WithDb db) "/abs"
                |> Request.bodySerialized {| data = {| ``type`` = "c" |} |}
                |> getResponse

            response |> testStatusCode 409
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "409" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'abs' does not support creating resources with type 'c'; expected one of 'a', 'b'"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if not supported at all" {
            let! response =
                Request.post Ctx3 "/abs"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {| a = true; x = "abc" |}
                    |}
                |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not support creating resources" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns error if collection case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.post ctx "/Abs" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Abs' does not exist, but differs only by case from the existing path '/abs'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

    ]
