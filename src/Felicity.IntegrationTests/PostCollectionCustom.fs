module ``POST collection custom``

open System
open Expecto
open Microsoft.Net.Http.Headers
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = { Id: string; A: bool; X: string }

type B = { Id: string; B: int; Y: string }


module ADomain =

    let create a =
        if a = false then
            Error [ Error.create 422 |> Error.setCode "custom" ]
        else
            Ok { Id = "1"; A = a; X = "" }

    let setA _x _a : A =
        failwith "Should not set properties that are consumed using the request parser"

    let setX x a = { a with X = x }



type Ctx = Ctx


module A =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("abs")
    let lookup = define.Operation.Lookup(fun _ _ -> failwith "not used")
    let get = define.Operation.GetResource()

    let a = define.Attribute.SimpleBool().Get(fun a -> a.A).Set(ADomain.setA)

    let x = define.Attribute.SimpleString().Get(fun a -> a.X).Set(ADomain.setX)

    let readonly = define.Attribute.SimpleString().Get(fun _ -> "test")

    let post =
        define.Operation.PostCustomAsync(fun ctx parser helper -> async {
            let parser = parser.For(id, a)

            match helper.ValidateRequest parser with
            | Error errs -> return Error errs
            | Ok() ->
                match! parser.ParseTask() |> Async.AwaitTask with
                | Error errs -> return Error errs
                | Ok a ->
                    match ADomain.create a with
                    | Error errs -> return Error errs
                    | Ok entity ->
                        match! helper.RunSettersAsync(entity, parser) with
                        | Error errs -> return Error errs
                        | Ok entity ->
                            if entity.X <> "" then
                                return helper.ReturnCreatedEntity entity |> Ok
                            else
                                return helper.Return202Accepted() |> Ok
        })



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

    let post: CustomPostOperation<Ctx2, Ctx2, A> =
        define.Operation
            .ForContextRes(fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ])
            .PostCustomTask(fun _ _ _ -> failwith "not used")


module A2 =

    let define = Define<Ctx3, A, string>()
    let resId = define.Id.Simple(fun _ -> failwith "not used")
    let resDef = define.Resource("a", resId).CollectionName("abs")


module D =

    let define = Define<Ctx, unit, string>()
    let resId = define.Id.Simple(fun () -> "notUsed")
    let resDef = define.Resource("d", resId).CollectionName("ds")

    let post =
        define.Operation.PostCustomAsync(fun ctx parser helper -> async {
            let eTag = EntityTagHeaderValue.FromString false "valid-etag"
            let lastModified = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)

            match helper.ValidatePreconditions(eTag, lastModified) with
            | Error errs -> return Error errs
            | Ok() -> return helper.Return202Accepted() |> Ok
        })


module E =

    let define = Define<Ctx, unit, string>()
    let resId = define.Id.Simple(fun () -> "notUsed")
    let resDef = define.Resource("e", resId).CollectionName("es")

    let post =
        define.Operation.PostCustomAsync(fun ctx parser helper -> async {
            let eTag = EntityTagHeaderValue.FromString false "valid-etag"
            let lastModified = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)

            match helper.ValidatePreconditions(eTag, lastModified, isOptional = true) with
            | Error errs -> return Error errs
            | Ok() -> return helper.Return202Accepted() |> Ok
        })


[<Tests>]
let tests =
    testList "POST collection custom" [

        testJob "Create 1: Returns 201, runs setters and returns correct data if successful" {
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "a"
                            attributes = {| a = true; x = "abc" |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 201
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "a" @>
            test <@ json |> getPath "data.id" = "1" @>
            test <@ json |> getPath "data.attributes.a" = true @>
            test <@ json |> getPath "data.attributes.x" = "abc" @>

            test <@ response.headers[Location] = "http://example.com/abs/1" @>
        }

        testJob "Insensitive to trailing slashes" {
            let! response =
                Request.post Ctx "/abs/"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "a"
                            attributes = {| a = true; x = "abc" |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 201
        }

        testJob "Create 2: Returns 202 and runs setters" {
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "a"
                            attributes = {| a = true |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 202
            let! json = response |> Response.readBodyAsString
            test <@ json = "" @>
            test <@ response.headers.ContainsKey Location = false @>
        }

        testJob "Returns 403 when read-only" {
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "a"
                            attributes = {| a = true; readonly = "foo" |}
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

        testJob "Returns 403 when client-generated ID is not supported" {
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "a"
                            id = "foo"
                            attributes = {| a = true |}
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
            let! response = Request.post Ctx "/abs" |> getResponse
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
            let! response =
                Request.post Ctx "/abs"
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
            let! response =
                Request.post Ctx "/abs"
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
            let! response =
                Request.post Ctx "/abs"
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
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
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
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
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
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
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
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
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
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized
                    {|
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
            let! response =
                Request.postWithoutStrictMode Ctx "/abs"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "a"
                            attributes = {|
                                a = true
                                x = "abc"
                                nullable = "foo"
                                nonExistentAttribute = "foo"
                            |}
                            relationships = {|
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
            let! response =
                Request.post Ctx "/abs"
                |> Request.bodySerialized {| data = {| ``type`` = "c" |} |}
                |> getResponse

            response |> testStatusCode 409
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "409" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'abs' does not support creating resources with type 'c'; expected 'a'"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if not supported at all" {
            let! response =
                Request.post Ctx3 "/abs"
                |> Request.bodySerialized
                    {|
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

        testJob "Correctly handles precondition validation using ETag" {
            let! response =
                Request.post Ctx "/ds"
                |> Request.bodySerialized {| data = {| ``type`` = "d" |} |}
                |> getResponse

            response |> testStatusCode 428
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "428" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match or If-Unmodified-Since headers"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx "/ds"
                |> Request.setHeader (IfMatch "\"invalid-etag\"")
                |> Request.bodySerialized {| data = {| ``type`` = "d" |} |}
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey ETag = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx "/ds"
                |> Request.setHeader (IfMatch "\"valid-etag\"")
                |> Request.bodySerialized {| data = {| ``type`` = "d" |} |}
                |> getResponse

            test <@ response.headers.TryFind ETag <> Some "\"valid-etag\"" @>
            response |> testSuccessStatusCode
        }

        testJob "Correctly handles precondition validation using If-Unmodified-Since" {
            let! response =
                Request.post Ctx "/ds"
                |> Request.bodySerialized {| data = {| ``type`` = "d" |} |}
                |> getResponse

            response |> testStatusCode 428
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "428" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match or If-Unmodified-Since headers"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx "/ds"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> Request.bodySerialized {| data = {| ``type`` = "d" |} |}
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx "/ds"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> Request.bodySerialized {| data = {| ``type`` = "d" |} |}
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testSuccessStatusCode
        }

        testJob "Correctly handles optional precondition validation using ETag" {
            let! response =
                Request.post Ctx "/es"
                |> Request.bodySerialized {| data = {| ``type`` = "e" |} |}
                |> getResponse

            test <@ response.headers.TryFind ETag <> Some "\"valid-etag\"" @>
            response |> testSuccessStatusCode

            let! response =
                Request.post Ctx "/es"
                |> Request.setHeader (IfMatch "\"invalid-etag\"")
                |> Request.bodySerialized {| data = {| ``type`` = "e" |} |}
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey ETag = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx "/es"
                |> Request.setHeader (IfMatch "\"valid-etag\"")
                |> Request.bodySerialized {| data = {| ``type`` = "e" |} |}
                |> getResponse

            test <@ response.headers.TryFind ETag <> Some "\"valid-etag\"" @>
            response |> testSuccessStatusCode
        }

        testJob "Correctly handles optional precondition validation using If-Unmodified-Since" {
            let! response =
                Request.post Ctx "/es"
                |> Request.bodySerialized {| data = {| ``type`` = "e" |} |}
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testSuccessStatusCode

            let! response =
                Request.post Ctx "/es"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> Request.bodySerialized {| data = {| ``type`` = "e" |} |}
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.post Ctx "/es"
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> Request.bodySerialized {| data = {| ``type`` = "e" |} |}
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testSuccessStatusCode
        }

        testJob "Returns error if collection case does not match" {
            let! response = Request.post Ctx "/Abs" |> getResponse
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
