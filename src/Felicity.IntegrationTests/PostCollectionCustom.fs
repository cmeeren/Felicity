module ``POST collection custom``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = {
  Id: string
  A: bool
  X: string
}

type B = {
  Id: string
  B: int
  Y: string
}


module ADomain =

  let create a =
    if a = false then Error [Error.create 422 |> Error.setCode "custom"]
    else Ok {
      Id = "1"
      A = a
      X = ""
    }

  let setA x a : A =
    failwith "Should not set properties that are consumed using the request parser"

  let setX x a =
    { a with X = x }



type Ctx = Ctx


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("abs")
  let lookup = define.Operation.Lookup(fun _ _ -> failwith "not used")
  let get = define.Operation.GetResource()

  let a =
    define.Attribute
      .SimpleBool()
      .Get(fun a -> a.A)
      .Set(ADomain.setA)

  let x =
    define.Attribute
      .SimpleString()
      .Get(fun a -> a.X)
      .Set(ADomain.setX)

  let readonly =
    define.Attribute
      .SimpleString()
      .Get(fun _ -> "test")

  let post =
    define.Operation
      .PostCustomAsync(fun ctx parser helper ->
        async {
          let parser = parser.For(id, a)
          match helper.ValidateRequest parser with
          | Error errs -> return Error errs
          | Ok () ->
              match! parser.ParseJob() |> Hopac.Job.toAsync with
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
        }
      )



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
  let post : CustomPostOperation<Ctx2, Ctx2, A> =
    define.Operation
      .ForContextRes(fun _ -> Error [Error.create 422 |> Error.setCode "custom"])
      .PostCustomJob(fun _ _ _ -> failwith "not used")


module A2 =

  let define = Define<Ctx3, A, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("a", resId).CollectionName("abs")


[<Tests>]
let tests =
  testList "POST collection custom" [

    testJob "Create 1: Returns 201, runs setters and returns correct data if successful" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  attributes =
                    {|a = true
                      x = "abc"
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

      test <@ response.headers.[Location] = "http://example.com/abs/1" @>
    }

    testJob "Create 2: Returns 202 and runs setters" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  attributes =
                    {|a = true
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 202
      let! json = response |> Response.readBodyAsString
      test <@ json  = "" @>
      test <@ response.headers.ContainsKey Location = false @>
    }

    testJob "Returns 403 when read-only" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  attributes =
                      {|a = true
                        readonly = "foo"
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

    testJob "Returns 403 when client-generated ID is not supported" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "foo"
                  attributes = {| a = true |}
                |}
            |}
        |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not support creating resource type 'a' with a client-generated ID" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when missing body" {
      let! response = Request.post Ctx "/abs" |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Request to create resources must contain a single resource object as primary data" @>
      test <@ json |> getPath "errors[0].source.pointer" = "" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when JSON is invalid" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodyString """
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
      test <@ json |> getPath "errors[0].detail" = "'\"' is invalid after a property name. Expected a ':'. Path: $.test | LineNumber: 3 | BytePositionInLine: 19." @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when missing data" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodySerialized {| meta = obj() |}
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
      test <@ json |> getPath "errors[0].detail" = "Request to create resources must contain a single resource object as primary data" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when missing type" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodySerialized
            {|data =
                {| attributes = {| a = true |} |}
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
            {|data =
                {|``type`` = null
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
            {|data =
                {|``type`` = "a"
                  id = null
                |}
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
            {|data =
                {|``type`` = "a"
                  attributes = null
                |}
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
            {|data =
                {|``type`` = "a"
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

    testJob "Ignores unknown members and relationships" {
      let! response =
        Request.post Ctx "/abs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  attributes =
                    {|a = true
                      x = "abc"
                      nullable = "foo"
                      nonExistentAttribute = "foo"
                    |}
                  relationships =
                    {|nonExistentRelationship =
                        {|data = null
                        |}
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
        |> Request.bodySerialized {| data = {|``type`` = "c" |} |}
        |> getResponse

      response |> testStatusCode 409
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "409" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not support creating resources with type 'c'; expected 'a'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/type" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported at all" {
      let! response =
        Request.post Ctx3 "/abs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  attributes =
                    {|a = true
                      x = "abc"
                    |}
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

  ]
