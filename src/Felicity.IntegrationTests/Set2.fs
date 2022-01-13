module Set2

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx =
  Ctx of
    setNonNull: (string * int -> string -> string) *
    setNull12: (string option * int option -> string -> string) *
    setNull34: ((int * string) option -> string -> string)

module A9 =

  let define = Define<Ctx, string, string>()
  let resId = define.Id.Simple(id)
  let resDef = define.Resource("a", resId).CollectionName("as")

  let nonNull1 =
    define.Attribute
      .SimpleString()

  let nonNull2 =
    define.Attribute
      .SimpleInt()

  let null1 =
    define.Attribute
      .Nullable
      .SimpleString()

  let null2 =
    define.Attribute
      .Nullable
      .SimpleInt()

  let null3 =
    define.Attribute
      .Nullable
      .SimpleInt()

  let nullRel =
    define.Relationship
      .ToOneNullable(resDef)

  let setNonNull =
    define.Operation
      .Set2((fun (Ctx (set, _, _)) x e -> set x e), nonNull1, nonNull2)

  let setNull12 =
    define.Operation
      .Set2((fun (Ctx (_, set, _)) x e -> set x e), null1, null2)

  let setNull34 =
    define.Operation
      .Set2SameNull((fun (Ctx (_, _, set)) x e -> set x e), null3, nullRel)

  let post = define.Operation.Post(fun () -> "foobar").AfterCreate(fun _ -> ())

  let lookup = define.Operation.Lookup(fun _ id -> Some id)
  
  let get = define.Operation.GetResource()

  let patch =
    define.Operation
      .Patch()
      .AfterUpdate(fun (Ctx (_, _, _)) _ -> ())


[<Tests>]
let tests =
  testList "Set2" [

    testJob "Runs Set2 with non-nullable fields" {
      let mutable calledWith = ValueNone

      let ctx =
        Ctx (
          (fun x e -> calledWith <- ValueSome x; e),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| nonNull1 = "abc"; nonNull2 = 123 |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let calledWith = calledWith
      test <@ calledWith = ValueSome ("abc", 123) @>
    }

    testJob "Runs Set2 with nullable fields" {
      let mutable calledWith = ValueNone

      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun x e -> calledWith <- ValueSome x; e),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| null1 = "abc"; null2 = null |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let calledWith = calledWith
      test <@ calledWith = ValueSome (Some "abc", None) @>
    }

    testJob "Runs Set2SameNull when both non-null" {
      let mutable calledWith = ValueNone

      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun x e -> calledWith <- ValueSome x; e)
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| null3 = 123 |}
                  relationships = {| nullRel = {| data = {| ``type`` = "a"; id = "abc" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let calledWith = calledWith
      test <@ calledWith = ValueSome (Some (123, "abc")) @>
    }

    testJob "Runs Set2SameNull when both null" {
      let mutable calledWith = ValueNone

      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun x e -> calledWith <- ValueSome x; e)
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
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
      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| nonNull1 = "abc" |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Field 'nonNull1' can only be set together with field 'nonNull2', which was missing" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Set2 returns error if second is missing" {
      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| nonNull2 = 123 |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Field 'nonNull2' can only be set together with field 'nonNull1', which was missing" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Set2SameNull returns error if first is missing" {
      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| null3 = null |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Field 'null3' can only be set together with field 'nullRel', which was missing" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Set2SameNull returns error if second is missing" {
      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  relationships = {| nullRel = {| data = null |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Field 'nullRel' can only be set together with field 'null3', which was missing" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Set2SameNull returns error if first is null and second is non-null" {
      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| null3 = null |}
                  relationships = {| nullRel = {| data = {| ``type`` = "a"; id = "abc" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "The fields 'null3' and 'nullRel' must both be either null or non-null" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Set2SameNull returns error if first is non-null and second is null" {
      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.patch ctx "/as/ignoredId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "ignoredId"
                  attributes = {| null3 = 123 |}
                  relationships = {| nullRel = {| data = null |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "The fields 'null3' and 'nullRel' must both be either null or non-null" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "POST runs Set2 with non-nullable fields" {
      let mutable calledWith = ValueNone

      let ctx =
        Ctx (
          (fun x e -> calledWith <- ValueSome x; e),
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called")
        )

      let! response =
        Request.post ctx "/as"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  attributes = {| nonNull1 = "abc"; nonNull2 = 123 |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 201
      let calledWith = calledWith
      test <@ calledWith = ValueSome ("abc", 123) @>
    }

    testJob "POST runs Set2SameNull when both non-null" {
      let mutable calledWith = ValueNone

      let ctx =
        Ctx (
          (fun _ -> failwith "not called"),
          (fun _ -> failwith "not called"),
          (fun x e -> calledWith <- ValueSome x; e)
        )

      let! response =
        Request.post ctx "/as"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  attributes = {| null3 = 123 |}
                  relationships = {| nullRel = {| data = {| ``type`` = "a"; id = "abc" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 201
      let calledWith = calledWith
      test <@ calledWith = ValueSome (Some (123, "abc")) @>
    }

  ]
