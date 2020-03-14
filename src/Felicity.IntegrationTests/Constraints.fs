module Constraints

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx = Ctx

type X = X
type Y = Y of bool


module X =

  let define = Define<Ctx, X, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("x", resId).CollectionName("xs")

  let a =
    define.Attribute
      .Simple()
      .AddConstraint("foo", 1)
      .AddConstraint("bar", true)
      .AddConstraint("baz", [123; 456])
      .Get(fun _ -> true)

  let toOne =
    define.Relationship
      .ToOne(resDef)
      .AddConstraint("foo2", 2)
      .AddConstraint("bar2", false)
      .AddConstraint("baz2", ["foo"; "bar"])
      // No getter so that we test that constraints are also visible for fields without
      // getter

  let toOneNullable =
    define.Relationship
      .ToOneNullable(resDef)
      .AddConstraint("foo3", 3)
      .AddConstraint("bar3", true)
      .AddConstraint("baz3", ["abc"; "def"])
      .Get(fun _ _ -> failwith "not used")

  let toMany =
    define.Relationship
      .ToMany(resDef)
      .AddConstraint("foo4", 4)
      .AddConstraint("bar4", false)
      .AddConstraint("baz4", ["123"; "456"])
      .Get(fun _ _ -> failwith "not used")

  let lookup = define.Operation.Lookup(fun _ -> Some X)

  let getColl = define.Operation.GetCollection(fun () -> [X])
  let post = define.Operation.Post(fun () -> X).AfterCreate(ignore)
  let get = define.Operation.GetResource()
  let patch = define.Operation.Patch().AfterUpdate(ignore)


module Y =

  let define = Define<Ctx, Y, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("y", resId).CollectionName("ys")

  let constraints =
    define.Attribute
      .Simple()
      .Get(fun (Y b) -> b)
      .Set(fun b _ -> Y b)

  let lookup = define.Operation.Lookup(fun _ -> Some (Y false))
  let get = define.Operation.GetResource()
  let patch = define.Operation.Patch().AfterUpdate(ignore)


[<Tests>]
let tests =
  testList "Constraints" [

    testJob "Returns all constraints when all fields are included" {
      let! response = Request.get Ctx "/xs" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].attributes.constraints.a.foo" = 1 @>
      test <@ json |> getPath "data[0].attributes.constraints.a.bar" = true @>
      test <@ json |> getPath "data[0].attributes.constraints.a.baz" = [123; 456] @>
      test <@ json |> getPath "data[0].attributes.constraints.toOne.foo2" = 2 @>
      test <@ json |> getPath "data[0].attributes.constraints.toOne.bar2" = false @>
      test <@ json |> getPath "data[0].attributes.constraints.toOne.baz2" = ["foo"; "bar"] @>
      test <@ json |> getPath "data[0].attributes.constraints.toOneNullable.foo3" = 3 @>
      test <@ json |> getPath "data[0].attributes.constraints.toOneNullable.bar3" = true @>
      test <@ json |> getPath "data[0].attributes.constraints.toOneNullable.baz3" = ["abc"; "def"] @>
      test <@ json |> getPath "data[0].attributes.constraints.toMany.foo4" = 4 @>
      test <@ json |> getPath "data[0].attributes.constraints.toMany.bar4" = false @>
      test <@ json |> getPath "data[0].attributes.constraints.toMany.baz4" = ["123"; "456"] @>
    }

    testJob "Does not return constraints for excluded fields" {
      let! response = Request.get Ctx "/xs?fields[x]=a,constraints" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].attributes.constraints.a.foo" = 1 @>
      test <@ json |> getPath "data[0].attributes.constraints.a.bar" = true @>
      test <@ json |> getPath "data[0].attributes.constraints.a.baz" = [123; 456] @>
      test <@ json |> hasNoPath "data[0].attributes.constraints.toOne" @>
      test <@ json |> hasNoPath "data[0].attributes.constraints.toOneNullable" @>
      test <@ json |> hasNoPath "data[0].attributes.constraints.toMany" @>
    }

    testJob "Does not return constraints if excluded" {
      let! response = Request.get Ctx "/xs?fields[x]=a,b" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> hasNoPath "data[0].attributes.constraints" @>
    }

    testJob "Returns 403 when included in POST" {
      let! response =
        Request.post Ctx "/xs"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "x"
                  attributes = {| constraints = obj() |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Attribute 'constraints' is read-only" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/constraints" @>
    }

    testJob "Returns 403 when included in PATCH" {
      let! response =
        Request.patch Ctx "/xs/someId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "x"
                  id = "someId"
                  attributes = {| constraints = obj() |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Attribute 'constraints' is read-only" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/constraints" @>
    }

    testJob "Can update a normal attribute called 'constraints'" {
      // Sanity check
      let! response = Request.get Ctx "/ys/someId" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.attributes.constraints" = false @>

      // Actual test
      let! response =
        Request.patch Ctx "/ys/someId"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "y"
                  id = "someId"
                  attributes = {| constraints = true |}
                |}
            |}
        |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.attributes.constraints" = true @>
    }

  ]
