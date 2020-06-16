module Routing

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx = Ctx

type A = A

module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun _-> "1")
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> Some A)
  let post = define.Operation.Post(fun () -> A).AfterCreate(ignore)
  let knownRel = define.Relationship.ToOne(resDef).Get(fun _ -> A)

[<Tests>]
let tests =
  testList "Routing" [

    testJob "Unknown/invalid collections fall through" {
      let! response =
        Request.get Ctx "/unknown"
        |> Request.setHeader (Accept "text/plain")
        |> getResponse
      response |> testStatusCode 404
      let! content = response |> Response.readBodyAsString
      test <@ content = "" @>
    }

    testJob "Unknown link or relationship related URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/unknownRel"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The link or relationship 'unknownRel' does not exist for any resource in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown relationship self URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/relationships/unknownRel"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The link or relationship 'unknownRel' does not exist for any resource in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after unknown relationship related URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/unknownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'unknownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after known relationship related URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/knownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'knownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after unknown relationship self URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/relationships/unknownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'relationships/unknownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Unknown path after known relationship self URL gives 404" {
      let! response =
        Request.get Ctx "/as/ignoredId/relationships/knownRel/path1/path2"
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path 'relationships/knownRel/path1' does not exist for resources in collection 'as'" @>
      test <@ json |> hasNoPath "errors[0].source.pointer" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

]
