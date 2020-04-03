﻿module Routing

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

]
