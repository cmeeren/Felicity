module ResourceLocking

open System
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Hopac
open Felicity


// Only test with one type of operation; assume locking for all operations work the same


type Ctx = Ctx of int ref


module A =

  let define = Define<Ctx, string, string>()
  let resId = define.Id.Simple(id)
  let resDef = define.Resource("a", resId) .CollectionName("as")
  let lookup = define.Operation.Lookup(Some)

  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .CustomLink()
      .PostAsync(fun (Ctx i) parser responder _ -> i := !i + 1; setStatusCode 200 |> Ok |> async.Return)


module B =

  let define = Define<Ctx, string, string>()
  let resId = define.Id.Simple(id)
  let resDef =
    define.Resource("b", resId)
      .CollectionName("bs")
      .Lock()
  let lookup = define.Operation.Lookup(Some)

  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .CustomLink()
      .PostAsync(fun (Ctx i) parser responder _ -> i := !i + 1; setStatusCode 200 |> Ok |> async.Return)


module C =

  let define = Define<Ctx, string, string>()
  let resId = define.Id.Simple(id)
  let resDef =
    define.Resource("c", resId)
      .CollectionName("cs")
      .LockOther(B.resDef, Some >> Job.result)
  let lookup = define.Operation.Lookup(Some)

  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .CustomLink()
      .PostAsync(fun (Ctx i) parser responder _ -> i := !i + 1; setStatusCode 200 |> Ok |> async.Return)


module D =

  let define = Define<Ctx, string, string>()
  let resId = define.Id.Simple(id)
  let resDef =
    define.Resource("d", resId)
      .CollectionName("ds")
      .Lock(TimeSpan.FromMilliseconds 10.)
  let lookup = define.Operation.Lookup(Some)

  let get = define.Operation.GetResource()

  let customOp =
    define.Operation
      .CustomLink()
      .PostAsync(fun ctx parser responder _ ->
        async {
          do! Async.Sleep 10000
          return setStatusCode 200 |> Ok
        })


[<Tests>]
let tests =
  testSequenced <| testList "Resource locking" [

    testJob "Unlocked resources are not thread safe" {
      let i = ref 0
      let ctx = Ctx i
      do!
        Request.post ctx "/as/ignoredId/customOp"
        |> getResponse
        |> Array.replicate 1000
        |> Array.map Job.toAsync
        |> Async.Parallel
        |> Async.Ignore<Response []>
      test <@ !i < 1000 @>
    }

    testJob "Locked resources are thread-safe" {
      let i = ref 0
      let ctx = Ctx i
      let testClient = startTestServer ctx
      do!
        Request.createWithClient testClient Post (Uri("http://example.com/bs/someId/customOp"))
        |> Request.jsonApiHeaders
        |> getResponse
        |> Array.replicate 1000
        |> Array.map Job.toAsync
        |> Async.Parallel
        |> Async.Ignore<Response []>
      test <@ !i = 1000 @>
    }

    testJob "Cross-locked resources are collectively thread-safe when IDs match" {
      let i = ref 0
      let ctx = Ctx i
      let testClient = startTestServer ctx
      do!
        [|
          Request.createWithClient testClient Post (Uri("http://example.com/bs/someId/customOp"))
          Request.createWithClient testClient Post (Uri("http://example.com/cs/someId/customOp"))
        |]
        |> Array.map (Request.jsonApiHeaders >> getResponse)
        |> Array.replicate 500
        |> Array.collect id
        |> Array.map Job.toAsync
        |> Async.Parallel
        |> Async.Ignore<Response []>
      test <@ !i = 1000 @>
    }

    testJob "Cross-locked resources are not collectively thread-safe when IDs don't match" {
      let i = ref 0
      let ctx = Ctx i
      let testClient = startTestServer ctx
      do!
        [|
          Request.createWithClient testClient Post (Uri("http://example.com/bs/id1/customOp"))
          Request.createWithClient testClient Post (Uri("http://example.com/cs/id2/customOp"))
        |]
        |> Array.map (Request.jsonApiHeaders >> getResponse)
        |> Array.replicate 500
        |> Array.collect id
        |> Array.map Job.toAsync
        |> Async.Parallel
        |> Async.Ignore<Response []>
      test <@ !i < 1000 @>
    }

    testJob "Returns 503 if error times out" {
      let testClient = startTestServer (Ctx (ref 0))
      let getJob () =
        Request.createWithClient testClient Post (Uri("http://example.com/ds/someId/customOp"))
        |> Request.jsonApiHeaders
        |> getResponse

      do! getJob () |> Job.toAsync |> Async.StartChild |> Async.Ignore 
      let! secondResp = getJob ()
      secondResp |> testStatusCode 503
      test <@ secondResp.headers.ContainsKey LastModified = false @>
      let! json = secondResp |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "503" @>
      test <@ json |> getPath "errors[0].detail" = "Timed out waiting for the completion of other operations on the requested resource" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
