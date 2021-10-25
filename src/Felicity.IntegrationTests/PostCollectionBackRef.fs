module ``POST collection backRef``

open System
open Microsoft.Net.Http.Headers
open Giraffe
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Child = {
  Id: string
}

type Parent = {
  Id: string
  Children: Child list
  LastModified: DateTimeOffset
  ETag: string
}


module ChildDomain =

  let create id = { Id = id }


module ParentDomain =

  let createChild parent childId =
    let child = ChildDomain.create childId
    { parent with Children = parent.Children @ [child] }, child




type Db () =
  let mutable Parents = [
    {
      Id = "p1"
      Children = [ { Id = "c1" } ]
      LastModified = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
      ETag = "valid-etag"
    }
  ]

  member _.SaveParent p =
    Parents <- p :: Parents

  member _.GetParentOrFail id =
    Parents |> List.find (fun p -> p.Id = id)



type Ctx = {
  AfterModifyParent: Parent -> Parent -> unit
  Db: Db
} with
  static member WithDb (db: Db) = {
    AfterModifyParent = fun _ pNew -> db.SaveParent pNew
    Db = db
  }


#nowarn "40"

[<AutoOpen>]
module rec ResourceModules =

  module Child =

    let define = Define<Ctx, Parent * Child, string>()
    let resId = define.Id.Simple(fun (_, c) -> c.Id)
    let resDef = define.Resource("child", resId).CollectionName("children")

    let parent =
      define.Relationship
        .ToOne(Parent.resDef)

    let post =
      define.Operation
        .PostBackRef(
          parent.Related(Parent.lookup),
          fun (ctx: Ctx, parent: Parent) parser -> parser.For(ParentDomain.createChild parent, resId)
        )
        .AfterCreate(fun (ctx: Ctx, pOld: Parent) (pNew, c) -> ctx.AfterModifyParent pOld pNew)



  module Parent =

    let define = Define<Ctx, Parent, string>()
    let resId = define.Id.Simple(fun p -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")
    let lookup = define.Operation.Lookup(fun ctx pid -> ctx.Db.GetParentOrFail pid |> Some)

    let get = define.Operation.GetResource()

    let ``as`` =
      define.Relationship
        .ToMany(Child.resDef)
        .Get(fun b -> b.Children |> List.map (fun a -> b, a))


type Ctx2 = {
  AfterModifyParent: Parent -> Parent -> unit
  Db: Db
} with
  static member WithDb (db: Db) = {
    AfterModifyParent = fun _ pNew -> db.SaveParent pNew
    Db = db
  }

[<AutoOpen>]
module rec ResourceModules2 =

  module Child2 =

    let define = Define<Ctx2, Parent * Child, string>()
    let resId = define.Id.Simple(fun (_, c) -> c.Id)
    let resDef = define.Resource("child", resId).CollectionName("children")

    let parent =
      define.Relationship
        .ToOne(Parent2.resDef)
        .Set(fun _ _ _ -> failwith "Should never be called")
        .AfterModifySelf(ignore)

    let post =
      define.Operation
        .PostBackRef(
          parent.Related(Parent2.lookup),
          fun (ctx: Ctx2, parent: Parent) parser -> parser.For(ParentDomain.createChild parent, resId)
        )
        .AfterCreate(fun (ctx: Ctx2, pOld: Parent) (pNew, c) -> ctx.AfterModifyParent pOld pNew)



  module Parent2 =

    let define = Define<Ctx2, Parent, string>()
    let resId = define.Id.Simple(fun p -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")
    let lookup = define.Operation.Lookup(fun ctx pid -> ctx.Db.GetParentOrFail pid |> Some)

    let get = define.Operation.GetResource()

    let ``as`` =
      define.Relationship
        .ToMany(Child2.resDef)
        .Get(fun b -> b.Children |> List.map (fun a -> b, a))


type Ctx3 = Ctx3


module rec ResourceModulesPreconditions =

  module Child =

    let define = Define<Ctx3, Parent * Child, string>()
    let resId = define.Id.Simple(fun (_, c) -> c.Id)
    let resDef = define.Resource("child", resId).CollectionName("children")

    let parent =
      define.Relationship
        .ToOne(Parent.resDef)

    let post =
      define.Operation
        .PostBackRef(
          parent.Related(Parent.lookup),
          fun (ctx: Ctx3, parent: Parent) parser -> parser.For(ParentDomain.createChild parent, resId)
        )
        .PeconditionsETag(fun (_, p) -> EntityTagHeaderValue.FromString false p.ETag)
        .PeconditionsLastModified(fun (_, p) -> p.LastModified)
        .AfterCreate(ignore)


  module Parent =

    let define = Define<Ctx3, Parent, string>()
    let resId = define.Id.Simple(fun p -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")
    let lookup = define.Operation.Lookup(fun ctx pid -> Db().GetParentOrFail("p1") |> Some )

    let get = define.Operation.GetResource()

    let children =
      define.Relationship
        .ToMany(Child.resDef)
        .Get(fun b -> b.Children |> List.map (fun a -> b, a))


type Ctx4 = Ctx4


module rec ResourceModulesPreconditionsOptional =

  module Child =

    let define = Define<Ctx4, Parent * Child, string>()
    let resId = define.Id.Simple(fun (_, c) -> c.Id)
    let resDef = define.Resource("child", resId).CollectionName("children")

    let parent =
      define.Relationship
        .ToOne(Parent.resDef)

    let post =
      define.Operation
        .PostBackRef(
          parent.Related(Parent.lookup),
          fun (ctx: Ctx4, parent: Parent) parser -> parser.For(ParentDomain.createChild parent, resId)
        )
        .PeconditionsETag(fun (_, p) -> EntityTagHeaderValue.FromString false p.ETag)
        .PeconditionsLastModified(fun (_, p) -> p.LastModified)
        .PeconditionsOptional
        .AfterCreate(ignore)


  module Parent =

    let define = Define<Ctx4, Parent, string>()
    let resId = define.Id.Simple(fun p -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")
    let lookup = define.Operation.Lookup(fun ctx pid -> Db().GetParentOrFail("p1") |> Some )

    let get = define.Operation.GetResource()

    let children =
      define.Relationship
        .ToMany(Child.resDef)
        .Get(fun b -> b.Children |> List.map (fun a -> b, a))


[<Tests>]
let tests =
  testList "POST collection backRef" [

    testJob "Returns 201, calls afterCreate with correct arguments, and returns correct data if successful" {
      let db = Db ()
      let mutable parents = None
      let ctx = { Ctx.WithDb db with AfterModifyParent = fun pOld pNew -> parents <- Some (pOld, pNew); db.SaveParent pNew }
      let! response =
        Request.post ctx "/children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 201
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "child" @>
      test <@ json |> getPath "data.id" = "c2" @>

      let expectedOldParent = {
        Id = "p1"
        Children = [ { Id = "c1" } ]
        LastModified = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
        ETag = "valid-etag"
      }

      let expectedNewParent = {
        Id = "p1"
        Children = [ { Id = "c1" }; { Id = "c2" } ]
        LastModified = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
        ETag = "valid-etag"
      }

      let parents' = parents
      test <@ parents' = Some (expectedOldParent, expectedNewParent) @>

      let p = db.GetParentOrFail "p1"
      test <@ p = expectedNewParent @>
    }

    testJob "Insensitive to trailing slashes" {
      let ctx = Ctx.WithDb (Db ())
      let! response =
        Request.post ctx "/children/"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 201
    }

    testJob "Does not run backRef setter" {
      let db = Db ()
      let ctx = Ctx2.WithDb db
      let! response =
        Request.post ctx "/children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      // Will fail with exception if setter is run, so just check for success status code
      response |> testSuccessStatusCode
    }

    testJob "Returns 400 when backRef relationship not supplied" {
      let db = Db ()
      let ctx = Ctx.WithDb db
      let! response =
        Request.post ctx "/children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                |}
            |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'parent' is required for this operation" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Correctly handles precondition validation using ETag" {
      let! response =
        Request.post Ctx3 "/children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match or If-Unmodified-Since headers" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx3 "/children"
        |> Request.setHeader (IfMatch "\"invalid-etag\"")
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey ETag = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx3 "/children"
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      test <@ response.headers.[ETag] <> "\"valid-etag\"" @>
      response |> testSuccessStatusCode
    }

    testJob "Correctly handles precondition validation using If-Unmodified-Since" {
      let! response =
        Request.post Ctx3 "/children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match or If-Unmodified-Since headers" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx3 "/children"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx3 "/children"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testSuccessStatusCode
    }

    testJob "Correctly handles optional precondition validation using ETag" {
      let! response =
        Request.post Ctx4 "/children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      test <@ response.headers.[ETag] <> "\"valid-etag\"" @>
      response |> testSuccessStatusCode

      let! response =
        Request.post Ctx4 "/children"
        |> Request.setHeader (IfMatch "\"invalid-etag\"")
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey ETag = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx4 "/children"
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      test <@ response.headers.[ETag] <> "\"valid-etag\"" @>
      response |> testSuccessStatusCode
    }

    testJob "Correctly handles optional precondition validation using If-Unmodified-Since" {
      let! response =
        Request.post Ctx4 "/children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testSuccessStatusCode

      let! response =
        Request.post Ctx4 "/children"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Match or If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.post Ctx4 "/children"
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> Request.bodySerialized
            {|data =
                {|``type`` = "child"
                  id = "c2"
                  relationships = {| parent = {| data = {| ``type`` = "parent"; id = "p1" |} |} |}
                |}
            |}
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testSuccessStatusCode
    }

    testJob "Falls through if collection case does not match" {
      let ctx = Ctx.WithDb (Db ())
      let! response = Request.post ctx "/Children" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json = "" @>
    }

  ]
