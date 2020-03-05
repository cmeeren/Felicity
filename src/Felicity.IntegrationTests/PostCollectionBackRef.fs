module ``POST collection backRef``

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
        //.Set(fun _ _ _ -> failwith "Should never be called")

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

      let expectedOldParent = { Id = "p1"; Children = [ { Id = "c1" } ] }
      let expectedNewParent = { Id = "p1"; Children = [ { Id = "c1" }; { Id = "c2" } ] }

      let parents' = parents
      test <@ parents' = Some (expectedOldParent, expectedNewParent) @>

      let p = db.GetParentOrFail "p1"
      test <@ p = expectedNewParent @>
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

  ]
