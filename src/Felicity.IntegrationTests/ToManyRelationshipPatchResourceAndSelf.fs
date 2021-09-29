module ``To-many relationship PATCH resource and self``

open System
open System.Text.Json.Serialization
open Microsoft.Net.Http.Headers
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type Child3 = {
  Id: string
}

type Child1 = {
  Id: string
  Child: Child3
}

type Child2 = {
  Id: string
}

type Child = C1 of Child1 | C2 of Child2


type Parent1 = {
  Id: string
  Children: Child list
  OtherChildIds: string list
}

type Parent2 = {
  Id: string
  Children: Child list
}

type Parent3 = {
  Id: string
}

type Parent4 = {
  Id: string
}

type Parent = P1 of Parent1 | P2 of Parent2 | P3 of Parent3 | P4 of Parent4


type Db () =
  let mutable parents : Map<string, Parent> =
    Map.empty
    |> Map.add "p1" (P1 { Id = "p1"; Children = [C1 { Id = "c1"; Child = { Id = "c3" } }; C2 { Id = "c2" }]; OtherChildIds = ["c2"; "c1"] })
    |> Map.add "p2" (P2 { Id = "p2"; Children = [C2 { Id = "c2" }; C1 { Id = "c1"; Child = { Id = "c3" } }] })
    |> Map.add "p3" (P3 { Id = "p3" })
    |> Map.add "p4" (P4 { Id = "p4" })

  let mutable children : Map<string, Child> =
    Map.empty
    |> Map.add "c1" (C1 { Id = "c1"; Child = { Id = "c3" } })
    |> Map.add "c2" (C2 { Id = "c2" })

  member _.TryGetParent id =
    parents.TryFind id

  member _.TryGetChild id =
    children.TryFind id

  member _.Save1 (p1: Parent1) =
    parents <- parents.Add(p1.Id, P1 p1)

  member _.Save2 (p2: Parent2) =
    parents <- parents.Add(p2.Id, P2 p2)


type Ctx = {
  Db: Db
  ModifyPatchSelfOkResponse: Parent1 -> Child list -> HttpHandler
  ModifyPatchSelfAcceptedResponse: Parent1 -> HttpHandler
  GetParent1Children: Parent1 -> Child list Skippable
  SetChildren1: Child list -> Parent1 -> Result<Parent1, Error list>
  SetOtherChildrenIds1: string list -> Parent1 -> Result<Parent1, Error list>
  ParseChild2Id: string -> Result<string, Error list>
  LookupChild: string -> Result<Child option, Error list>
  BeforeModifySelf1: Parent1 -> Result<unit, Error list>
  AfterUpdate1: Parent1 -> Parent1 -> unit
} with
  static member WithDb db = {
    Db = db
    ModifyPatchSelfOkResponse = fun _ _ -> fun next ctx -> next ctx
    ModifyPatchSelfAcceptedResponse = fun _ -> fun next ctx -> next ctx
    GetParent1Children = fun p -> Include p.Children
    SetChildren1 = fun c p -> Ok { p with Children = c }
    SetOtherChildrenIds1 = fun id p -> Ok { p with OtherChildIds = id }
    ParseChild2Id = Ok
    LookupChild = db.TryGetChild >> Ok
    BeforeModifySelf1 = fun _ -> Ok ()
    AfterUpdate1 = fun _ e -> db.Save1 e
  }


module Child3 =

  let define = Define<Ctx, Child3, string>()
  let resId = define.Id.Simple(fun (c: Child3) -> c.Id)
  let resDef = define.Resource("child3", resId)
  let c = define.Attribute.SimpleString().Get(fun _ -> "abc")


module Child1 =

  let define = Define<Ctx, Child1, string>()
  let resId = define.Id.Simple(fun (c: Child1) -> c.Id)
  let resDef = define.Resource("child1", resId)
  let subChild =
    define.Relationship
      .ToOne(Child3.resDef)
      .Get(fun c -> c.Child)


module Child2 =

  let define = Define<Ctx, Child2, string>()
  let resId = define.Id.ParsedRes(id, (fun ctx -> ctx.ParseChild2Id), fun (c: Child2) -> c.Id)
  let resDef = define.Resource("child2", resId)


module Child =

  let define = Define<Ctx, Child, string>()

  let lookup =
    define.Operation
      .Polymorphic
      .LookupRes(
        (fun ctx id -> ctx.LookupChild id),
        function
          | C1 c -> Child1.resDef.PolymorphicFor c
          | C2 c -> Child2.resDef.PolymorphicFor c
      )



module Parent1 =  // set and get - PATCH resource/self OK

  let define = Define<Ctx, Parent1, string>()
  let resId = define.Id.Simple(fun (p: Parent1) -> p.Id)
  let resDef = define.Resource("parent1", resId).CollectionName("parents")
  let get = define.Operation.GetResource()

  let children =
    define.Relationship
      .Polymorphic()
      .AddIdParser(Child1.resDef, id)
      .AddIdParser(Child2.resDef, id)
      .ResolveEntity(function
        | C1 c -> Child1.resDef.PolymorphicFor c
        | C2 c -> Child2.resDef.PolymorphicFor c
      )
      .ToMany()
      .GetSkip(fun ctx p -> ctx.GetParent1Children p)
      .SetAllRes(Child.lookup, fun ctx -> ctx.SetChildren1)
      .BeforeModifySelfRes(fun ctx p -> ctx.BeforeModifySelf1 p)
      .AfterModifySelf(fun ctx -> ctx.AfterUpdate1)
      .ModifyPatchSelfOkResponse(fun ctx -> ctx.ModifyPatchSelfOkResponse)

  let otherChildren =
    define.Relationship
      .Polymorphic()
      .AddIdParser(Child1.resDef, id)
      .AddIdParser(Child2.resDef, id)
      .ResolveEntity(function
        | C1 c -> Child1.resDef.PolymorphicFor c
        | C2 c -> Child2.resDef.PolymorphicFor c
      )
      .ToMany()
      .Get(fun ctx p -> p.OtherChildIds |> List.map (ctx.Db.TryGetChild >> Option.defaultWith (fun () -> failwith "Not found")))
      .SetAllRes(fun ctx -> ctx.SetOtherChildrenIds1)
      .AfterModifySelf(fun ctx -> ctx.AfterUpdate1)
      .ModifySelfReturn202Accepted()
      .ModifyPatchSelfAcceptedResponse(fun ctx -> ctx.ModifyPatchSelfAcceptedResponse)

  let patch =
    define.Operation
      .Patch()
      .AfterUpdate(fun ctx p -> ctx.Db.Save1 p)


module Parent2 =  // set without get - PATCH resource OK, PATCH self error

  let define = Define<Ctx, Parent2, string>()
  let resId = define.Id.Simple(fun (p: Parent2) -> p.Id)
  let resDef = define.Resource("parent2", resId).CollectionName("parents")
  let get = define.Operation.GetResource()

  let children =
    define.Relationship
      .Polymorphic()
      .AddIdParser(Child1.resDef, id)
      .AddIdParser(Child2.resDef, id)
      .ResolveEntity(function
        | C1 c -> Child1.resDef.PolymorphicFor c
        | C2 c -> Child2.resDef.PolymorphicFor c
      )
      .ToMany()
      .SetAll(Child.lookup, fun c (p: Parent2) -> { p with Children = c })
      .AfterModifySelf(fun ctx -> ctx.Db.Save2)

  let supportsPost : ToManyRelationship<Ctx, Ctx, Parent2, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .SetAll(fun (_: string list) _ -> failwith "not used")
      .Add(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let supportsDelete : ToManyRelationship<Ctx, Ctx, Parent2, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .SetAll(fun (_: string list) _ -> failwith "not used")
      .Remove(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let supportsPostAndDelete : ToManyRelationship<Ctx, Ctx, Parent2, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .Add(fun (_: string list) _ -> failwith "not used")
      .SetAll(fun (_: string list) _ -> failwith "not used")
      .Remove(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let patch =
    define.Operation
      .Patch()
      .AfterUpdate(fun ctx p -> ctx.Db.Save2 p)


module Parent3 =  // no set - PATCH resource/self error

  let define = Define<Ctx, Parent3, string>()
  let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
  let resDef = define.Resource("parent3", resId).CollectionName("parents")
  let get = define.Operation.GetResource()

  let children =
    define.Relationship
      .Polymorphic()
      .AddIdParser(Child1.resDef, id)
      .AddIdParser(Child2.resDef, id)
      .ResolveEntity(function
        | C1 c -> Child1.resDef.PolymorphicFor c
        | C2 c -> Child2.resDef.PolymorphicFor c
      )
      .ToMany()
      .Get(fun _ -> [])

  let supportsPost : ToManyRelationship<Ctx, Ctx, Parent3, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .Add(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let supportsDelete : ToManyRelationship<Ctx, Ctx, Parent3, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .Remove(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let supportsPostAndDelete : ToManyRelationship<Ctx, Ctx, Parent3, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .Add(fun (_: string list) _ -> failwith "not used")
      .Remove(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let supportsPostUnique : ToManyRelationship<Ctx, Ctx, Parent3, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .Add(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let supportsDeleteUnique : ToManyRelationship<Ctx, Ctx, Parent3, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .Remove(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)

  let supportsPostAndDeleteUnique : ToManyRelationship<Ctx, Ctx, Parent3, Child1, string> =
    define.Relationship
      .ToMany(Child1.resDef)
      .Get(fun _ _ -> failwith "not used")
      .Add(fun (_: string list) _ -> failwith "not used")
      .Remove(fun (_: string list) _ -> failwith "not used")
      .AfterModifySelf(ignore)


  let patch =
    define.Operation
      .Patch()
      .AfterUpdate(ignore)


module Parent4 =  // no relationship at all

  let define = Define<Ctx, Parent4, string>()
  let resId = define.Id.Simple(fun (p: Parent4) -> p.Id)
  let resDef = define.Resource("parent4", resId).CollectionName("parents")


module Parent =

  let define = Define<Ctx, Parent, string>()

  let resId = define.Id.Simple(function P1 p -> p.Id | P2 p -> p.Id | P3 p -> p.Id | P4 p -> p.Id)

  let resDef =
    define.PolymorphicResource(resId)
      .CollectionName("parents")

  let lookup =
    define.Operation
      .Polymorphic
      .Lookup(
        (fun ctx id -> ctx.Db.TryGetParent id),
        function
          | P1 p -> Parent1.resDef.PolymorphicFor p
          | P2 p -> Parent2.resDef.PolymorphicFor p
          | P3 p -> Parent3.resDef.PolymorphicFor p
          | P4 p -> Parent4.resDef.PolymorphicFor p
      )


type Ctx2 = Ctx2

module Parent5 =

  let define = Define<Ctx2, Parent4, string>()
  let resId = define.Id.Simple(fun x -> x.Id)
  let resDef = define.Resource("p5", resId).CollectionName("parents")
  let lookup = define.Operation.Lookup(fun _ -> Some { Parent4.Id = "a1" })

  let children : ToManyRelationship<Ctx2, Ctx2, Parent4, obj, string> =
    define.Relationship
      .Polymorphic()
      .ToMany()


type Ctx3 = Ctx3

module Parent6 =

  let define = Define<Ctx3, Parent1, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("p6", resId).CollectionName("parents")


type Ctx4 = Ctx4

module Parent7 =  // ETag precondition

  let define = Define<Ctx4, Parent3, string>()
  let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
  let resDef = define.Resource("parent", resId).CollectionName("parents")
  let lookup = define.Operation.Lookup(fun _ -> Some { Parent3.Id = "someId" })
  let get = define.Operation.GetResource()
  let preconditions = define.Preconditions.ETag(fun _ -> EntityTagHeaderValue.FromString false "valid-etag")

  let children =
    define.Relationship
      .ToMany(resDef)
      .Get(fun ctx -> [])
      .SetAll(fun ctx e -> e)
      .AfterModifySelf(ignore)


type Ctx5 = Ctx5

module Parent8 =  // LastModified precondition

  let define = Define<Ctx5, Parent3, string>()
  let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
  let resDef = define.Resource("parent", resId).CollectionName("parents")
  let lookup = define.Operation.Lookup(fun _ -> Some { Parent3.Id = "someId" })
  let get = define.Operation.GetResource()
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))

  let children =
    define.Relationship
      .ToMany(resDef)
      .Get(fun ctx -> [])
      .SetAll(fun ctx e -> e)
      .AfterModifySelf(ignore)


type Ctx6 = Ctx6

module Parent9 =  // Optional precondition

  let define = Define<Ctx6, Parent3, string>()
  let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
  let resDef = define.Resource("parent", resId).CollectionName("parents")
  let lookup = define.Operation.Lookup(fun _ -> Some { Parent3.Id = "someId" })
  let get = define.Operation.GetResource()
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)).Optional

  let children =
    define.Relationship
      .ToMany(resDef)
      .Get(fun ctx -> [])
      .SetAll(fun ctx e -> e)
      .AfterModifySelf(ignore)


type Ctx7 = Ctx7
type MappedCtx = MappedCtx


module MapCtxCompileTest =

  let define = Define<Ctx7, string, string>()
  let resId = define.Id.Simple(id)
  let resDef = define.Resource("x", resId).CollectionName("xs")
  let lookup = define.Operation.Lookup(fun _ -> failwith "never called")
  let get = define.Operation.GetResource()

  let rel =
    define.Relationship
      .MapSetContext(fun _ -> MappedCtx)
      .ToMany(resDef)
      .Get(fun _ _ -> failwith "never called")
      .SetAll(fun (ctx: MappedCtx) _ _ -> failwith "never called")
      .BeforeModifySelf(fun (_: MappedCtx) _ -> ())
      .AfterModifySelf(fun (_: MappedCtx) _ -> ())
      .ModifyPatchSelfAcceptedResponse(fun (_: MappedCtx) _ _ -> ())
      .ModifyPatchSelfOkResponse(fun (_: MappedCtx) _ _ _ -> ())


[<Tests>]
let tests1 =
  testList "To-many relationship PATCH resource" [

    testJob "Get/set: Returns 200, saves, and returns correct data if successful" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1?include=children,otherChildren"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [
                            {| ``type`` = "child2"; id = "c2" |}
                            {| ``type`` = "child1"; id = "c1" |}
                          ]
                        |}
                      otherChildren =
                        {| data = [
                            {| ``type`` = "child1"; id = "c1" |}
                            {| ``type`` = "child2"; id = "c2" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "parent1" @>
      test <@ json |> getPath "data.id" = "p1" @>
      test <@ json |> getPath "data.relationships.children.data[0].type" = "child2" @>
      test <@ json |> getPath "data.relationships.children.data[0].id" = "c2" @>
      test <@ json |> getPath "data.relationships.children.data[1].type" = "child1" @>
      test <@ json |> getPath "data.relationships.children.data[1].id" = "c1" @>
      test <@ json |> getPath "data.relationships.otherChildren.data[0].type" = "child1" @>
      test <@ json |> getPath "data.relationships.otherChildren.data[0].id" = "c1" @>
      test <@ json |> getPath "data.relationships.otherChildren.data[1].type" = "child2" @>
      test <@ json |> getPath "data.relationships.otherChildren.data[1].id" = "c2" @>

      let p = 
        match db.TryGetParent "p1" with
        | Some (P1 p) -> p
        | _ -> failwith "not found"
      test <@ p.Id = "p1" @>
      test <@ p.Children = [C2 { Id = "c2" }; C1 { Id = "c1"; Child = { Id = "c3" } }] @>
      test <@ p.OtherChildIds = ["c1"; "c2"] @>
    }

    testJob "Set-only: Returns 200, saves, and returns correct data if successful" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p2?include=children"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent2"
                  id = "p2"
                  relationships =
                    {|children =
                        {| data = [
                            {| ``type`` = "child1"; id = "c1" |}
                            {| ``type`` = "child2"; id = "c2" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "parent2" @>
      test <@ json |> getPath "data.id" = "p2" @>
      test <@ json |> hasNoPath "data.relationships.children" @>

      let p = 
        match db.TryGetParent "p2" with
        | Some (P2 p) -> p
        | _ -> failwith "not found"
      test <@ p.Id = "p2" @>
      test <@ p.Children = [C1 { Id = "c1"; Child = { Id = "c3" } }; C2 { Id = "c2" }] @>
    }

    testJob "Related setter returns errors returned by related lookup's getById for each lookup" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with LookupChild = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [
                            {| ``type`` = "child1"; id = "c1" |}
                            {| ``type`` = "child2"; id = "c2" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0" @>
      test <@ json |> getPath "errors[1].status" = "422" @>
      test <@ json |> getPath "errors[1].code" = "custom" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Related setter returns errors returned by setter" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with SetChildren1 = fun _ _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [
                            {| ``type`` = "child1"; id = "c1" |}
                            {| ``type`` = "child2"; id = "c2" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Related setter returns 404 for each related resource that is not found" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with LookupChild = fun _ -> Ok None }
      let! response =
        Request.patch ctx "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [
                            {| ``type`` = "child2"; id = "c2" |}
                            {| ``type`` = "child1"; id = "c1" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0" @>
      test <@ json |> getPath "errors[1].status" = "404" @>
      test <@ json |> getPath "errors[1].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Related setter returns 404 for each related ID that fails to parse" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ParseChild2Id = fun _ -> Error [Error.create 422] }
      let! response =
        Request.patch ctx "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [
                            {| ``type`` = "child2"; id = "c2" |}
                            {| ``type`` = "child2"; id = "c22" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0" @>
      test <@ json |> getPath "errors[1].status" = "404" @>
      test <@ json |> getPath "errors[1].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "ID setter returns errors returned by setter" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with SetOtherChildrenIds1 = fun _ _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|otherChildren =
                        {| data = [] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/otherChildren/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "ID setter returns 404 for each related ID that fails to parse" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ParseChild2Id = fun _ -> Error [Error.create 422] }
      let! response =
        Request.patch ctx "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|otherChildren =
                        {| data = [
                            {| ``type`` = "child2"; id = "c2" |}
                            {| ``type`` = "child2"; id = "c22" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/otherChildren/data/0" @>
      test <@ json |> getPath "errors[1].status" = "404" @>
      test <@ json |> getPath "errors[1].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/otherChildren/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 403 if relationship is not settable" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent3"
                  id = "p3"
                  relationships =
                    {|children =
                        {| data = [] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is read-only" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable but supports POST" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent3"
                  id = "p3"
                  relationships =
                    {|supportsPost =
                        {| data = [] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsPost' on type 'parent3' does not support complete replacement using PATCH; it supports POST to add items" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/supportsPost" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable but supports DELETE" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent3"
                  id = "p3"
                  relationships =
                    {|supportsDelete =
                        {| data = [] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsDelete' on type 'parent3' does not support complete replacement using PATCH; it supports DELETE to remove items" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/supportsDelete" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable but supports POST/DELETE" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent3"
                  id = "p3"
                  relationships =
                    {|supportsPostAndDelete =
                        {| data = [] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsPostAndDelete' on type 'parent3' does not support complete replacement using PATCH; it supports POST/DELETE to add/remove items" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/supportsPostAndDelete" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 if relationship is missing data" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships = {| children = obj() |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' was specified without relationship data" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 409 for each type that is not allowed" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [
                            {| ``type`` = "invalid1"; id = "foo1" |}
                            {| ``type`` = "invalid2"; id = "foo2" |}
                          ]
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 409
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "409" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' contains data with invalid type 'invalid1'; expected one of 'child1', 'child2'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0/type" @>
      test <@ json |> getPath "errors[1].status" = "409" @>
      test <@ json |> getPath "errors[1].detail" = "Relationship 'children' contains data with invalid type 'invalid2'; expected one of 'child1', 'child2'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1/type" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 when relationship has null data" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = null |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'data' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 for each relationship data identifier that has missing type" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [ {| id = "foo" |}; {| id = "bar" |} ] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Missing required member 'type'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Missing required member 'type'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 for each relationship data identifier that has null type" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [{| ``type`` = null; id = "foo" |}; {| ``type`` = null; id = "bar" |}] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'type' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0/type" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Member 'type' may not be null" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1/type" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 for each relationship data identifier that has missing ID" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [ {| ``type`` = "child2" |}; {| ``type`` = "child2" |}] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Missing required member 'id'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Missing required member 'id'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 for each relationship data identifier that has null ID" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "parent1"
                  id = "p1"
                  relationships =
                    {|children =
                        {| data = [ {| ``type`` = "child2"; id = null |}; {| ``type`` = "child2"; id = null |}] |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'id' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/children/data/0/id" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Member 'id' may not be null" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/relationships/children/data/1/id" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

  ]


[<Tests>]
let tests2 =
  testList "To-many relationship PATCH self" [

    testJob "Parent1.children: Returns 200, modifies response, saves, and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyPatchSelfOkResponse = fun _ _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {|data = [
                {|``type`` = "child2"; id = "c2" |}
                {|``type`` = "child1"; id = "c1" |}
            ] |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "child2" @>
      test <@ json |> getPath "data[0].id" = "c2" @>
      test <@ json |> hasNoPath "data[0].attributes" @>
      test <@ json |> hasNoPath "data[0].relationships" @>
      test <@ json |> hasNoPath "data[0].links" @>
      test <@ json |> getPath "data[1].type" = "child1" @>
      test <@ json |> getPath "data[1].id" = "c1" @>
      test <@ json |> hasNoPath "data[1].attributes" @>
      test <@ json |> hasNoPath "data[1].relationships" @>
      test <@ json |> hasNoPath "data[1].links" @>
      test <@ json |> hasNoPath "included" @>

      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>

      let p = 
        match db.TryGetParent "p1" with
        | Some (P1 p) -> p
        | _ -> failwith "not found"
      test <@ p.Id = "p1" @>
      test <@ p.Children = [C2 { Id = "c2" }; C1 { Id = "c1"; Child = { Id = "c3" } }] @>
    }

    testJob "Supports include parameter and ignores include paths not starting with relationship name" {
      let db = Db ()
      let ctx = Ctx.WithDb db
      let! response =
        Request.patch ctx "/parents/p1/relationships/children?include=children.subChild,otherChildren"
        |> Request.bodySerialized
            {|data = [
              {|``type`` = "child2"; id = "c2" |}
              {|``type`` = "child1"; id = "c1" |}
            ] |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "child2" @>
      test <@ json |> getPath "data[0].id" = "c2" @>
      test <@ json |> hasNoPath "data[0].attributes" @>
      test <@ json |> hasNoPath "data[0].relationships" @>
      test <@ json |> hasNoPath "data[0].links" @>
      test <@ json |> getPath "data[1].type" = "child1" @>
      test <@ json |> getPath "data[1].id" = "c1" @>
      test <@ json |> hasNoPath "data[1].attributes" @>
      test <@ json |> hasNoPath "data[1].relationships" @>
      test <@ json |> hasNoPath "data[1].links" @>
      test <@ json |> getPath "included.[0].type" = "child1" @>
      test <@ json |> getPath "included.[0].id" = "c1" @>
      test <@ json |> getPath "included.[0].relationships.subChild.data.type" = "child3" @>
      test <@ json |> getPath "included.[0].relationships.subChild.data.id" = "c3" @>
      test <@ json |> getPath "included.[1].type" = "child2" @>
      test <@ json |> getPath "included.[1].id" = "c2" @>
      test <@ json |> getPath "included.[2].type" = "child3" @>
      test <@ json |> getPath "included.[2].id" = "c3" @>
      test <@ json |> getPath "included.[2].attributes.c" = "abc" @>
      test <@ json |> hasNoPath "included.[3]" @>
    }

    testJob "Parent1.otherChildren: Returns 202, modifies response, saves, and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyPatchSelfAcceptedResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/parents/p1/relationships/otherChildren"
        |> Request.bodySerialized
            {|data = [
                {|``type`` = "child1"; id = "c1" |}
                {|``type`` = "child2"; id = "c2" |}
            ] |}
        |> getResponse

      response |> testStatusCode 202
      let! json = response |> Response.readBodyAsString
      test <@ json = "" @>

      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>

      let p = 
        match db.TryGetParent "p1" with
        | Some (P1 p) -> p
        | _ -> failwith "not found"
      test <@ p.Id = "p1" @>
      test <@ p.OtherChildIds = ["c1"; "c2"] @>
    }

    testJob "Calls AfterModifySelf with the initial and new entity" {
      let db = Db ()
      let pOrig =
        match db.TryGetParent "p1" with
        | Some (P1 p) -> p
        | _ -> failwith "not found"

      let pExpected = { Id = "p1"; Children = [C2 { Id = "c2" }; C1 { Id = "c1"; Child = { Id = "c3" } }]; OtherChildIds = ["c2"; "c1"] }

      let mutable called = false
      let afterUpdate before after =
        called <- true
        test <@ before = pOrig @>
        test <@ after = pExpected @>

      let ctx = { Ctx.WithDb db with AfterUpdate1 = afterUpdate }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {|data = [
              {|``type`` = "child2"; id = "c2" |}
              {|``type`` = "child1"; id = "c1" |}
            ] |}
        |> getResponse

      response |> testSuccessStatusCode

      let called' = called
      test <@ called' = true @>

    }

    testJob "Correctly handles precondition validation using ETag" {
      let! response =
        Request.patch Ctx4 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx4 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> Request.setHeader (IfMatch "\"invalid-etag\"")
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey ETag = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Match header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx4 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> getResponse
      test <@ response.headers.[ETag] <> "\"valid-etag\"" @>
      response |> testStatusCode 200
    }

    testJob "Correctly handles precondition validation using If-Unmodified-Since" {
      let! response =
        Request.patch Ctx5 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx5 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx5 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "Correctly handles optional precondition validation" {
      let! response =
        Request.patch Ctx6 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> getResponse
      response |> testStatusCode 200

      let! response =
        Request.patch Ctx6 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx6 "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "Returns errors and does not call AfterModifySelf when BeforeModifySelf returns errors" {
      let db = Db ()
      let ctx = {
        Ctx.WithDb db with
          BeforeModifySelf1 = fun _ -> Error [Error.create 422 |> Error.setCode "custom"]
          AfterUpdate1 = fun _ _ -> failwith "should not be called"
      }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = [||] |}
        |> getResponse

      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if the relationship has no getter" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p2/relationships/children"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' on type 'parent2' is write-only and may only be updated through PATCH requests to the parent resource (other resource types in collection 'parents' may have a relationship called 'children' that supports this operation)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Related setter returns errors returned by related lookup's getById for each lookup" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with LookupChild = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {|data = [
                {| ``type`` = "child1"; id = "c1" |}
                {| ``type`` = "child2"; id = "c2" |}
            ] |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0" @>
      test <@ json |> getPath "errors[1].status" = "422" @>
      test <@ json |> getPath "errors[1].code" = "custom" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Related setter returns errors returned by setter" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with SetChildren1 = fun _ _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [
                {| ``type`` = "child1"; id = "c1" |}
                {| ``type`` = "child2"; id = "c2" |}
            ] |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Related setter returns 404 for each related resource that is not found" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with LookupChild = fun _ -> Ok None }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [
                {| ``type`` = "child1"; id = "c1" |}
                {| ``type`` = "child2"; id = "c2" |}
            ] |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0" @>
      test <@ json |> getPath "errors[1].status" = "404" @>
      test <@ json |> getPath "errors[1].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Related setter returns 404 for each related ID that fails to parse" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ParseChild2Id = fun _ -> Error [Error.create 422] }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [
                {| ``type`` = "child2"; id = "c2" |}
                {| ``type`` = "child2"; id = "c22" |}
            ] |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0" @>
      test <@ json |> getPath "errors[1].status" = "404" @>
      test <@ json |> getPath "errors[1].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "ID setter returns errors returned by setter" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with SetOtherChildrenIds1 = fun _ _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/parents/p1/relationships/otherChildren"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "ID setter returns 404 for each related ID that fails to parse" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ParseChild2Id = fun _ -> Error [Error.create 422] }
      let! response =
        Request.patch ctx "/parents/p1/relationships/otherChildren"
        |> Request.bodySerialized
            {| data = [
                {| ``type`` = "child2"; id = "c2" |}
                {| ``type`` = "child2"; id = "c22" |}
            ] |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0" @>
      test <@ json |> getPath "errors[1].status" = "404" @>
      test <@ json |> getPath "errors[1].detail" = "The related resource does not exist" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 403 if relationship is not settable" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3/relationships/children"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' on type 'parent3' is read-only" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable but supports POST" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3/relationships/supportsPost"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsPost' on type 'parent3' does not support complete replacement using PATCH; it supports POST to add items (other resource types in collection 'parents' may have a relationship called 'supportsPost' that supports PATCH)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable but supports DELETE" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3/relationships/supportsDelete"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsDelete' on type 'parent3' does not support complete replacement using PATCH; it supports DELETE to remove items (other resource types in collection 'parents' may have a relationship called 'supportsDelete' that supports PATCH)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable but supports POST/DELETE" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3/relationships/supportsPostAndDelete"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsPostAndDelete' on type 'parent3' does not support complete replacement using PATCH; it supports POST/DELETE to add/remove items (other resource types in collection 'parents' may have a relationship called 'supportsPostAndDelete' that supports PATCH)" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable for any resource but supports POST" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3/relationships/supportsPostUnique"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsPostUnique' does not support PATCH for any resource in collection 'parents'; it may support POST to add items" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable for any resource but supports DELETE" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3/relationships/supportsDeleteUnique"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsDeleteUnique' does not support PATCH for any resource in collection 'parents'; it may support DELETE to remove items" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship is not settable for any resource but supports POST/DELETE" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p3/relationships/supportsPostAndDeleteUnique"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'supportsPostAndDeleteUnique' does not support PATCH for any resource in collection 'parents'; it may support POST/DELETE to add/remove items" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when missing body" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "The request must specify the primary data to update" @>
      test <@ json |> getPath "errors[0].source.pointer" = "" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when JSON is invalid" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
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

    testJob "Returns 400 if missing data" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.bodySerialized (obj())
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Missing required member 'data'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 if data is null" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.bodySerialized {| data = null |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'data' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 for each missing type" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [ {| id = "p1" |}; {| id = "p2" |}] |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Missing required member 'type'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Missing required member 'type'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 for each null type" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [ {| ``type`` = null; id = "c2" |}; {| ``type`` = null; id = "c2" |} ] |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'type' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0/type" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Member 'type' may not be null" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1/type" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 for each missing ID" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [ {| ``type`` = "child2" |}; {| ``type`` = "child2" |} ] |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Missing required member 'id'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Missing required member 'id'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 400 for each null ID" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [ {| ``type`` = "child2"; id = null |}; {| ``type`` = "child2"; id = null |} ] |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'id' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0/id" @>
      test <@ json |> getPath "errors[1].status" = "400" @>
      test <@ json |> getPath "errors[1].detail" = "Member 'id' may not be null" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1/id" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Returns 409 for each type that is not allowed" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [
                {| ``type`` = "invalid1"; id = "foo1" |}
                {| ``type`` = "invalid2"; id = "foo2" |}
            ] |}
        |> getResponse
      response |> testStatusCode 409
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "409" @>
      test <@ json |> getPath "errors[0].detail" = "Data contains invalid type 'invalid1'; expected one of 'child1', 'child2'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/0/type" @>
      test <@ json |> getPath "errors[1].status" = "409" @>
      test <@ json |> getPath "errors[1].detail" = "Data contains invalid type 'invalid2'; expected one of 'child1', 'child2'" @>
      test <@ json |> getPath "errors[1].source.pointer" = "/data/1/type" @>
      test <@ json |> hasNoPath "errors[2]" @>
    }

    testJob "Saves and returns 500 if Skip after update" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with GetParent1Children = fun _ -> Skip }
      let! response =
        Request.patch ctx "/parents/p1/relationships/children"
        |> Request.bodySerialized
            {| data = [
                {| ``type`` = "child2"; id = "c2" |}
                {| ``type`` = "child1"; id = "c1" |}
            ] |}
        |> getResponse

      response |> testStatusCode 500
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "500" @>
      test <@ json |> getPath "errors[0].detail" = "The relationship was updated, but the server has erroneously chosen not to disclose the value of the updated relationship" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let p = 
        match db.TryGetParent "p1" with
        | Some (P1 p) -> p
        | _ -> failwith "not found"
      test <@ p.Id = "p1" @>
      test <@ p.Children = [C2 { Id = "c2" }; C1 { Id = "c1"; Child = { Id = "c3" } }] @>
    }

    testJob "Returns 404 if relationship does not exist for resource" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/parents/p4/relationships/children"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is not defined for resource type 'parent4' (other resource types in collection 'parents' may have a relationship called 'children')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship exists but is not settable for any resource" {
      let! response =
        Request.patch Ctx2 "/parents/p5/relationships/children"
        |> Request.bodySerialized {| data = [] |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' does not support PATCH for any resource in collection 'parents'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 if resource does not exist" {
      let db = Db ()
      let! response = Request.patch (Ctx.WithDb db) "/parents/invalidId/relationships/children" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'parents' does not contain a resource with ID 'invalidId'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if missing lookup" {
      let! response = Request.patch Ctx3 "/parents/ignoredId/relationships/children" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'parents' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
