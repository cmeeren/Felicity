module ``To-many relationship GET related and self``

open System.Text.Json.Serialization
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
  Child2: Child2
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
    |> Map.add "p1" (P1 { Id = "p1"; Children = [C1 { Id = "c1"; Child = { Id = "c3" } }; C2 { Id = "c2" }]; Child2 = { Id = "c22" } })
    |> Map.add "p2" (P2 { Id = "p2"; Children = [C2 { Id = "c2" }; C1 { Id = "c1"; Child = { Id = "c3" } }] })
    |> Map.add "p3" (P3 { Id = "p3" })
    |> Map.add "p4" (P4 { Id = "p4" })

  member _.TryGet id =
    parents.TryFind id


type Ctx = {
  Db: Db
  ModifyGetRelatedResponse1: Parent1 -> Child list -> HttpHandler
  ModifyGetSelfResponse1: Parent1 -> Child list -> HttpHandler
  ModifyGetRelatedResponse2: Parent2 -> Child list -> HttpHandler
  ModifyGetSelfResponse2: Parent2 -> Child list -> HttpHandler
  GetParent1Children: Parent1 -> Child list Skippable
} with
  static member WithDb db = {
    Db = db
    ModifyGetRelatedResponse1 = fun _ _ -> fun next ctx -> next ctx
    ModifyGetSelfResponse1 = fun _ _ -> fun next ctx -> next ctx
    ModifyGetRelatedResponse2 = fun _ _ -> fun next ctx -> next ctx
    ModifyGetSelfResponse2 = fun _ _ -> fun next ctx -> next ctx
    GetParent1Children = fun p -> Include p.Children
  }


module Child3 =

  let define = Define<Ctx, Child3, string>()
  let resId = define.Id.Simple(fun (c: Child3) -> c.Id)
  let resDef = define.Resource("child3", resId)
  let c = define.Attribute.Simple().Get(fun _ -> "abc")


module Child1 =

  let define = Define<Ctx, Child1, string>()
  let resId = define.Id.Simple(fun (c: Child1) -> c.Id)
  let resDef = define.Resource("child1", resId)
  let a = define.Attribute.Simple().Get(fun _ -> 2)
  let subChild =
    define.Relationship
      .ToOne(Child3.resDef)
      .Get(fun c -> c.Child)


module Child2 =

  let define = Define<Ctx, Child2, string>()
  let resId = define.Id.Simple(fun (c: Child2) -> c.Id)
  let resDef = define.Resource("child2", resId)
  let b = define.Attribute.Simple().Get(fun _ -> true)


module Parent1 =

  let define = Define<Ctx, Parent1, string>()
  let resId = define.Id.Simple(fun (p: Parent1) -> p.Id)
  let resDef = define.Resource("parent1", resId).CollectionName("parents")

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
      .ModifyGetRelatedResponse(fun ctx -> ctx.ModifyGetRelatedResponse1)
      .ModifyGetSelfResponse(fun ctx -> ctx.ModifyGetSelfResponse1)

  let child2 =
    define.Relationship
      .ToOne(Child2.resDef)
      .Get(fun p -> p.Child2)


module Parent2 =

  let define = Define<Ctx, Parent2, string>()
  let resId = define.Id.Simple(fun (p: Parent2) -> p.Id)
  let resDef = define.Resource("parent2", resId).CollectionName("parents")

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
      .Get(fun p -> p.Children)
      .ModifyGetRelatedResponse(fun ctx -> ctx.ModifyGetRelatedResponse2)
      .ModifyGetSelfResponse(fun ctx -> ctx.ModifyGetSelfResponse2)


module Parent3 =

  let define = Define<Ctx, Parent3, string>()
  let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
  let resDef = define.Resource("parent3", resId).CollectionName("parents")

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


module Parent4 =

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
        (fun ctx id -> ctx.Db.TryGet id),
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

  let children : ToManyRelationship<Ctx2, Parent4, obj, string> =
    define.Relationship
      .Polymorphic()
      .ToMany()


type Ctx3 = Ctx3

module Parent6 =

  let define = Define<Ctx3, Parent1, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("p6", resId).CollectionName("parents")


[<Tests>]
let tests1 =
  testList "To-many relationship GET related" [

    testJob "Parent1: Returns 200, modifies response and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyGetRelatedResponse1 = fun _ _ -> setHttpHeader "Foo" "Bar" }
      let! response = Request.get ctx "/parents/p1/children" |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "child1" @>
      test <@ json |> getPath "data[0].id" = "c1" @>
      test <@ json |> getPath "data[0].attributes.a" = 2 @>
      test <@ json |> getPath "data[1].type" = "child2" @>
      test <@ json |> getPath "data[1].id" = "c2" @>
      test <@ json |> getPath "data[1].attributes.b" = true @>

      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>
    }

    testJob "Correctly handles ETag and If-None-Match" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/p1/children" |> getResponse
      response |> testStatusCode 200
      let eTag = response.headers.[ETag]

      let! response =
        Request.get (Ctx.WithDb db) "/parents/p1/children"
        |> Request.setHeader (IfNoneMatch eTag)
        |> getResponse
      response |> testStatusCode 304
    }

    testJob "Parent2: Returns 200, modifies response and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyGetRelatedResponse2 = fun _ _ -> setHttpHeader "Foo" "Bar" }
      let! response = Request.get ctx "/parents/p2/children" |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "child2" @>
      test <@ json |> getPath "data[0].id" = "c2" @>
      test <@ json |> getPath "data[0].attributes.b" = true @>
      test <@ json |> getPath "data[1].type" = "child1" @>
      test <@ json |> getPath "data[1].id" = "c1" @>
      test <@ json |> getPath "data[1].attributes.a" = 2 @>

      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>
    }

    testJob "Returns 403 if Skip" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with GetParent1Children = fun _ -> Skip }
      let! response = Request.get ctx "/parents/p1/children" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "The server has chosen not to disclose the value of this relationship" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship exists but is not gettable for resource" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/p3/children" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' on type 'parent3' is not readable (other resource types in collection 'parents' may have a readable relationship called 'children')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 if relationship does not exist for resource" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/p4/children" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is not defined for resource type 'parent4' (other resource types in collection 'parents' may have a relationship called 'children')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship exists but is not gettable for any resource" {
      let! response = Request.get Ctx2 "/parents/p3/children" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is not readable for any resource in collection 'parents'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 if resource does not exist" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/invalidId/children" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'parents' does not contain a resource with ID 'invalidId'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if missing lookup" {
      let! response = Request.get Ctx3 "/parents/ignoredId/children" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'parents' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]


[<Tests>]
let tests2 =
  testList "To-many relationship GET self" [

    testJob "Parent1: Returns 200, modifies response and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyGetSelfResponse1 = fun _ _ -> setHttpHeader "Foo" "Bar" }
      let! response = Request.get ctx "/parents/p1/relationships/children" |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "child1" @>
      test <@ json |> getPath "data[0].id" = "c1" @>
      test <@ json |> hasNoPath "data[0].attributes" @>
      test <@ json |> hasNoPath "data[0].relationships" @>
      test <@ json |> hasNoPath "data[0].links" @>
      test <@ json |> getPath "data[1].type" = "child2" @>
      test <@ json |> getPath "data[1].id" = "c2" @>
      test <@ json |> hasNoPath "data[1].attributes" @>
      test <@ json |> hasNoPath "data[1].relationships" @>
      test <@ json |> hasNoPath "data[1].links" @>
      test <@ json |> hasNoPath "included" @>

      test <@ response.headers.[NonStandard "Foo"] = "Bar" @>
    }

    testJob "Supports include parameter and ignores include paths not starting with relationship name" {
      let db = Db ()
      let ctx = Ctx.WithDb db
      let! response = Request.get ctx "/parents/p1/relationships/children?include=children.subChild,child2" |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].type" = "child1" @>
      test <@ json |> getPath "data[0].id" = "c1" @>
      test <@ json |> hasNoPath "data[0].attributes" @>
      test <@ json |> hasNoPath "data[0].relationships" @>
      test <@ json |> hasNoPath "data[0].links" @>
      test <@ json |> getPath "data[1].type" = "child2" @>
      test <@ json |> getPath "data[1].id" = "c2" @>
      test <@ json |> hasNoPath "data[1].attributes" @>
      test <@ json |> hasNoPath "data[1].relationships" @>
      test <@ json |> hasNoPath "data[1].links" @>
      test <@ json |> getPath "included.[0].type" = "child1" @>
      test <@ json |> getPath "included.[0].id" = "c1" @>
      test <@ json |> getPath "included.[0].attributes.a" = 2 @>
      test <@ json |> getPath "included.[0].relationships.subChild.data.type" = "child3" @>
      test <@ json |> getPath "included.[0].relationships.subChild.data.id" = "c3" @>
      test <@ json |> getPath "included.[1].type" = "child2" @>
      test <@ json |> getPath "included.[1].id" = "c2" @>
      test <@ json |> getPath "included.[1].attributes.b" = true @>
      test <@ json |> getPath "included.[2].type" = "child3" @>
      test <@ json |> getPath "included.[2].id" = "c3" @>
      test <@ json |> getPath "included.[2].attributes.c" = "abc" @>
      test <@ json |> hasNoPath "included.[3]" @>
    }

    testJob "Correctly handles ETag and If-None-Match" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/p1/relationships/children" |> getResponse
      response |> testStatusCode 200
      let eTag = response.headers.[ETag]

      let! response =
        Request.get (Ctx.WithDb db) "/parents/p1/relationships/children"
        |> Request.setHeader (IfNoneMatch eTag)
        |> getResponse
      response |> testStatusCode 304
    }

    testJob "Parent2: Returns 200, modifies response and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyGetSelfResponse2 = fun _ _ -> setHttpHeader "Foo" "Bar" }
      let! response = Request.get ctx "/parents/p2/relationships/children" |> getResponse
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
    }

    testJob "Returns 403 if Skip" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with GetParent1Children = fun _ -> Skip }
      let! response = Request.get ctx "/parents/p1/relationships/children" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "The server has chosen not to disclose the value of this relationship" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship exists but is not gettable for resource" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/p3/relationships/children" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' on type 'parent3' is not readable (other resource types in collection 'parents' may have a readable relationship called 'children')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 if relationship does not exist for resource" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/p4/relationships/children" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is not defined for resource type 'parent4' (other resource types in collection 'parents' may have a relationship called 'children')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if relationship exists but is not gettable for any resource" {
      let! response = Request.get Ctx2 "/parents/p3/relationships/children" |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is not readable for any resource in collection 'parents'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 if resource does not exist" {
      let db = Db ()
      let! response = Request.get (Ctx.WithDb db) "/parents/invalidId/relationships/children" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'parents' does not contain a resource with ID 'invalidId'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if missing lookup" {
      let! response = Request.get Ctx3 "/parents/ignoredId/relationships/children" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'parents' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
