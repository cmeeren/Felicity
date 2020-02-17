module ``Resource links``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx = Ctx

type A = A
type B = B
type C = C
type X = X


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("a", resId)
  // Include relationships to ensure that we can generate relationships correctly for
  // related resources without collection names
  let toOne = define.Relationship.ToOne(resDef).Get(fun _ _ -> A)
  let toOneNullable = define.Relationship.ToOneNullable(resDef).Get(fun _ _ -> Some A)
  let toMany = define.Relationship.ToMany(resDef).Get(fun _ _ -> [A])


module B =

  let define = Define<Ctx, B, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("b", resId).CollectionName("bs")


module C =

  let define = Define<Ctx, C, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("c", resId).CollectionName("cs")
  let lookup = define.Operation.Lookup(fun _ _ -> failwith "not used")
  let get = define.Operation.GetResource()


module X =

  let define = Define<Ctx, X, string>()
  let resId = define.Id.Simple(fun _ -> "someId")
  let resDef = define.Resource("x", resId).CollectionName("xs")
  let a = define.Relationship.ToOne(A.resDef).Get(fun _ _ -> A)
  let b = define.Relationship.ToOne(B.resDef).Get(fun _ _ -> B)
  let c = define.Relationship.ToOne(C.resDef).Get(fun _ _ -> C)
  let getColl = define.Operation.GetCollection(fun () -> [X])
  let lookup = define.Operation.Lookup(fun _ -> Some X)
  let get = define.Operation.GetResource()

  let relToOneNoGetSet = define.Relationship.ToOne(A.resDef)
  let relToOneGet = define.Relationship.ToOne(A.resDef).Get(fun _ _ -> A)
  let relToOneSet = define.Relationship.ToOne(A.resDef).Set(fun _ _ _ -> failwith "not used")
  let relToOneGetSet = define.Relationship.ToOne(A.resDef).Get(fun _ _ -> failwith "not used").Set(fun _ _ _ -> failwith "not used")

  let relToOneNullableNoGetSet = define.Relationship.ToOneNullable(A.resDef)
  let relToOneNullableGet = define.Relationship.ToOneNullable(A.resDef).Get(fun _ _ -> failwith "not used")
  let relToOneNullableSet = define.Relationship.ToOneNullable(A.resDef).Set(fun _ _ _ -> failwith "not used")
  let relToOneNullableGetSet = define.Relationship.ToOneNullable(A.resDef).Get(fun _ _ -> failwith "not used").Set(fun _ _ _ -> failwith "not used")

  let relToManyNoGetSetAddRemove = define.Relationship.ToMany(A.resDef)
  let relToManyGet = define.Relationship.ToMany(A.resDef).Get(fun _ _ -> failwith "not used")
  let relToManySet = define.Relationship.ToMany(A.resDef).SetAll(fun _ _ _ -> failwith "not used")
  let relToManyGetSet = define.Relationship.ToMany(A.resDef).Get(fun _ _ -> failwith "not used").SetAll(fun _ _ _ -> failwith "not used")
  let relToManyGetAdd = define.Relationship.ToMany(A.resDef).Get(fun _ _ -> failwith "not used").Add(fun _ _ _ -> failwith "not used")
  let relToManyGetRemove = define.Relationship.ToMany(A.resDef).Get(fun _ _ -> failwith "not used").Remove(fun _ _ _ -> failwith "not used")




[<Tests>]
let tests =
  testList "Resource links" [

    testJob "Resource self link should not be present if type has no collection name" {
      let! response = Request.get Ctx "/xs?include=a" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "included[0].type" = "a" @>
      test <@ json |> hasNoPath "included[0].links" @>
    }

    testJob "Resource self link should not be present if type has collection name but no GET operation" {
      let! response = Request.get Ctx "/xs?include=b" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "included[0].type" = "b" @>
      test <@ json |> hasNoPath "included[0].links" @>
    }

    testJob "Resource self link should be present if type has collection name and GET operation" {
      let! response = Request.get Ctx "/xs?include=c" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data[0].links.self" = "http://example.com/xs/someId" @>
      test <@ json |> getPath "included[0].type" = "c" @>
      test <@ json |> getPath "included[0].links.self" = "http://example.com/cs/someId" @>
    }

    // Apart from requiring a collection name, relationship getter is both a necessary
    // and sufficient condition for relationship self/related links:
    // - Necessary, because any operation on related/self requires getter
    //   (PATCH/POST/DELETE self also requires getter for proper response)
    // - Sufficient, because as long as it has a getter, you should be able to retrieve
    //   the relationship data through the related/self links
    //
    // The collection name is a necessary additional condition because a relationship
    // getter may be defined without a collection name, meaning that the relationship
    // value may only be obtained through includes.

    testJob "To-one relationship self/related link should be present iff relationship has getter" {
      let! response = Request.get Ctx "/xs" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString

      test <@ json |> hasNoPath "data[0].relationships.relToOneNoGetSet.links" @>

      test <@ json |> getPath "data[0].relationships.relToOneGet.links.self" = "http://example.com/xs/someId/relationships/relToOneGet" @>
      test <@ json |> getPath "data[0].relationships.relToOneGet.links.related" = "http://example.com/xs/someId/relToOneGet" @>

      test <@ json |> hasNoPath "data[0].relationships.relToOneSet.links" @>

      test <@ json |> getPath "data[0].relationships.relToOneGetSet.links.self" = "http://example.com/xs/someId/relationships/relToOneGetSet" @>
      test <@ json |> getPath "data[0].relationships.relToOneGetSet.links.related" = "http://example.com/xs/someId/relToOneGetSet" @>
    }

    testJob "To-one nullable relationship self/related link should be present iff relationship has getter" {
      let! response = Request.get Ctx "/xs" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString

      test <@ json |> hasNoPath "data[0].relationships.relToOneNullableNoGetSet.links" @>

      test <@ json |> getPath "data[0].relationships.relToOneNullableGet.links.self" = "http://example.com/xs/someId/relationships/relToOneNullableGet" @>
      test <@ json |> getPath "data[0].relationships.relToOneNullableGet.links.related" = "http://example.com/xs/someId/relToOneNullableGet" @>

      test <@ json |> hasNoPath "data[0].relationships.relToOneNullableSet.links" @>

      test <@ json |> getPath "data[0].relationships.relToOneNullableGetSet.links.self" = "http://example.com/xs/someId/relationships/relToOneNullableGetSet" @>
      test <@ json |> getPath "data[0].relationships.relToOneNullableGetSet.links.related" = "http://example.com/xs/someId/relToOneNullableGetSet" @>
    }

    testJob "To-many relationship self/related link should be present iff relationship has getter" {
      let! response = Request.get Ctx "/xs" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> hasNoPath "data[0].relationships.relToManyNoGetSetAddRemove.links" @>

      test <@ json |> getPath "data[0].relationships.relToManyGet.links.self" = "http://example.com/xs/someId/relationships/relToManyGet" @>
      test <@ json |> getPath "data[0].relationships.relToManyGet.links.related" = "http://example.com/xs/someId/relToManyGet" @>

      test <@ json |> hasNoPath "data[0].relationships.relToManySet.links" @>

      test <@ json |> getPath "data[0].relationships.relToManyGetSet.links.self" = "http://example.com/xs/someId/relationships/relToManyGetSet" @>
      test <@ json |> getPath "data[0].relationships.relToManyGetSet.links.related" = "http://example.com/xs/someId/relToManyGetSet" @>

      test <@ json |> getPath "data[0].relationships.relToManyGetAdd.links.self" = "http://example.com/xs/someId/relationships/relToManyGetAdd" @>
      test <@ json |> getPath "data[0].relationships.relToManyGetAdd.links.related" = "http://example.com/xs/someId/relToManyGetAdd" @>

      test <@ json |> getPath "data[0].relationships.relToManyGetRemove.links.self" = "http://example.com/xs/someId/relationships/relToManyGetRemove" @>
      test <@ json |> getPath "data[0].relationships.relToManyGetRemove.links.related" = "http://example.com/xs/someId/relToManyGetRemove" @>
    }

    testJob "Relationships are not present if the resource does not have a collection name and the relationship is not included and does not have meta" {
      let! response = Request.get Ctx "/xs?include=a" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> hasNoPath "included[0].relationships.toOne" @>
      test <@ json |> hasNoPath "included[0].relationships.toOneNullable" @>
      test <@ json |> hasNoPath "included[0].relationships.toMany" @>
    }

    testJob "Relationships are present with data and not links if the resource does not have a collection name and the relationship is included" {
      let! response = Request.get Ctx "/xs?include=a.toOne,a.toOneNullable,a.toMany" |> getResponse
      response |> testSuccessStatusCode
      let! json = response |> Response.readBodyAsString
      test <@ json |> hasPath "included[0].relationships.toOne.data" @>
      test <@ json |> hasNoPath "included[0].relationships.toOne.links" @>
      test <@ json |> hasNoPath "included[0].relationships.toOneNullable.links" @>
      test <@ json |> hasPath "included[0].relationships.toOneNullable.data" @>
      test <@ json |> hasNoPath "included[0].relationships.toMany.links" @>
      test <@ json |> hasPath "included[0].relationships.toMany.data" @>
    }

    // Links for custom operations are tested together with other custom operation tests

  ]
