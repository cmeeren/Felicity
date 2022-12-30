module ``Basic responses, sparse fieldsets and includes``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx = Ctx

type Person = {
    Id: string
    FirstName: string
    LastName: string
}

let alice = {
    Id = "1"
    FirstName = "Alice"
    LastName = "Doe"
}

let bob = {
    Id = "2"
    FirstName = "Bob"
    LastName = "Doe"
}

let jane = {
    Id = "3"
    FirstName = "Jane"
    LastName = "Doe"
}


module Person =

    let define = Define<Ctx, Person, string>()
    let resId = define.Id.Simple(fun p -> p.Id)
    let resDef = define.Resource("person", resId).CollectionName("persons")

    let firstName = define.Attribute.SimpleString().Get(fun p -> p.FirstName)

    let attrLastName =
        define.Attribute.SimpleString("lastName").Get(fun p -> p.LastName)

    let nonNullableRequiresExplicitInclude =
        define.Attribute.SimpleString().Get(fun _ -> "").RequireExplicitInclude()

    let nonNullableRequiresExplicitIncludeTrue =
        define.Attribute.SimpleString().Get(fun _ -> "").RequireExplicitInclude(true)

    let nonNullableRequiresExplicitIncludeFalse =
        define.Attribute.SimpleString().Get(fun _ -> "").RequireExplicitInclude(false)

    let nullableRequiresExplicitInclude =
        define
            .Attribute
            .Nullable
            .SimpleString()
            .Get(fun _ -> None)
            .RequireExplicitInclude()

    let nullableRequiresExplicitIncludeTrue =
        define
            .Attribute
            .Nullable
            .SimpleString()
            .Get(fun _ -> None)
            .RequireExplicitInclude(true)

    let nullableRequiresExplicitIncludeFalse =
        define
            .Attribute
            .Nullable
            .SimpleString()
            .Get(fun _ -> None)
            .RequireExplicitInclude(false)

    let friends = define.Relationship.ToMany(resDef).Get(fun _ -> [ bob; jane ])
    let bestFriend = define.Relationship.ToMany(resDef).Get(fun _ -> [ bob ])
    let emptyRel = define.Relationship.ToMany(resDef).Get(fun _ -> [])


    let toOneWithLinkage =
        define
            .Relationship
            .ToOne(resDef)
            .GetLinkageIfNotIncluded(fun _ _ -> bob.Id)
            .Get(fun _ -> jane)

    let toOneNullableWithLinkage =
        define
            .Relationship
            .ToOneNullable(resDef)
            .GetLinkageIfNotIncluded(fun _ _ -> Some bob.Id)
            .Get(fun _ -> Some jane)

    let toManyWithLinkage =
        define
            .Relationship
            .ToMany(resDef)
            .GetLinkageIfNotIncluded(fun _ _ -> [ bob.Id ])
            .Get(fun _ -> [ jane ])

    let toOneWithLinkageWithoutGet =
        define.Relationship.ToOne(resDef).GetLinkageIfNotIncluded(fun _ _ -> bob.Id)

    let toOneNullableWithLinkageWithoutGet =
        define
            .Relationship
            .ToOneNullable(resDef)
            .GetLinkageIfNotIncluded(fun _ _ -> Some bob.Id)

    let toManyWithLinkageWithoutGet =
        define
            .Relationship
            .ToMany(resDef)
            .GetLinkageIfNotIncluded(fun _ _ -> [ bob.Id ])

    let toOneSkipped =
        define
            .Relationship
            .ToOne(resDef)
            .SkipRelationshipIf(fun _ -> true)
            .Get(fun _ _ -> failwith "Not called")

    let toOneNullableSkipped =
        define
            .Relationship
            .ToOneNullable(resDef)
            .SkipRelationshipIf(fun _ -> true)
            .Get(fun _ _ -> failwith "Not called")

    let toManySkipped =
        define
            .Relationship
            .ToMany(resDef)
            .SkipRelationshipIf(fun _ -> true)
            .Get(fun _ _ -> failwith "Not called")

    let lookup = define.Operation.Lookup(fun _ -> Some alice)
    let get = define.Operation.GetResource()


[<Tests>]
let tests =
    testList "Basic responses, sparse fieldsets and includes" [

        testJob "Basic response" {
            let! response = Request.get Ctx "/persons/1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "person" @>
            test <@ json |> getPath "data.id" = "1" @>
            test <@ json |> getPath "data.attributes.firstName" = "Alice" @>
            test <@ json |> getPath "data.attributes.lastName" = "Doe" @>
            test <@ json |> getPath "data.links.self" = "http://example.com/persons/1" @>

            test
                <@
                    json |> getPath "data.relationships.friends.links.self" = "http://example.com/persons/1/relationships/friends"
                @>

            test
                <@ json |> getPath "data.relationships.friends.links.related" = "http://example.com/persons/1/friends" @>

            test <@ json |> hasNoPath "data.relationships.friends.data" @>
        }

        testJob "Sparse fieldsets" {
            let! response =
                Request.get Ctx "/persons/1"
                |> Request.queryStringItem "fields[person]" "firstName"
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.attributes.firstName" = "Alice" @>
            test <@ json |> hasNoPath "data.attributes.lastName" @>
            test <@ json |> hasNoPath "data.relationships.friends" @>
        }

        testJob "Includes" {
            let! response =
                Request.get Ctx "/persons/1"
                |> Request.queryStringItem "include" "friends,bestFriend"
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.relationships.friends.data[0].type" = "person" @>
            test <@ json |> getPath "data.relationships.friends.data[0].id" = "2" @>
            test <@ json |> getPath "data.relationships.friends.data[1].type" = "person" @>
            test <@ json |> getPath "data.relationships.friends.data[1].id" = "3" @>
            test <@ json |> hasNoPath "data.relationships.friends.data[2]" @>
            test <@ json |> getPath "included[0].type" = "person" @>
            test <@ json |> getPath "included[0].id" = "2" @>
            test <@ json |> getPath "included[0].attributes.firstName" = "Bob" @>
            test <@ json |> getPath "included[0].attributes.lastName" = "Doe" @>
            test <@ json |> getPath "included[0].links.self" = "http://example.com/persons/2" @>

            test
                <@
                    json |> getPath "included[0].relationships.friends.links.self" = "http://example.com/persons/2/relationships/friends"
                @>

            test
                <@
                    json |> getPath "included[0].relationships.friends.links.related" = "http://example.com/persons/2/friends"
                @>

            test <@ json |> hasNoPath "included[0].relationships.friends.data" @>
            test <@ json |> getPath "included[1].type" = "person" @>
            test <@ json |> getPath "included[1].id" = "3" @>
            test <@ json |> getPath "included[1].attributes.firstName" = "Jane" @>
            test <@ json |> getPath "included[1].attributes.lastName" = "Doe" @>
            test <@ json |> getPath "included[1].links.self" = "http://example.com/persons/3" @>

            test
                <@
                    json |> getPath "included[1].relationships.friends.links.self" = "http://example.com/persons/3/relationships/friends"
                @>

            test
                <@
                    json |> getPath "included[1].relationships.friends.links.related" = "http://example.com/persons/3/friends"
                @>

            test <@ json |> hasNoPath "included[1].relationships.friends.data" @>
            // Should be unique
            test <@ json |> hasNoPath "included[2]" @>
        }

        testJob "when ?include is used, included is present even if empty" {
            let! response =
                Request.get Ctx "/persons/1"
                |> Request.queryStringItem "include" "emptyRel"
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasPath "included" @>
            test <@ json |> hasNoPath "included[0]" @>
        }

        testJob "when ?include is not used, included is not present" {
            let! response = Request.get Ctx "/persons/1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasNoPath "included" @>
        }

        testJob "Linkage without ?include" {
            let! response = Request.get Ctx "/persons/1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.relationships.toOneWithLinkage.data.type" = "person" @>
            test <@ json |> getPath "data.relationships.toOneWithLinkage.data.id" = "2" @>
            test <@ json |> getPath "data.relationships.toOneNullableWithLinkage.data.type" = "person" @>
            test <@ json |> getPath "data.relationships.toOneNullableWithLinkage.data.id" = "2" @>
            test <@ json |> getPath "data.relationships.toManyWithLinkage.data[0].type" = "person" @>
            test <@ json |> getPath "data.relationships.toManyWithLinkage.data[0].id" = "2" @>
            test <@ json |> hasNoPath "data.relationships.toManyWithLinkage.data.1" @>
        }

        testJob "Linkage with ?include overrides linkage without ?include" {
            let! response =
                Request.get Ctx "/persons/1"
                |> Request.queryStringItem "include" "toOneWithLinkage,toOneNullableWithLinkage,toManyWithLinkage"
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.relationships.toOneWithLinkage.data.type" = "person" @>
            test <@ json |> getPath "data.relationships.toOneWithLinkage.data.id" = "3" @>
            test <@ json |> getPath "data.relationships.toOneNullableWithLinkage.data.type" = "person" @>
            test <@ json |> getPath "data.relationships.toOneNullableWithLinkage.data.id" = "3" @>
            test <@ json |> getPath "data.relationships.toManyWithLinkage.data[0].type" = "person" @>
            test <@ json |> getPath "data.relationships.toManyWithLinkage.data[0].id" = "3" @>
            test <@ json |> hasNoPath "data.relationships.toManyWithLinkage.data[1]" @>
            test <@ json |> getPath "included[0].id" = "3" @>
            test <@ json |> hasNoPath "included[1]" @>
        }

        testJob "No link when only linkage and no get" {
            let! response = Request.get Ctx "/persons/1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.relationships.toOneWithLinkageWithoutGet.data.type" = "person" @>
            test <@ json |> getPath "data.relationships.toOneWithLinkageWithoutGet.data.id" = "2" @>
            test <@ json |> hasNoPath "data.relationships.toOneWithLinkageWithoutGet.links" @>

            test
                <@
                    json
                    |> getPath "data.relationships.toOneNullableWithLinkageWithoutGet.data.type" = "person"
                @>

            test <@ json |> getPath "data.relationships.toOneNullableWithLinkageWithoutGet.data.id" = "2" @>
            test <@ json |> hasNoPath "data.relationships.toOneNullableWithLinkageWithoutGet.links" @>
            test <@ json |> getPath "data.relationships.toManyWithLinkageWithoutGet.data[0].type" = "person" @>
            test <@ json |> getPath "data.relationships.toManyWithLinkageWithoutGet.data[0].id" = "2" @>
            test <@ json |> hasNoPath "data.relationships.toManyWithLinkageWithoutGet.data.1" @>
            test <@ json |> hasNoPath "data.relationships.toManyWithLinkageWithoutGet.links" @>
        }

        (*

        TODO: Make the test below pass. Might need to test separately for each operation.
        Relevant part of spec:

            https://jsonapi.org/format/#fetching-includes

            If a server is unable to identify a relationship path or does not support inclusion
            of resources from a path, it MUST respond with 400 Bad Request.

        Notes:
        - Concern about unnecessary breaking changes: https://github.com/json-api/json-api/issues/1459
        - GET/POST collection and GET/POST/PATCH resource: Based on resource types in collection
        - GET/POST/PATCH/DELETE relationships: Based on resource types in relationship

        *)

        //testJob "Returns 400 for each invalid include path" {
        //  let! response =
        //    Request.get Ctx "/persons/1"
        //    |> Request.queryStringItem "include" "a,friends.foo,bestFriend.friends.bar"
        //    |> getResponse
        //  response |> testStatusCode 400
        //  let! json = response |> Response.readBodyAsString
        //  test <@ json |> getPath "errors[0].status" = "400" @>
        //  test <@ json |> getPath "errors[0].detail" = "Unknown relationship 'a' in include parameter" @>
        //  test <@ json |> getPath "errors[0].source.parameter" = "include" @>
        //  test <@ json |> getPath "errors[1].status" = "400" @>
        //  test <@ json |> getPath "errors[1].detail" = "Unknown relationship 'foo' after include path 'friends'" @>
        //  test <@ json |> getPath "errors[1].source.parameter" = "include" @>
        //  test <@ json |> getPath "errors[2].status" = "400" @>
        //  test <@ json |> getPath "errors[2].detail" = "Unknown relationship 'bar' after include path 'bestFriend.friends'" @>
        //  test <@ json |> getPath "errors[2].source.parameter" = "include" @>
        //  test <@ json |> hasNoPath "included[3]" @>
        //}

        testJob "Sparse fieldsets and includes" {
            let! response =
                Request.get Ctx "/persons/1"
                |> Request.queryStringItem "fields[person]" "firstName"
                |> Request.queryStringItem "include" "friends"
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.attributes.firstName" = "Alice" @>
            test <@ json |> hasNoPath "data.attributes.lastName" @>
            test <@ json |> hasNoPath "data.relationships.friends" @>
            test <@ json |> getPath "included[0].type" = "person" @>
            test <@ json |> getPath "included[0].id" = "2" @>
            test <@ json |> getPath "included[0].attributes.firstName" = "Bob" @>
            test <@ json |> hasNoPath "included[0].attributes.lastName" @>
            test <@ json |> hasNoPath "included[0].relationships.friends" @>
            test <@ json |> getPath "included[1].type" = "person" @>
            test <@ json |> getPath "included[1].id" = "3" @>
            test <@ json |> getPath "included[1].attributes.firstName" = "Jane" @>
            test <@ json |> hasNoPath "included[1].attributes.lastName" @>
            test <@ json |> hasNoPath "included[1].relationships.friends" @>
            test <@ json |> hasNoPath "included[2]" @>
        }

        testJob "Skipped relationships are omitted" {
            let! response = Request.get Ctx "/persons/1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasNoPath "data.relationships.toOneSkipped" @>
            test <@ json |> hasNoPath "data.relationships.toOneNullableSkipped" @>
            test <@ json |> hasNoPath "data.relationships.toManySkipped" @>
        }

        testJob "Attributes with RequireExplicitInclude are included if specified using sparse fieldsets" {
            let! response =
                Request.get
                    Ctx
                    "/persons/1?fields[person]=nonNullableRequiresExplicitInclude,nonNullableRequiresExplicitIncludeTrue,nonNullableRequiresExplicitIncludeFalse,nullableRequiresExplicitInclude,nullableRequiresExplicitIncludeTrue,nullableRequiresExplicitIncludeFalse"
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasPath "data.attributes.nonNullableRequiresExplicitInclude" @>
            test <@ json |> hasPath "data.attributes.nonNullableRequiresExplicitIncludeTrue" @>
            test <@ json |> hasPath "data.attributes.nonNullableRequiresExplicitIncludeFalse" @>
            test <@ json |> hasPath "data.attributes.nullableRequiresExplicitInclude" @>
            test <@ json |> hasPath "data.attributes.nullableRequiresExplicitIncludeTrue" @>
            test <@ json |> hasPath "data.attributes.nullableRequiresExplicitIncludeFalse" @>
        }

        testJob "Attributes with RequireExplicitInclude are not included if not specified using sparse fieldsets" {
            let! response = Request.get Ctx "/persons/1" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasNoPath "data.attributes.nonNullableRequiresExplicitInclude" @>
            test <@ json |> hasNoPath "data.attributes.nonNullableRequiresExplicitIncludeTrue" @>
            test <@ json |> hasPath "data.attributes.nonNullableRequiresExplicitIncludeFalse" @>
            test <@ json |> hasNoPath "data.attributes.nullableRequiresExplicitInclude" @>
            test <@ json |> hasNoPath "data.attributes.nullableRequiresExplicitIncludeTrue" @>
            test <@ json |> hasPath "data.attributes.nullableRequiresExplicitIncludeFalse" @>
        }

    ]
