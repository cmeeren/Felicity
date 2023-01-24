module ``POST collection with included``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type GreatGrandChild = { Id: string }


type GrandChild = {
    Id: string
    Name: string
    Children: GreatGrandChild list
}

type Child = {
    Id: string
    Name: string
    Name2: string option
    Child: GrandChild option
}

type Parent = { Id: string; Child: Child }


module GreatGrandChildDomain =

    let create = { Id = "ggc1" }


module GrandChildDomain =

    let create name children = {
        Id = "gc1"
        Name = name
        Children = children
    }


module ChildDomain =

    let create name child = {
        Id = "c1"
        Name = name
        Name2 = None
        Child = child
    }

    let setName2 name2 child = { child with Name2 = name2 }

module ParentDomain =

    let create child = { Id = "p1"; Child = child }



type Db() =
    let mutable Parents: Parent list = []

    member _.SaveParent p = Parents <- p :: Parents

    member _.GetParentOrFail id =
        Parents |> List.find (fun p -> p.Id = id)



type Ctx = Ctx of Db


module GreatGrandChild =

    let define = Define<Ctx, GreatGrandChild, string>()
    let resId = define.Id.Simple(fun c -> c.Id)
    let resDef = define.Resource("greatGrandChild", resId)



module GrandChild =

    let define = Define<Ctx, GrandChild, string>()
    let resId = define.Id.Simple(fun c -> c.Id)
    let resDef = define.Resource("grandChild", resId)

    let name = define.Attribute.SimpleString().Get(fun c -> c.Name)

    let children =
        define.Relationship.ToMany(GreatGrandChild.resDef).Get(fun c -> c.Children)


module Child =

    let define = Define<Ctx, Child, string>()
    let resId = define.Id.Simple(fun c -> c.Id)
    let resDef = define.Resource("child", resId)

    let name = define.Attribute.SimpleString().Get(fun c -> c.Name)

    let name2 = define.Attribute.Nullable.SimpleString().Get(fun c -> c.Name2)

    let child =
        define.Relationship.ToOneNullable(GrandChild.resDef).Get(fun c -> c.Child)


module Parent =

    let define = Define<Ctx, Parent, string>()
    let resId = define.Id.Simple(fun p -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")

    let child = define.Relationship.ToOne(Child.resDef).Get(fun p -> p.Child)

    let post =
        define.Operation
            .Post(fun ctx parser ->
                parser.For(
                    ParentDomain.create,
                    child.Included(fun parser ->
                        parser
                            .For(
                                ChildDomain.create,
                                Child.name,
                                Child.child.Included(fun parser ->
                                    parser.For(
                                        GrandChildDomain.create,
                                        GrandChild.name,
                                        GrandChild.children.Included(fun parser ->
                                            parser.For(GreatGrandChildDomain.create))
                                    ))
                            )
                            .Add(ChildDomain.setName2, Child.name2))
                ))
            .AfterCreate(fun (Ctx db) p -> db.SaveParent p)


[<Tests>]
let tests =
    testList "POST collection with included" [

        testJob "Successfully creates hierarchy with all entities" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {|
                                    name = "childName"
                                    name2 = "childName2"
                                |}
                                relationships = {|
                                    child = {|
                                        data = {|
                                            ``type`` = "grandChild"
                                            id = "ignoredId"
                                        |}
                                    |}
                                |}
                            |}
                            |> box

                            {|
                                ``type`` = "grandChild"
                                id = "ignoredId"
                                attributes = {| name = "grandChildName" |}
                                relationships = {|
                                    children = {|
                                        data = [|
                                            {|
                                                ``type`` = "greatGrandChild"
                                                id = "ignoredId"
                                            |}
                                            {|
                                                ``type`` = "greatGrandChild"
                                                id = "ignoredId2"
                                            |}
                                        |]
                                    |}
                                |}
                            |}
                            |> box

                            {|
                                ``type`` = "greatGrandChild"
                                id = "ignoredId1"
                            |}
                            |> box

                            {|
                                ``type`` = "greatGrandChild"
                                id = "ignoredId2"
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testSuccessStatusCode

            let p = db.GetParentOrFail "p1"

            let expected = {
                Id = "p1"
                Child = {
                    Id = "c1"
                    Name = "childName"
                    Name2 = Some "childName2"
                    Child =
                        Some
                            {
                                Id = "gc1"
                                Name = "grandChildName"
                                Children = [ { Id = "ggc1" }; { Id = "ggc1" } ]
                            }
                }
            }

            test <@ p = expected @>
        }


        testJob "Successfully creates hierarchy with null to-one nullable and missing optional attribute" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {| child = {| data = null |} |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testSuccessStatusCode

            let p = db.GetParentOrFail "p1"

            let expected = {
                Id = "p1"
                Child = {
                    Id = "c1"
                    Name = "childName"
                    Name2 = None
                    Child = None
                }
            }

            test <@ p = expected @>
        }


        testJob "Insensitive to trailing slashes" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents/"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {| child = {| data = null |} |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testSuccessStatusCode

            let p = db.GetParentOrFail "p1"

            let expected = {
                Id = "p1"
                Child = {
                    Id = "c1"
                    Name = "childName"
                    Name2 = None
                    Child = None
                }
            }

            test <@ p = expected @>
        }

        testJob "Returns 400 when missing required to-one nullable, with relationships member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = obj ()
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'child' is required for this operation" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationship is null" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {| child = null |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'child' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships/child" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing required to-one nullable, no relationships member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'child' is required for this operation" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing required to-many, with relationships member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {|
                                    child = {|
                                        data = {|
                                            ``type`` = "grandChild"
                                            id = "ignoredId"
                                        |}
                                    |}
                                |}
                            |}
                            |> box

                            {|
                                ``type`` = "grandChild"
                                id = "ignoredId"
                                attributes = {| name = "grandChildName" |}
                                relationships = obj ()
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is required for this operation" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/1/relationships" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing required to-many, no relationships member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {|
                                    child = {|
                                        data = {|
                                            ``type`` = "grandChild"
                                            id = "ignoredId"
                                        |}
                                    |}
                                |}
                            |}
                            |> box

                            {|
                                ``type`` = "grandChild"
                                id = "ignoredId"
                                attributes = {| name = "grandChildName" |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'children' is required for this operation" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/1" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing required attribute, with attributes member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = obj ()
                                relationships = {| child = {| data = null |} |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'name' is required for this operation" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/attributes" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing required attribute, no attributes member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                relationships = {| child = {| data = null |} |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Attribute 'name' is required for this operation" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing included resource, with included member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [||]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Expected to find referenced resource with type 'child' and ID 'ignoredId' in the 'included' part of the request"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/included" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing included resource, no included member" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Expected to find referenced resource with type 'child' and ID 'ignoredId' in the 'included' part of the request"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing included required relationship" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized {| data = {| ``type`` = "parent" |} |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'child' is required for this operation" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationships is missing data" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {| child = obj |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@ json |> getPath "errors[0].detail" = "Relationship 'child' was specified without relationship data" @>

            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships/child" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 409 if type is not allowed" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {|
                                    child = {|
                                        data = {| ``type`` = "invalid"; id = "foo" |}
                                    |}
                                |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 409
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "409" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Relationship 'child' contains data with invalid type 'invalid'; expected 'grandChild'"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships/child/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationships has missing type" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {|
                                    child = {| data = {| id = "foo" |} |}
                                |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'type'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }


        testJob "Returns 400 when relationships has null type" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {|
                                    child = {|
                                        data = {| ``type`` = null; id = "foo" |}
                                    |}
                                |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'type' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships/child/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationships has missing ID" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {|
                                    child = {| data = {| ``type`` = "child" |} |}
                                |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'id'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }


        testJob "Returns 400 when relationships has null ID" {
            let db = Db()

            let! response =
                Request.post (Ctx db) "/parents"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            relationships = {|
                                child = {|
                                    data = {|
                                        ``type`` = "child"
                                        id = "ignoredId"
                                    |}
                                |}
                            |}
                        |}

                        included = [|

                            {|
                                ``type`` = "child"
                                id = "ignoredId"
                                attributes = {| name = "childName" |}
                                relationships = {|
                                    child = {|
                                        data = {| ``type`` = "child"; id = null |}
                                    |}
                                |}
                            |}
                            |> box

                        |]
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'id' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/included/0/relationships/child/data/id" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns error if collection case does not match" {
            let db = Db()
            let! response = Request.post (Ctx db) "/Parents" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Parents' does not exist, but differs only by case from the existing path '/parents'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

    ]
