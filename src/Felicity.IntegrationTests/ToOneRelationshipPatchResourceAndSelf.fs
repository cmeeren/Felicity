module ``To-one relationship PATCH resource and self``

open System
open System.Text.Json.Serialization
open Microsoft.Net.Http.Headers
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type Child3 = { Id: string }

type Child1 = { Id: string; Child: Child3 }

type Child2 = { Id: string }

type Child =
    | C1 of Child1
    | C2 of Child2


type Parent1 = {
    Id: string
    Child: Child
    OtherChildId: string
}

type Parent2 = { Id: string; Child: Child }

type Parent3 = { Id: string }

type Parent4 = { Id: string }

type Parent =
    | P1 of Parent1
    | P2 of Parent2
    | P3 of Parent3
    | P4 of Parent4


type Db() =
    let mutable parents: Map<string, Parent> =
        Map.empty
        |> Map.add
            "p1"
            (P1
                {
                    Id = "p1"
                    Child = C1 { Id = "c1"; Child = { Id = "c3" } }
                    OtherChildId = "c2"
                })
        |> Map.add "p2" (P2 { Id = "p2"; Child = C2 { Id = "c2" } })
        |> Map.add "p3" (P3 { Id = "p3" })
        |> Map.add "p4" (P4 { Id = "p4" })

    let mutable children: Map<string, Child> =
        Map.empty
        |> Map.add "c1" (C1 { Id = "c1"; Child = { Id = "c3" } })
        |> Map.add "c2" (C2 { Id = "c2" })

    member _.TryGetParent id = parents.TryFind id

    member _.TryGetChild id = children.TryFind id

    member _.Save1(p1: Parent1) = parents <- parents.Add(p1.Id, P1 p1)

    member _.Save2(p2: Parent2) = parents <- parents.Add(p2.Id, P2 p2)


type Ctx =
    {
        Db: Db
        ModifyPatchSelfOkResponse: Parent1 -> Child -> HttpHandler
        ModifyPatchSelfAcceptedResponse: Parent1 -> HttpHandler
        GetParent1Child: Parent1 -> Child Skippable
        SetChild1: Child -> Parent1 -> Result<Parent1, Error list>
        SetOtherChildId1: string -> Parent1 -> Result<Parent1, Error list>
        ParseChild2Id: string -> Result<string, Error list>
        LookupChild: string -> Result<Child option, Error list>
        BeforeModifySelf1: Parent1 -> Result<unit, Error list>
        AfterUpdate1: Parent1 -> Parent1 -> unit
    }

    static member WithDb db = {
        Db = db
        ModifyPatchSelfOkResponse = fun _ _ -> fun next ctx -> next ctx
        ModifyPatchSelfAcceptedResponse = fun _ -> fun next ctx -> next ctx
        GetParent1Child = fun p -> Include p.Child
        SetChild1 = fun c p -> Ok { p with Child = c }
        SetOtherChildId1 = fun id p -> Ok { p with OtherChildId = id }
        ParseChild2Id = Ok
        LookupChild = db.TryGetChild >> Ok
        BeforeModifySelf1 = fun _ -> Ok()
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
    let subChild = define.Relationship.ToOne(Child3.resDef).Get(fun c -> c.Child)


module Child2 =

    let define = Define<Ctx, Child2, string>()

    let resId =
        define.Id.ParsedRes(id, (fun ctx -> ctx.ParseChild2Id), (fun (c: Child2) -> c.Id))

    let resDef = define.Resource("child2", resId)


module Child =

    let define = Define<Ctx, Child, string>()

    let lookup =
        define.Operation.Polymorphic.LookupRes(
            (fun ctx id -> ctx.LookupChild id),
            function
            | C1 c -> Child1.resDef.PolymorphicFor c
            | C2 c -> Child2.resDef.PolymorphicFor c
        )



module Parent1 = // set and get - PATCH resource/self OK

    let define = Define<Ctx, Parent1, string>()
    let resId = define.Id.Simple(fun (p: Parent1) -> p.Id)
    let resDef = define.Resource("parent1", resId).CollectionName("parents")
    let get = define.Operation.GetResource()

    let child =
        define.Relationship
            .Polymorphic()
            .AddIdParser(Child1.resDef, id)
            .AddIdParser(Child2.resDef, id)
            .ResolveEntity(
                function
                | C1 c -> Child1.resDef.PolymorphicFor c
                | C2 c -> Child2.resDef.PolymorphicFor c
            )
            .ToOne()
            .GetSkip(fun ctx p -> ctx.GetParent1Child p)
            .SetRes(Child.lookup, (fun ctx -> ctx.SetChild1))
            .BeforeModifySelfRes(fun ctx p -> ctx.BeforeModifySelf1 p)
            .AfterModifySelf(fun ctx -> ctx.AfterUpdate1)
            .ModifyPatchSelfOkResponse(fun ctx -> ctx.ModifyPatchSelfOkResponse)

    let otherChild =
        define.Relationship
            .Polymorphic()
            .AddIdParser(Child1.resDef, id)
            .AddIdParser(Child2.resDef, id)
            .ResolveEntity(
                function
                | C1 c -> Child1.resDef.PolymorphicFor c
                | C2 c -> Child2.resDef.PolymorphicFor c
            )
            .ToOne()
            .Get(fun ctx p ->
                ctx.Db.TryGetChild p.OtherChildId
                |> Option.defaultWith (fun () -> failwith "Not found"))
            .SetRes(fun ctx -> ctx.SetOtherChildId1)
            .AfterModifySelf(fun ctx -> ctx.AfterUpdate1)
            .PatchSelfReturn202Accepted()
            .ModifyPatchSelfAcceptedResponse(fun ctx -> ctx.ModifyPatchSelfAcceptedResponse)

    let patch = define.Operation.Patch().AfterUpdate(fun ctx p -> ctx.Db.Save1 p)


module Parent2 = // set without get - PATCH resource OK, PATCH self error

    let define = Define<Ctx, Parent2, string>()
    let resId = define.Id.Simple(fun (p: Parent2) -> p.Id)
    let resDef = define.Resource("parent2", resId).CollectionName("parents")
    let get = define.Operation.GetResource()

    let child =
        define.Relationship
            .Polymorphic()
            .AddIdParser(Child1.resDef, id)
            .AddIdParser(Child2.resDef, id)
            .ResolveEntity(
                function
                | C1 c -> Child1.resDef.PolymorphicFor c
                | C2 c -> Child2.resDef.PolymorphicFor c
            )
            .ToOne()
            .Set(Child.lookup, (fun c (p: Parent2) -> { p with Child = c }))
            .AfterModifySelf(fun ctx -> ctx.Db.Save2)

    let patch = define.Operation.Patch().AfterUpdate(fun ctx p -> ctx.Db.Save2 p)


module Parent3 = // no set - PATCH resource/self error

    let define = Define<Ctx, Parent3, string>()
    let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
    let resDef = define.Resource("parent3", resId).CollectionName("parents")
    let get = define.Operation.GetResource()

    let child =
        define.Relationship
            .Polymorphic()
            .AddIdParser(Child1.resDef, id)
            .AddIdParser(Child2.resDef, id)
            .ResolveEntity(
                function
                | C1 c -> Child1.resDef.PolymorphicFor c
                | C2 c -> Child2.resDef.PolymorphicFor c
            )
            .ToOne()
            .Get(fun _ ->
                C1
                    {
                        Id = "c3"
                        Child = { Id = "ignored" }
                    })

    let patch = define.Operation.Patch().AfterUpdate(ignore)


module Parent4 = // no relationship at all

    let define = Define<Ctx, Parent4, string>()
    let resId = define.Id.Simple(fun (p: Parent4) -> p.Id)
    let resDef = define.Resource("parent4", resId).CollectionName("parents")


module Parent =

    let define = Define<Ctx, Parent, string>()

    let resId =
        define.Id.Simple (function
            | P1 p -> p.Id
            | P2 p -> p.Id
            | P3 p -> p.Id
            | P4 p -> p.Id)

    let resDef = define.PolymorphicResource(resId).CollectionName("parents")

    let lookup =
        define.Operation.Polymorphic.Lookup(
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

    let child: ToOneRelationship<Ctx2, Ctx2, Parent4, obj, string> =
        define.Relationship.Polymorphic().ToOne()


type Ctx3 = Ctx3

module Parent6 =

    let define = Define<Ctx3, Parent1, string>()
    let resId = define.Id.Simple(fun _ -> failwith "not used")
    let resDef = define.Resource("p6", resId).CollectionName("parents")


type Ctx4 = Ctx4

module Parent7 = // ETag precondition

    let define = Define<Ctx4, Parent3, string>()
    let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")
    let lookup = define.Operation.Lookup(fun _ -> Some { Parent3.Id = "someId" })
    let get = define.Operation.GetResource()

    let preconditions =
        define.Preconditions.ETag(fun _ -> EntityTagHeaderValue.FromString false "valid-etag")

    let child =
        define.Relationship
            .ToOne(resDef)
            .Get(fun ctx -> { Parent3.Id = "someId" })
            .Set(fun ctx e -> e)
            .AfterModifySelf(ignore)



type Ctx5 = Ctx5

module Parent8 = // LastModified precondition

    let define = Define<Ctx5, Parent3, string>()
    let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")
    let lookup = define.Operation.Lookup(fun _ -> Some { Parent3.Id = "someId" })
    let get = define.Operation.GetResource()

    let preconditions =
        define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))

    let child =
        define.Relationship
            .ToOne(resDef)
            .Get(fun ctx -> { Parent3.Id = "someId" })
            .Set(fun ctx e -> e)
            .AfterModifySelf(ignore)


type Ctx6 = Ctx6

module Parent9 = // LastModified optional precondition

    let define = Define<Ctx6, Parent3, string>()
    let resId = define.Id.Simple(fun (p: Parent3) -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")
    let lookup = define.Operation.Lookup(fun _ -> Some { Parent3.Id = "someId" })
    let get = define.Operation.GetResource()

    let preconditions =
        define.Preconditions
            .LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero))
            .Optional

    let child =
        define.Relationship
            .ToOne(resDef)
            .Get(fun ctx -> { Parent3.Id = "someId" })
            .Set(fun ctx e -> e)
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
            .ToOne(resDef)
            .Get(fun _ _ -> failwith "never called")
            .Set(fun (ctx: MappedCtx) _ _ -> failwith "never called")
            .BeforeModifySelf(fun (_: MappedCtx) _ -> ())
            .AfterModifySelf(fun (_: MappedCtx) _ -> ())
            .ModifyPatchSelfOkResponse(fun (_: MappedCtx) _ _ _ -> ())
            .ModifyPatchSelfAcceptedResponse(fun (_: MappedCtx) _ _ -> ())

    let withEntity =
        define.Relationship.MapSetContext(fun (ctx: Ctx7) (e: string) -> MappedCtx)


[<Tests>]
let tests1 =
    testList "To-one relationship PATCH resource" [

        testJob "Get/set: Returns 200, saves, and returns correct data if successful" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1?include=child,otherChild"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child2"; id = "c2" |}
                                |}
                                otherChild = {|
                                    data = {| ``type`` = "child1"; id = "c1" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "parent1" @>
            test <@ json |> getPath "data.id" = "p1" @>
            test <@ json |> getPath "data.relationships.child.data.type" = "child2" @>
            test <@ json |> getPath "data.relationships.child.data.id" = "c2" @>
            test <@ json |> getPath "data.relationships.otherChild.data.type" = "child1" @>
            test <@ json |> getPath "data.relationships.otherChild.data.id" = "c1" @>

            let p =
                match db.TryGetParent "p1" with
                | Some(P1 p) -> p
                | _ -> failwith "not found"

            test <@ p.Id = "p1" @>
            test <@ p.Child = C2 { Id = "c2" } @>
            test <@ p.OtherChildId = "c1" @>
        }

        testJob "Set-only: Returns 200, saves, and returns correct data if successful" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p2?include=child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent2"
                            id = "p2"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child1"; id = "c1" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "parent2" @>
            test <@ json |> getPath "data.id" = "p2" @>
            test <@ json |> hasNoPath "data.relationships.child" @>

            let p =
                match db.TryGetParent "p2" with
                | Some(P2 p) -> p
                | _ -> failwith "not found"

            test <@ p.Id = "p2" @>
            test <@ p.Child = C1 { Id = "c1"; Child = { Id = "c3" } } @>
        }

        testJob "Related setter returns errors returned by related lookup's getById" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    LookupChild = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child2"; id = "c2" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related setter returns errors returned by setter" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    SetChild1 = fun _ _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child2"; id = "c2" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related setter returns 404 if related resource is not found" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    LookupChild = fun _ -> Ok None
                }

            let! response =
                Request.patch ctx "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child2"; id = "c2" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The related resource with type 'child2' and ID 'c2' does not exist"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related setter returns 404 if related ID parsing fails" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    ParseChild2Id = fun _ -> Error [ Error.create 422 ]
                }

            let! response =
                Request.patch ctx "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child2"; id = "c2" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The related resource with type 'child2' and ID 'c2' does not exist"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "ID setter returns errors returned by setter" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    SetOtherChildId1 = fun _ _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                otherChild = {|
                                    data = {| ``type`` = "child1"; id = "c1" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/otherChild/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "ID setter returns 404 if related ID parsing fails" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    ParseChild2Id = fun _ -> Error [ Error.create 422 ]
                }

            let! response =
                Request.patch ctx "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                otherChild = {|
                                    data = {| ``type`` = "child2"; id = "c2" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The related resource with type 'child2' and ID 'c2' does not exist"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/otherChild/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if relationship is not settable" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p3"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent3"
                            id = "p3"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child2"; id = "c2" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'child' is read-only" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if relationship is missing data" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {| child = obj () |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>

            test
                <@ json |> getPath "errors[0].detail" = "Relationship 'child' was specified without relationship data" @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 409 if type is not allowed" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "invalid"; id = "foo" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 409
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "409" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Relationship 'child' contains data with invalid type 'invalid'; expected one of 'child1', 'child2'"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationship has null data" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {| child = {| data = null |} |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'child' on type 'parent1' is not nullable" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationship data has missing type" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {| data = {| id = "foo" |} |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'type'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationship data has null type" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = null; id = "foo" |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'type' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationship data has missing ID" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {| data = {| ``type`` = "child2" |} |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'id'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when relationship data has null ID" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent1"
                            id = "p1"
                            relationships = {|
                                child = {|
                                    data = {| ``type`` = "child2"; id = null |}
                                |}
                            |}
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'id' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships/child/data/id" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

    ]


[<Tests>]
let tests2 =
    testList "To-one relationship PATCH self" [

        testJob "Parent1.child: Returns 200, modifies response, saves, and returns correct data if successful" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    ModifyPatchSelfOkResponse = fun _ _ -> setHttpHeader "Foo" "Bar"
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "child2" @>
            test <@ json |> getPath "data.id" = "c2" @>
            test <@ json |> hasNoPath "data.attributes" @>
            test <@ json |> hasNoPath "data.relationships" @>
            test <@ json |> hasNoPath "data.links" @>
            test <@ json |> hasNoPath "included" @>

            test <@ response.headers[NonStandard "Foo"] = "Bar" @>

            let p =
                match db.TryGetParent "p1" with
                | Some(P1 p) -> p
                | _ -> failwith "not found"

            test <@ p.Id = "p1" @>
            test <@ p.Child = C2 { Id = "c2" } @>
        }

        testJob "Insensitive to trailing slashes" {
            let ctx = Ctx.WithDb(Db())

            let! response =
                Request.patch ctx "/parents/p1/relationships/child/"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 200
        }

        testJob "Supports include parameter and ignores include paths not starting with relationship name" {
            let db = Db()
            let ctx = Ctx.WithDb db

            let! response =
                Request.patch ctx "/parents/p1/relationships/child?include=child.subChild,otherChild"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child1"; id = "c1" |}
                    |}
                |> getResponse

            response |> testStatusCode 200
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data.type" = "child1" @>
            test <@ json |> getPath "data.id" = "c1" @>
            test <@ json |> hasNoPath "data.attributes" @>
            test <@ json |> hasNoPath "data.relationships" @>
            test <@ json |> hasNoPath "data.links" @>
            test <@ json |> getPath "included.[0].type" = "child1" @>
            test <@ json |> getPath "included.[0].id" = "c1" @>
            test <@ json |> getPath "included.[0].relationships.subChild.data.type" = "child3" @>
            test <@ json |> getPath "included.[0].relationships.subChild.data.id" = "c3" @>
            test <@ json |> getPath "included.[1].type" = "child3" @>
            test <@ json |> getPath "included.[1].id" = "c3" @>
            test <@ json |> getPath "included.[1].attributes.c" = "abc" @>
            test <@ json |> hasNoPath "included.[2]" @>
        }

        testJob "Parent1.otherChild: Returns 202, modifies response, saves, and returns correct data if successful" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    ModifyPatchSelfAcceptedResponse = fun _ -> setHttpHeader "Foo" "Bar"
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/otherChild"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child1"; id = "c1" |}
                    |}
                |> getResponse

            response |> testStatusCode 202
            let! json = response |> Response.readBodyAsString
            test <@ json = "" @>

            test <@ response.headers[NonStandard "Foo"] = "Bar" @>

            let p =
                match db.TryGetParent "p1" with
                | Some(P1 p) -> p
                | _ -> failwith "not found"

            test <@ p.Id = "p1" @>
            test <@ p.OtherChildId = "c1" @>
        }

        testJob "Calls AfterModifySelf with the initial and new entity" {
            let db = Db()

            let pOrig =
                match db.TryGetParent "p1" with
                | Some(P1 p) -> p
                | _ -> failwith "not found"

            let pExpected = {
                Id = "p1"
                Child = C2 { Id = "c2" }
                OtherChildId = "c2"
            }

            let mutable called = false

            let afterUpdate before after =
                called <- true
                test <@ before = pOrig @>
                test <@ after = pExpected @>

            let ctx =
                { Ctx.WithDb db with
                    AfterUpdate1 = afterUpdate
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testSuccessStatusCode

            let called' = called
            test <@ called' = true @>

        }

        testJob "Correctly handles precondition validation using ETag" {
            let! response =
                Request.patch Ctx4 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 428
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "428" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.patch Ctx4 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
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
                Request.patch Ctx4 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> Request.setHeader (IfMatch "\"valid-etag\"")
                |> getResponse

            test <@ response.headers[ETag] <> "\"valid-etag\"" @>
            response |> testStatusCode 200
        }

        testJob "Correctly handles precondition validation using If-Unmodified-Since" {
            let! response =
                Request.patch Ctx5 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 428
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "428" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.patch Ctx5 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.patch Ctx5 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "Correctly handles optional precondition validation" {
            let! response =
                Request.patch Ctx6 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> getResponse

            response |> testStatusCode 200

            let! response =
                Request.patch Ctx6 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> Request.setHeader (Custom("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
                |> getResponse

            response |> testStatusCode 412
            test <@ response.headers.ContainsKey LastModified = false @>
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "412" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let! response =
                Request.patch Ctx6 "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {|
                            ``type`` = "parent"
                            id = "ignoredId"
                        |}
                    |}
                |> Request.setHeader (Custom("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
                |> getResponse

            test <@ response.headers.ContainsKey LastModified = false @>
            response |> testStatusCode 200
        }

        testJob "Returns errors and does not call AfterModifySelf when BeforeModifySelf returns errors" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    BeforeModifySelf1 = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                    AfterUpdate1 = fun _ _ -> failwith "should not be called"
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if the relationship has no getter" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p2/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child1"; id = "c1" |}
                    |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Relationship 'child' on type 'parent2' is write-only and may only be updated through PATCH requests to the parent resource (other resource types in collection 'parents' may have a relationship called 'child' that supports this operation)"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related setter returns errors returned by related lookup's getById" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    LookupChild = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related setter returns errors returned by setter" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    SetChild1 = fun _ _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related setter returns 404 if related resource is not found" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    LookupChild = fun _ -> Ok None
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The related resource with type 'child2' and ID 'c2' does not exist"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related setter returns 404 if related ID parsing fails" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    ParseChild2Id = fun _ -> Error [ Error.create 422 ]
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The related resource with type 'child2' and ID 'c2' does not exist"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "ID setter returns errors returned by setter" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    SetOtherChildId1 = fun _ _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/otherChild"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child1"; id = "c1" |}
                    |}
                |> getResponse

            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "ID setter returns 404 if related ID parsing fails" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    ParseChild2Id = fun _ -> Error [ Error.create 422 ]
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/otherChild"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The related resource with type 'child2' and ID 'c2' does not exist"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if relationship is not settable" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p3/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'child' on type 'parent3' is read-only" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing body" {
            let db = Db()
            let! response = Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "The request must specify the primary data to update" @>
            test <@ json |> getPath "errors[0].source.pointer" = "" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when JSON is invalid" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodyString
                    """
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

            test
                <@
                    json |> getPath "errors[0].detail" = "Invalid JSON or incorrect data type at path $.test, line 3, position 19"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if missing data" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodySerialized (obj ())
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'data'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if data is null" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodySerialized {| data = null |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Relationship 'child' on type 'parent1' is not nullable" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing type" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodySerialized {| data = {| id = "p1" |} |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'type'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when type is null" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = null; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'type' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when missing ID" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodySerialized {| data = {| ``type`` = "child2" |} |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Missing required member 'id'" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 when ID is null" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = null |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "Member 'id' may not be null" @>
            test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 409 if type is not allowed" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "invalid"; id = "foo" |}
                    |}
                |> getResponse

            response |> testStatusCode 409
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "409" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Data contains invalid type 'invalid'; expected one of 'child1', 'child2'"
                @>

            test <@ json |> getPath "errors[0].source.pointer" = "/data/type" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Saves and returns 500 if Skip after update" {
            let db = Db()

            let ctx =
                { Ctx.WithDb db with
                    GetParent1Child = fun _ -> Skip
                }

            let! response =
                Request.patch ctx "/parents/p1/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 500
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "500" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The relationship was updated, but the server has erroneously chosen not to disclose the value of the updated relationship"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>

            let p =
                match db.TryGetParent "p1" with
                | Some(P1 p) -> p
                | _ -> failwith "not found"

            test <@ p.Id = "p1" @>
            test <@ p.Child = C2 { Id = "c2" } @>
        }

        testJob "Returns 404 if relationship does not exist for resource" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/p4/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Relationship 'child' is not defined for resource type 'parent4' (other resource types in collection 'parents' may have a relationship called 'child')"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if relationship exists but is not settable for any resource" {
            let! response =
                Request.patch Ctx2 "/parents/p5/relationships/child"
                |> Request.bodySerialized
                    {|
                        data = {| ``type`` = "child2"; id = "c2" |}
                    |}
                |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Relationship 'child' does not support PATCH for any resource in collection 'parents'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 404 if resource is not found" {
            let db = Db()

            let! response =
                Request.patch (Ctx.WithDb db) "/parents/invalidId/relationships/child"
                |> getResponse

            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'parents' does not contain a resource with ID 'invalidId'"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if missing lookup" {
            let! response = Request.get Ctx3 "/parents/ignoredId/relationships/child" |> getResponse

            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "Collection 'parents' does not support any resource-specific operations"
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns error if collection case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.patch ctx "/Parents/p1/relationships/child" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Parents/p1/relationships/child' does not exist, but differs only by case from the existing path '/parents/p1/relationships/child'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns error if 'relationships' case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.patch ctx "/parents/p1/Relationships/child" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/parents/p1/Relationships/child' does not exist, but differs only by case from the existing path '/parents/p1/relationships/child'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns error if relationship case does not match" {
            let ctx = Ctx.WithDb(Db())
            let! response = Request.patch ctx "/parents/p1/relationships/Child" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/parents/p1/relationships/Child' does not exist, but differs only by case from the existing path '/parents/p1/relationships/child'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

    ]
