module ``Relationship GET parent access``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Child = { Id: string }

type Parent1 = { Id: string; Child: Child }

type Parent2 = { Id: string; Child: Child }

type Parent =
    | P1 of Parent1
    | P2 of Parent2

type Other = { Id: string; Child: Child }


type Ctx = {
    MapGetParent1Ctx: Ctx -> Parent1 -> Result<Ctx, Error list>
} with

    static member Default = {
        MapGetParent1Ctx = fun ctx _ -> Ok ctx
    }


module Child =

    let define = Define<Ctx, Child, string>()
    let resId = define.Id.Simple(fun (c: Child) -> c.Id)
    let resDef = define.Resource("child", resId)


module Parent1 =

    let define = Define<Ctx, Parent1, string>()
    let resId = define.Id.Simple(fun (p: Parent1) -> p.Id)
    let resDef = define.Resource("parent1", resId).CollectionName("parents")

    let get =
        define.Operation.ForContextRes(fun ctx p -> ctx.MapGetParent1Ctx ctx p).GetResource()

    let child = define.Relationship.ToOne(Child.resDef).Get(fun p -> p.Child)


module Parent2 =

    let define = Define<Ctx, Parent2, string>()
    let resId = define.Id.Simple(fun (p: Parent2) -> p.Id)
    let resDef = define.Resource("parent2", resId).CollectionName("parents")
    let child = define.Relationship.ToOne(Child.resDef).Get(fun p -> p.Child)


module Parent =

    let define = Define<Ctx, Parent, string>()

    let resId =
        define.Id.Simple(
            function
            | P1 p -> p.Id
            | P2 p -> p.Id
        )

    let resDef = define.PolymorphicResource(resId).CollectionName("parents")

    let lookup =
        define.Operation.Polymorphic.Lookup(
            (fun _ id ->
                match id with
                | "p1" -> Some(P1 { Id = "p1"; Child = { Id = "c1" } })
                | "p2" -> Some(P2 { Id = "p2"; Child = { Id = "c2" } })
                | _ -> None
            ),
            function
            | P1 p -> Parent1.resDef.PolymorphicFor p
            | P2 p -> Parent2.resDef.PolymorphicFor p
        )


module Other =

    let define = Define<Ctx, Other, string>()
    let resId = define.Id.Simple(fun (o: Other) -> o.Id)
    let resDef = define.Resource("other", resId).CollectionName("others")

    let lookup =
        define.Operation.Lookup(fun id ->
            if id = "o1" then
                Some { Id = "o1"; Child = { Id = "c3" } }
            else
                None
        )

    let child = define.Relationship.ToOne(Child.resDef).Get(fun o -> o.Child)


[<Tests>]
let tests =
    testList "Relationship GET parent access" [

        testJob "Related and self return 200 if the parent's GET resource operation allows the context" {
            for path in [ "/parents/p1/child"; "/parents/p1/relationships/child" ] do
                let! response = Request.get Ctx.Default path |> getResponse
                response |> testStatusCode 200
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "data.id" = "c1" @>
        }

        testJob "Related and self return the errors of the parent's GET resource operation context mapping" {
            let ctx = {
                MapGetParent1Ctx = fun _ p -> Error [ Error.create 422 |> Error.setCode p.Id ]
            }

            for path in [ "/parents/p1/child"; "/parents/p1/relationships/child" ] do
                let! response = Request.get ctx path |> getResponse
                response |> testStatusCode 422
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].code" = "p1" @>
                test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Related and self return 403 if the parent's resource type has no GET resource operation" {
            let expectedDetail =
                "Relationship 'child' on type 'parent2' is not readable (other resource types in collection 'parents' may have a readable relationship called 'child')"

            for path in [ "/parents/p2/child"; "/parents/p2/relationships/child" ] do
                let! response = Request.get Ctx.Default path |> getResponse
                response |> testStatusCode 403
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].detail" = expectedDetail @>
                test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob
            "Related and self return 403 before looking up the parent if no resource type in the collection has a GET resource operation" {
            let expectedDetail =
                "Relationship 'child' is not readable for any resource in collection 'others'"

            for path in
                [
                    "/others/o1/child"
                    "/others/o1/relationships/child"
                    "/others/missing/child"
                    "/others/missing/relationships/child"
                ] do
                let! response = Request.get Ctx.Default path |> getResponse
                response |> testStatusCode 403
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].detail" = expectedDetail @>
                test <@ json |> hasNoPath "errors[1]" @>
        }

    ]
