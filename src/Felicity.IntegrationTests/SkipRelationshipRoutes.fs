module ``SkipRelationshipIf on relationship routes``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Child = { Id: string }

type Parent = { Id: string }


type Ctx = {
    SkippedParentId: string
    Modifications: ResizeArray<string>
} with

    static member Create() = {
        SkippedParentId = "p1"
        Modifications = ResizeArray()
    }


module Child =

    let define = Define<Ctx, Child, string>()
    let resId = define.Id.Simple(fun (c: Child) -> c.Id)
    let resDef = define.Resource("child", resId)


module Parent =

    let define = Define<Ctx, Parent, string>()
    let resId = define.Id.Simple(fun (p: Parent) -> p.Id)
    let resDef = define.Resource("parent", resId).CollectionName("parents")

    let lookup =
        define.Operation.Lookup(fun id -> if id = "p1" || id = "p2" then Some { Id = id } else None)

    let get = define.Operation.GetResource()

    let recordModification name =
        fun (ctx: Ctx) (_: Parent) -> ctx.Modifications.Add name

    let toOne =
        define.Relationship
            .ToOne(Child.resDef)
            .SkipRelationshipIf(fun ctx p -> p.Id = ctx.SkippedParentId)
            .Get(fun _ -> { Child.Id = "c1" })
            .Set(fun (_: string) p -> p)
            .AfterModifySelf(recordModification "toOne")

    let toOneNullable =
        define.Relationship
            .ToOneNullable(Child.resDef)
            .SkipRelationshipIf(fun ctx p -> p.Id = ctx.SkippedParentId)
            .Get(fun _ -> Some { Child.Id = "c1" })
            .Set(fun (_: string option) p -> p)
            .AfterModifySelf(recordModification "toOneNullable")

    let toMany =
        define.Relationship
            .ToMany(Child.resDef)
            .SkipRelationshipIf(fun ctx p -> p.Id = ctx.SkippedParentId)
            .Get(fun _ -> [ { Child.Id = "c1" } ])
            .SetAll(fun (_: string list) p -> p)
            .Add(fun (_: string list) p -> p)
            .Remove(fun (_: string list) p -> p)
            .AfterModifySelf(recordModification "toMany")

    let polymorphicToOne =
        define.Relationship
            .Polymorphic()
            .AddIdParser(Child.resDef, id)
            .ResolveEntity(fun c -> Child.resDef.PolymorphicFor c)
            .ToOne()
            .SkipRelationshipIf(fun ctx p -> p.Id = ctx.SkippedParentId)
            .Get(fun _ -> { Child.Id = "c1" })
            .Set(fun (_: string) p -> p)
            .AfterModifySelf(recordModification "polymorphicToOne")


let private toOneRels = [ "toOne"; "toOneNullable"; "polymorphicToOne" ]

let private allRels = toOneRels @ [ "toMany" ]

let private identifier = {| ``type`` = "child"; id = "c1" |}

let private modificationRequests ctx parentId = [
    for rel in toOneRels do
        rel,
        Request.patch ctx $"/parents/%s{parentId}/relationships/%s{rel}"
        |> Request.bodySerialized {| data = identifier |}

    for request in [ Request.patch; Request.post; Request.delete ] do
        "toMany",
        request ctx $"/parents/%s{parentId}/relationships/toMany"
        |> Request.bodySerialized {| data = [ identifier ] |}
]


[<Tests>]
let tests =
    testList "SkipRelationshipIf on relationship routes" [

        testJob "Related and self routes return 403 if the relationship is skipped" {
            for rel in allRels do
                for path in [ $"/parents/p1/%s{rel}"; $"/parents/p1/relationships/%s{rel}" ] do
                    let! response = Request.get (Ctx.Create()) path |> getResponse
                    response |> testStatusCode 403
                    let! json = response |> Response.readBodyAsString
                    test <@ json |> getPath "errors[0].status" = "403" @>
                    test <@ json |> getPath "errors[0].title" = "No relationship value" @>
                    test <@ json |> hasNoPath "errors[1]" @>
                    test <@ json |> hasNoPath "data" @>
        }

        testJob "Related and self routes return the relationship if it is not skipped" {
            for rel in allRels do
                for path in [ $"/parents/p2/%s{rel}"; $"/parents/p2/relationships/%s{rel}" ] do
                    let! response = Request.get (Ctx.Create()) path |> getResponse
                    response |> testStatusCode 200
                    let! json = response |> Response.readBodyAsString

                    if rel = "toMany" then
                        test <@ json |> getPath "data[0].id" = "c1" @>
                    else
                        test <@ json |> getPath "data.id" = "c1" @>
        }

        testJob "Self modification routes return 403 without modifying if the relationship is skipped" {
            let ctx = Ctx.Create()

            for _, request in modificationRequests ctx "p1" do
                let! response = request |> getResponse
                response |> testStatusCode 403
                let! json = response |> Response.readBodyAsString
                test <@ json |> getPath "errors[0].title" = "Relationship not available" @>
                test <@ json |> hasNoPath "errors[1]" @>
                test <@ ctx.Modifications |> Seq.isEmpty @>
        }

        testJob "Self modification routes modify the relationship if it is not skipped" {
            let ctx = Ctx.Create()

            for rel, request in modificationRequests ctx "p2" do
                let modificationsBefore = ctx.Modifications.Count
                let! response = request |> getResponse
                response |> testStatusCode 200
                test <@ ctx.Modifications.Count = modificationsBefore + 1 @>
                test <@ ctx.Modifications |> Seq.last = rel @>
        }

        testJob "The parent resource omits skipped relationships only" {
            let! response = Request.get (Ctx.Create()) "/parents/p1" |> getResponse
            response |> testStatusCode 200
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasNoPath "data.relationships" @>

            let! response = Request.get (Ctx.Create()) "/parents/p2" |> getResponse
            response |> testStatusCode 200
            let! json = response |> Response.readBodyAsString

            for rel in allRels do
                test <@ json |> hasPath $"data.relationships.%s{rel}" @>
        }

    ]
