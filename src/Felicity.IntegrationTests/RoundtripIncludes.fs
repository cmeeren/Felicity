module ``Roundtrip includes``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx = Ctx

type A = { Id: string }

let gen1 = { Id = "gen1" }

let gen2 = { Id = "gen2" }

let gen3 = { Id = "gen3" }


let getParent =
    function
    | { Id = "gen1" } -> None
    | { Id = "gen2" } -> Some gen1
    | { Id = "gen3" } -> Some gen2
    | _ -> failwith "Invalid argument"

let getChild =
    function
    | { Id = "gen1" } -> Some gen2
    | { Id = "gen2" } -> Some gen3
    | { Id = "gen3" } -> None
    | _ -> failwith "Invalid argument"



module A =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun p -> p.Id)
    let resDef = define.Resource("a", resId).CollectionName("as")

    let parent = define.Relationship.ToOneNullable(resDef).Get(getParent)
    let child = define.Relationship.ToOneNullable(resDef).Get(getChild)
    let children = define.Relationship.ToMany(resDef).Get(getChild >> Option.toList)

    let getCollection = define.Operation.GetCollection(fun () -> [ gen2 ])


[<Tests>]
let tests =
    testList "Roundtrip includes" [

        testJob "Returns correct relationship data, and each resource is only included once" {
            let! response =
                Request.get Ctx "/as?include=parent.child.child,parent.child.children"
                |> getResponse

            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString

            test <@ json |> getPath "data[0].type" = "a" @>
            test <@ json |> getPath "data[0].id" = "gen2" @>
            test <@ json |> getPath "data[0].relationships.parent.data.type" = "a" @>
            test <@ json |> getPath "data[0].relationships.parent.data.id" = "gen1" @>
            test <@ json |> getPath "data[0].relationships.child.data.type" = "a" @>
            test <@ json |> getPath "data[0].relationships.child.data.id" = "gen3" @>
            test <@ json |> getPath "data[0].relationships.children.data[0].type" = "a" @>
            test <@ json |> getPath "data[0].relationships.children.data[0].id" = "gen3" @>
            test <@ json |> hasNoPath "data[0].relationships.children.data[1].id" @>

            test <@ json |> hasNoPath "data[1]" @>

            test <@ json |> getPath "included[0].type" = "a" @>
            test <@ json |> getPath "included[0].id" = "gen1" @>
            test <@ json |> hasNoPath "included[0].relationships.parent.data" @>
            test <@ json |> hasNoPath "included[0].relationships.children.data" @>
            test <@ json |> getPath "included[0].relationships.child.data.type" = "a" @>
            test <@ json |> getPath "included[0].relationships.child.data.id" = "gen2" @>

            test <@ json |> getPath "included[1].type" = "a" @>
            test <@ json |> getPath "included[1].id" = "gen3" @>
            test <@ json |> hasNoPath "included[1].relationships.parent.data" @>
            test <@ json |> hasNoPath "included[1].relationships.child.data" @>
            test <@ json |> hasNoPath "included[1].relationships.children.data" @>

            test <@ json |> hasNoPath "included[2]" @>
        }

    ]
