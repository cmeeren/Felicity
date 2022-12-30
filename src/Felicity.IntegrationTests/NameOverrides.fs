module ``Name overrides``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type Ctx = Ctx

type X = X


module X =

    let define = Define<Ctx, X, string>()
    let resId = define.Id.Simple(fun _ -> "someId")
    let resDef = define.Resource("x", resId).CollectionName("xs")

    let nonNullable =
        define.Attribute.SimpleBool("nonNullableCustomName").Get(fun _ -> true)

    let nullable =
        define
            .Attribute
            .Nullable
            .SimpleBool("nullableCustomName")
            .Get(fun _ -> Some true)

    let toOne = define.Relationship.ToOne(resDef, "toOneCustomName").Get(fun _ _ -> X)

    let toOneNullable =
        define.Relationship.ToOne(resDef, "toOneNullableCustomName").Get(fun _ _ -> X)

    let toMany = define.Relationship.ToOne(resDef, "toManyCustomName").Get(fun _ _ -> X)

    let customOp =
        define
            .Operation
            .CustomLink("customOpCustomName")
            .ValidateStrictModeQueryParams()
            .GetAsync(fun _ _ _ _ -> setStatusCode 200 |> Ok |> async.Return)

    let lookup = define.Operation.Lookup(fun _ -> Some X)
    let getColl = define.Operation.GetCollection(fun () -> [ X ])
    let get = define.Operation.GetResource()


[<Tests>]
let tests =
    testList "Name overrides" [

        testJob "Uses overridden name for attributes, relationships, and custom operations" {
            let! response = Request.get Ctx "/xs" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> hasPath "data[0].attributes.nonNullableCustomName" @>
            test <@ json |> hasPath "data[0].attributes.nullableCustomName" @>
            test <@ json |> hasPath "data[0].relationships.toOneCustomName" @>
            test <@ json |> hasPath "data[0].relationships.toOneNullableCustomName" @>
            test <@ json |> hasPath "data[0].relationships.toManyCustomName" @>
            test <@ json |> hasPath "data[0].links.customOpCustomName" @>
        }

    ]
