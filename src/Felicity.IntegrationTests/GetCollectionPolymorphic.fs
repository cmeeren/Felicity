module ``GET collection polymorphic``

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = { Id: string; A: bool }

type B = { Id: string; B: int }

type AB =
    | A of A
    | B of B

let a = A { Id = "a1"; A = true }
let b = B { Id = "b2"; B = 2 }

type MappedCtx = {
    ModifyResponse: AB list -> HttpHandler
    GetColl: unit -> Result<AB list, Error list>
}

type Ctx =
    {
        ModifyResponse: AB list -> HttpHandler
        GetColl: unit -> Result<AB list, Error list>
        MapCtx: Ctx -> Result<MappedCtx, Error list>
    }

    static member Default = {
        ModifyResponse = fun _ -> fun next ctx -> next ctx
        GetColl = fun () -> Ok [ a; b ]
        MapCtx =
            fun ctx ->
                Ok
                    {
                        ModifyResponse = ctx.ModifyResponse
                        GetColl = ctx.GetColl
                    }
    }


module A =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun (a: A) -> a.Id)
    let resDef = define.Resource("a", resId).CollectionName("abs")
    let a = define.Attribute.SimpleBool().Get(fun a -> a.A)


module B =

    let define = Define<Ctx, B, string>()
    let resId = define.Id.Simple(fun (b: B) -> b.Id)
    let resDef = define.Resource("b", resId).CollectionName("abs")
    let b = define.Attribute.SimpleInt().Get(fun b -> b.B)



module AB =

    let define = Define<Ctx, AB, string>()

    let resId =
        define.Id.Simple (function
            | A a -> a.Id
            | B b -> b.Id)

    let resDef = define.PolymorphicResource(resId).CollectionName("abs")

    let getColl =
        define.Operation
            .ForContextRes(fun ctx -> ctx.MapCtx ctx)
            .Polymorphic.GetCollectionRes(
                (fun (ctx: MappedCtx) -> ctx.GetColl()),
                function
                | A a -> A.resDef.PolymorphicFor a
                | B b -> B.resDef.PolymorphicFor b
            )
            .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyResponse)


type Ctx2 = Ctx2


module AB2 =

    let define = Define<Ctx2, AB, string>()

    let resId =
        define.Id.ParsedOpt(id, (fun _ _ -> failwith "not used"), (fun _ -> failwith "not used"))

    let resDef = define.PolymorphicResource(resId).CollectionName("abs")


module AB3 =

    let define = Define<Ctx, AB, string>()

    let resId =
        define.Id.Simple (function
            | A a -> a.Id
            | B b -> b.Id)

    let resDef = define.PolymorphicResource(resId).CollectionName("sortTest")

    let getColl =
        define.Operation.Polymorphic.GetCollection(
            (fun ctx parser -> parser.For(id, Sort.Enum([ A.a.Name, "a" ])).Map(fun _ -> [ a ])),
            function
            | A a -> A.resDef.PolymorphicFor a
            | B b -> B.resDef.PolymorphicFor b
        )


[<Tests>]
let tests =
    testList "GET collection polymorphic" [

        testJob "Returns 200 if successful" {
            let! response = Request.get Ctx.Default "/abs" |> getResponse
            response |> testStatusCode 200
        }

        testJob "Insensitive to trailing slashes" {
            let! response = Request.get Ctx.Default "/abs/" |> getResponse
            response |> testStatusCode 200
        }

        testJob "Correctly handles ETag and If-None-Match" {
            let! response = Request.get Ctx.Default "/abs" |> getResponse
            response |> testStatusCode 200
            let eTag = response.headers[ETag]

            let! response =
                Request.get Ctx.Default "/abs"
                |> Request.setHeader (IfNoneMatch eTag)
                |> getResponse

            response |> testStatusCode 304
        }

        testJob "Returns correct data" {
            let! response = Request.get Ctx.Default "/abs" |> getResponse
            response |> testSuccessStatusCode
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "data[0].type" = "a" @>
            test <@ json |> getPath "data[0].id" = "a1" @>
            test <@ json |> getPath "data[0].attributes.a" = true @>
            test <@ json |> getPath "data[1].type" = "b" @>
            test <@ json |> getPath "data[1].id" = "b2" @>
            test <@ json |> getPath "data[1].attributes.b" = 2 @>
        }

        testJob "Modifies response if successful" {
            let ctx =
                { Ctx.Default with
                    ModifyResponse = fun _ -> setHttpHeader "Foo" "Bar"
                }

            let! response = Request.get ctx "/abs" |> getResponse
            response |> testSuccessStatusCode
            test <@ response.headers[NonStandard "Foo"] = "Bar" @>
        }

        testJob "Returns errors returned by GetColl" {
            let ctx =
                { Ctx.Default with
                    GetColl = fun () -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response = Request.get ctx "/abs" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns errors returned by mapCtx" {
            let ctx =
                { Ctx.Default with
                    MapCtx = fun _ -> Error [ Error.create 422 |> Error.setCode "custom" ]
                }

            let! response = Request.get ctx "/abs" |> getResponse
            response |> testStatusCode 422
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "422" @>
            test <@ json |> getPath "errors[0].code" = "custom" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 403 if not supported" {
            let! response = Request.get Ctx2 "/abs" |> getResponse
            response |> testStatusCode 403
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "403" @>
            test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not support fetching resources" @>
            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 400 if sort not supported" {
            let! response = Request.get Ctx.Default "/abs?sort=a" |> getResponse
            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "400" @>
            test <@ json |> getPath "errors[0].detail" = "This operation does not support sorting" @>
            test <@ json |> getPath "errors[0].source.parameter" = "sort" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

        testJob "Returns 200 if sort supported" {
            let! response = Request.get Ctx.Default "/sortTest?sort=a" |> getResponse
            response |> testStatusCode 200
        }

        testJob "Returns error if collection case does not match" {
            let! response = Request.get Ctx.Default "/Abs" |> getResponse
            response |> testStatusCode 404
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].status" = "404" @>

            test
                <@
                    json |> getPath "errors[0].detail" = "The path '/Abs' does not exist, but differs only by case from the existing path '/abs'. Paths are case sensitive."
                @>

            test <@ json |> hasNoPath "errors[0].source" @>
            test <@ json |> hasNoPath "errors[1]" @>
        }

    ]
