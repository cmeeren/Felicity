module Errors

open Expecto
open HttpFs.Client
open Swensen.Unquote
open Felicity


type Ctx = Ctx

module A =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("a", resId).CollectionName("as")

    let a =
        define
            .Attribute
            .SimpleBool()
            .SetRes(fun _ _ _ -> Error [ Error.create 400 |> Error.appendPointer "/foo/bar" ])

    let lookup = define.Operation.Lookup(Some)
    let get = define.Operation.GetResource()
    let patch = define.Operation.Patch().AfterUpdate(ignore)


[<Tests>]
let tests =
    testList "Errors" [

        testJob "Error.appendPointer works for attribute set errors" {
            let! response =
                Request.patch Ctx "/as/a1"
                |> Request.bodySerialized
                    {|
                        data =
                            {|
                                ``type`` = "a"
                                id = "a1"
                                attributes = {| a = true |}
                            |}
                    |}
                |> getResponse

            response |> testStatusCode 400
            let! json = response |> Response.readBodyAsString
            test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/a/foo/bar" @>
        }

    ]
