module ``Set order``

open Expecto
open HttpFs.Client
open Felicity


type Ctx = Ctx of int list ref


module X =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(fun _ -> "someId")
    let resDef = define.Resource("a", resId).CollectionName("as")
    let lookup = define.Operation.Lookup(fun _ -> Some "")
    let get = define.Operation.GetResource()

    let a =
        define.Attribute
            .SimpleString()
            .SetOrder(5)
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ 5 ]
                e
            )

    let b =
        define.Attribute
            .SimpleString()
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ 0 ]
                e
            )

    let c =
        define.Attribute.Nullable
            .SimpleString()
            .SetOrder(-3)
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ -3 ]
                e
            )

    let d =
        define.Attribute.Nullable
            .SimpleString()
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ 0 ]
                e
            )

    let e =
        define.Relationship
            .ToOne(resDef)
            .SetOrder(20)
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ 20 ]
                e
            )
            .AfterModifySelf(ignore)

    let f =
        define.Relationship
            .ToOne(resDef)
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ 0 ]
                e
            )
            .AfterModifySelf(ignore)

    let g =
        define.Relationship
            .ToOneNullable(resDef)
            .SetOrder(-80)
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ -80 ]
                e
            )
            .AfterModifySelf(ignore)

    let h =
        define.Relationship
            .ToOneNullable(resDef)
            .Set(fun (Ctx xs) _ e ->
                xs := !xs @ [ 0 ]
                e
            )
            .AfterModifySelf(ignore)

    let i =
        define.Relationship
            .ToMany(resDef)
            .SetOrder(6)
            .SetAll(fun (Ctx xs) _ e ->
                xs := !xs @ [ 6 ]
                e
            )
            .AfterModifySelf(ignore)

    let j =
        define.Relationship
            .ToMany(resDef)
            .SetAll(fun (Ctx xs) _ e ->
                xs := !xs @ [ 0 ]
                e
            )
            .AfterModifySelf(ignore)

    let post = define.Operation.Post(fun () -> "").AfterCreate(fun _ -> ())

    let patch = define.Operation.Patch().AfterUpdate(fun _ -> ())


[<Tests>]
let tests =
    testList "Set order" [

        testJob "Post: Sets fields in the correct order" {
            let xs = ref []
            let ctx = Ctx xs

            let! response =
                Request.post ctx "/as"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        attributes = {|
                            a = "ignored"
                            b = "ignored"
                            c = "ignored"
                            d = "ignored"
                        |}
                        relationships = {|
                            e = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            f = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            g = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            h = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            i = {|
                                data = [ {| ``type`` = "a"; id = "ignored" |} ]
                            |}
                            j = {|
                                data = [ {| ``type`` = "a"; id = "ignored" |} ]
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            Expect.isNonEmpty !xs "No fields were set"
            Expect.sequenceContainsOrder !xs (List.sort !xs) "Incorrect set order"
        }

        testJob "Patch: Sets fields in the correct order" {
            let xs = ref []
            let ctx = Ctx xs

            let! response =
                Request.patch ctx "/as/someId"
                |> Request.bodySerialized {|
                    data = {|
                        ``type`` = "a"
                        id = "someId"
                        attributes = {|
                            a = "ignored"
                            b = "ignored"
                            c = "ignored"
                            d = "ignored"
                        |}
                        relationships = {|
                            e = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            f = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            g = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            h = {|
                                data = {| ``type`` = "a"; id = "ignored" |}
                            |}
                            i = {|
                                data = [ {| ``type`` = "a"; id = "ignored" |} ]
                            |}
                            j = {|
                                data = [ {| ``type`` = "a"; id = "ignored" |} ]
                            |}
                        |}
                    |}
                |}
                |> getResponse

            response |> testSuccessStatusCode
            Expect.isNonEmpty !xs "No fields were set"
            Expect.sequenceContainsOrder !xs (List.sort !xs) "Incorrect set order"
        }


    ]
