module ``Field usage tracker``

open System
open Expecto
open HttpFs.Client
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Felicity
open Hopac


type Ctx = Ctx

type A = A of string
type B = B of string
type C = C of string
type D = D of string

type CD =
    | CDC of C
    | CDD of D


module C =

    let define = Define<Ctx, C, string>()
    let resId = define.Id.Simple(fun (C id) -> id)
    let resDef = define.Resource("c", resId).CollectionName("cds")

    let c = define.Attribute.SimpleString().Get(fun _ -> "")

    let relToOne =
        define.Relationship
            .ToOne(resDef)
            .Get(fun _ -> C "1")
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relToOneNullable =
        define.Relationship
            .ToOneNullable(resDef)
            .Get(fun _ -> None)
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relToMany =
        define.Relationship
            .ToMany(resDef)
            .Get(fun _ -> [])
            .SetAll(fun _ a -> a)
            .Add(fun _ a -> a)
            .Remove(fun _ a -> a)
            .AfterModifySelf(ignore)

    let post =
        define.Operation.Post(fun () -> C "1").Return202Accepted().AfterCreate(ignore)

    let get = define.Operation.GetResource()


module D =

    let define = Define<Ctx, D, string>()
    let resId = define.Id.Simple(fun (D id) -> id)
    let resDef = define.Resource("d", resId).CollectionName("cds")

    let c = define.Attribute.SimpleString().Get(fun _ -> "")

    let relToOne =
        define.Relationship
            .ToOne(C.resDef)
            .Get(fun _ -> C "1")
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relToOneNullable =
        define.Relationship
            .ToOneNullable(C.resDef)
            .Get(fun _ -> None)
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relToMany =
        define.Relationship
            .ToMany(C.resDef)
            .Get(fun _ -> [])
            .SetAll(fun _ a -> a)
            .Add(fun _ a -> a)
            .Remove(fun _ a -> a)
            .AfterModifySelf(ignore)

    let postCustom202 =
        define.Operation.PostCustomAsync(fun _ parser helper ->
            async {
                let parser = parser.For((fun _ _ -> D "1"), resId, c)

                do!
                    helper.ValidateRequestAsync(parser)
                    |> Async.map (
                        function
                        | Ok x -> x
                        | Error _ -> failwith "Should not happen"
                    )

                let! d =
                    parser.ParseAsync()
                    |> Async.map (
                        function
                        | Ok x -> x
                        | Error _ -> failwith "Should not happen"
                    )

                let! _d =
                    helper.RunSettersAsync(d, parser)
                    |> Async.map (
                        function
                        | Ok x -> x
                        | Error _ -> failwith "Should not happen"
                    )

                return helper.Return202Accepted() |> Ok
            }
        )

    let get = define.Operation.GetResource()


module B =

    let define = Define<Ctx, B, string>()
    let resId = define.Id.Simple(fun (B id) -> id)
    let resDef = define.Resource("b", resId).CollectionName("bs")

    let a = define.Attribute.SimpleString().Get(fun _ -> "").Set(fun _ b -> b)

    let b = define.Attribute.SimpleString().Get(fun _ -> "")

    let relC = define.Relationship.ToOne(C.resDef).Get(fun _ -> C "1")

    let postCustom =
        define.Operation.PostCustomAsync(fun _ parser helper ->
            async {
                let parser = parser.For((fun _ -> B "1"), b)

                do!
                    helper.ValidateRequestAsync(parser)
                    |> Async.map (
                        function
                        | Ok x -> x
                        | Error _ -> failwith "Should not happen"
                    )

                let! b =
                    parser.ParseAsync()
                    |> Async.map (
                        function
                        | Ok x -> x
                        | Error _ -> failwith "Should not happen"
                    )

                let! b =
                    helper.RunSettersAsync(b, parser)
                    |> Async.map (
                        function
                        | Ok x -> x
                        | Error _ -> failwith "Should not happen"
                    )

                return helper.ReturnCreatedEntity(b) |> Ok
            }
        )


module A =

    let define = Define<Ctx, A, string>()
    let resId = define.Id.Simple(fun (A id) -> id)
    let resDef = define.Resource("a", resId).CollectionName("as")

    let attrA =
        define.Attribute
            .SimpleString()
            .Get(fun _ -> "")
            .Set(fun _ a -> a)
            .AddConstraint("foo", (fun _ _ -> ""))

    let attrB = define.Attribute.Nullable.SimpleString()

    let attrC = define.Attribute.Nullable.SimpleString()

    let attrD = define.Attribute.Nullable.SimpleString()

    let attrCAndDSetter = define.Operation.Set2((fun _ _ a -> a), attrC, attrD)

    let relB =
        define.Relationship
            .ToOne(B.resDef)
            .Get(fun _ -> B "1")
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relBNone =
        define.Relationship
            .ToOneNullable(B.resDef)
            .Get(fun _ -> None)
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relBEmpty =
        define.Relationship
            .ToMany(B.resDef)
            .Get(fun _ -> [])
            .SetAll(fun _ a -> a)
            .Add(fun _ a -> a)
            .Remove(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relC = define.Relationship.ToOne(C.resDef).Get(fun _ -> C "1")

    let relD = define.Relationship.ToOne(D.resDef).Get(fun _ -> D "1")

    let relCDToOne =
        define.Relationship
            .Polymorphic()
            .AddIdParser(C.resDef)
            .AddIdParser(D.resDef)
            .ResolveEntity(
                function
                | CDC c -> C.resDef.PolymorphicFor(c)
                | CDD d -> D.resDef.PolymorphicFor(d)
            )
            .ToOne()
            .Get(fun _ -> CDC(C "1"))
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relCDToOneNullable =
        define.Relationship
            .Polymorphic()
            .AddIdParser(C.resDef)
            .AddIdParser(D.resDef)
            .ResolveEntity(
                function
                | CDC c -> C.resDef.PolymorphicFor(c)
                | CDD d -> D.resDef.PolymorphicFor(d)
            )
            .ToOneNullable()
            .Get(fun _ -> None)
            .Set(fun _ a -> a)
            .AfterModifySelf(ignore)

    let relCDToMany =
        define.Relationship
            .Polymorphic()
            .AddIdParser(C.resDef)
            .AddIdParser(D.resDef)
            .ResolveEntity(
                function
                | CDC c -> C.resDef.PolymorphicFor(c)
                | CDD d -> D.resDef.PolymorphicFor(d)
            )
            .ToMany()
            .Get(fun _ -> [])
            .SetAll(fun _ a -> a)
            .Add(fun _ a -> a)
            .Remove(fun _ a -> a)
            .AfterModifySelf(ignore)

    let lookup = define.Operation.Lookup(A >> Some)

    let getColl = define.Operation.GetCollection(fun () -> [])

    let post = define.Operation.Post(fun () -> A "1").AfterCreate(ignore)

    let get = define.Operation.GetResource()

    let patch =
        define.Operation
            .Patch()
            .AddCustomSetter(fun ctx a parser -> parser.For((fun _ -> a), attrB.Optional))
            .AfterUpdate(ignore)

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .GetAsync(fun _ _ respond _ -> respond.WithEntity(B.resDef, B "1") |> Ok |> async.Return)
            .PostAsync(fun _ _ respond _ -> respond.WithOptEntity(B.resDef, None) |> Ok |> async.Return)
            .PatchAsync(fun _ _ respond _ -> respond.WithEntities(B.resDef, []) |> Ok |> async.Return)
            .DeleteAsync(fun _ _ respond _ ->
                respond
                    .RegisterResourceType(resDef)
                    .RegisterResourceType(B.resDef)
                    .WithPolymorphicEntities([])
                |> Ok
                |> async.Return
            )



module CD =

    let define = Define<Ctx, CD, string>()

    let resId =
        define.Id.Simple(
            function
            | CDC(C id) -> id
            | CDD(D id) -> id
        )

    let resDef = define.PolymorphicResource(resId).CollectionName("cds")

    let lookup =
        define.Operation.Polymorphic.Lookup(
            (fun id ->
                if id.StartsWith("C", StringComparison.OrdinalIgnoreCase) then
                    Some(CDC(C id))
                elif id.StartsWith("D", StringComparison.OrdinalIgnoreCase) then
                    Some(CDD(D id))
                else
                    None
            ),
            function
            | CDC c -> C.resDef.PolymorphicFor(c)
            | CDD d -> D.resDef.PolymorphicFor(d)
        )

    let getColl =
        define.Operation.Polymorphic.GetCollection(
            (fun () -> []),
            function
            | CDC c -> C.resDef.PolymorphicFor(c)
            | CDD d -> D.resDef.PolymorphicFor(d)
        )



module E =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("e", resId).CollectionName("es")

    let a = define.Attribute.SimpleString().Get(fun _ -> "").Set(fun _ e -> e)

    let r = define.Relationship.ToOneNullable(resDef)

    let postBackRef =
        define.Operation.PostBackRef(r, (fun (_, _) -> "1")).AfterCreate(ignore)

    let lookup = define.Operation.Lookup(Some)

    let get = define.Operation.GetResource()

    let patch = define.Operation.Patch().Return202Accepted().AfterUpdate(ignore)



module F =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("f", resId).CollectionName("fs")

    let a = define.Attribute.SimpleString().Get(fun _ -> "").Set(fun _ f -> f)

    let r = define.Relationship.ToOneNullable(resDef)

    let postBackRefParser =
        define.Operation
            .PostBackRef(r, (fun (_, _) parser -> parser.For("1")))
            .AfterCreate(ignore)



module G =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("g", resId).CollectionName("ghs")

    let r =
        define.Relationship
            .Polymorphic()
            .AddIdParser(resDef)
            .AddIdParser(C.resDef)
            .ResolveEntity(fun _ -> resDef.PolymorphicFor(""))
            .ToOne()
            .Get(fun _ -> "")

    let post = define.Operation.Post(fun () -> "G1").AfterCreate(ignore)

    let get = define.Operation.GetResource()

    let patch = define.Operation.Patch().AfterUpdate(ignore)

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .GetAsync(fun _ _ respond _ -> respond.WithEntity(resDef, "G1") |> Ok |> async.Return)



module H =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(id)
    let resDef = define.Resource("h", resId).CollectionName("ghs")

    let r =
        define.Relationship
            .Polymorphic()
            .AddIdParser(resDef)
            .AddIdParser(D.resDef)
            .ResolveEntity(fun _ -> resDef.PolymorphicFor(""))
            .ToOne()
            .Get(fun _ -> "")

    let post = define.Operation.Post(fun () -> "H1").AfterCreate(ignore)

    let get = define.Operation.GetResource()

    let patch = define.Operation.Patch().AfterUpdate(ignore)

    let customOp =
        define.Operation
            .CustomLink()
            .ValidateStrictModeQueryParams()
            .GetAsync(fun _ _ respond _ -> respond.WithEntity(resDef, "H1") |> Ok |> async.Return)


type GH =
    | G of string
    | H of string


module GH =

    let define = Define<Ctx, GH, string>()

    let resId =
        define.Id.Simple(
            function
            | G g -> g
            | H h -> h
        )

    let resDef = define.PolymorphicResource(resId).CollectionName("ghs")

    let lookup =
        define.Operation.Polymorphic.Lookup(
            (fun id ->
                if id.StartsWith("G", StringComparison.OrdinalIgnoreCase) then
                    Some(G id)
                elif id.StartsWith("H", StringComparison.OrdinalIgnoreCase) then
                    Some(H id)
                else
                    None
            ),
            function
            | G g -> G.resDef.PolymorphicFor(g)
            | H h -> H.resDef.PolymorphicFor(h)
        )

    let getColl =
        define.Operation.Polymorphic.GetCollection((fun () -> []), (fun _ -> G.resDef.PolymorphicFor("")))


module GHAlt =

    let define = Define<Ctx, GH, string>()

    let resId =
        define.Id.Simple(
            function
            | G g -> g
            | H h -> h
        )

    let resDef = define.PolymorphicResource(resId).CollectionName("ghsAlt")

    let getColl =
        define.Operation.Polymorphic
            .GetCollection((fun () -> []), (fun _ -> G.resDef.PolymorphicFor("")))
            .RegisterResourceType(G.resDef)
            .RegisterResourceType(H.resDef)


module X =

    let define = Define<Ctx, string, string>()
    let resId = define.Id.Simple(fun _ -> "")
    let resDef = define.Resource("x", resId).CollectionName("xs")

    let nonNullableRequiresExplicitInclude =
        define.Attribute.SimpleString().Get(fun _ -> "").RequireExplicitInclude()

    let nonNullableRequiresExplicitIncludeTrue =
        define.Attribute.SimpleString().Get(fun _ -> "").RequireExplicitInclude(true)

    let nonNullableRequiresExplicitIncludeFalse =
        define.Attribute.SimpleString().Get(fun _ -> "").RequireExplicitInclude(false)

    let nullableRequiresExplicitInclude =
        define.Attribute.Nullable
            .SimpleString()
            .Get(fun _ -> None)
            .RequireExplicitInclude()

    let nullableRequiresExplicitIncludeTrue =
        define.Attribute.Nullable
            .SimpleString()
            .Get(fun _ -> None)
            .RequireExplicitInclude(true)

    let nullableRequiresExplicitIncludeFalse =
        define.Attribute.Nullable
            .SimpleString()
            .Get(fun _ -> None)
            .RequireExplicitInclude(false)

    let getColl = define.Operation.GetCollection(fun () -> [])



let createServerAndGetClient (trackFieldUsage: _ -> _ -> _ -> HttpHandler) =
    let server =
        new TestServer(
            WebHostBuilder()
                .ConfigureServices(fun services ->
                    services
                        .AddGiraffe()
                        .AddRouting()
                        .AddJsonApi()
                        .GetCtx(fun _ -> Ctx)
                        .TrackFieldUsage(trackFieldUsage)
                        .EnableUnknownFieldStrictMode()
                        .EnableUnknownQueryParamStrictMode()
                        .Add()
                    |> ignore
                )
                .Configure(fun app -> app.UseRouting().UseJsonApiEndpoints<Ctx>() |> ignore)
        )

    server.CreateClient()


let doTest method url body expected =
    job {
        let mutable actual = Unchecked.defaultof<_>

        let trackFieldUsage _ _ctx (xs: FieldUseInfo list) =
            actual <- xs |> List.map (fun x -> (x.TypeName, x.FieldName), x.Usage)
            setHttpHeader "Foo" "Bar"

        let client = createServerAndGetClient trackFieldUsage

        let! response =
            Request.createWithClient client method (Uri(url))
            |> Request.jsonApiHeaders
            |> match body with
               | None -> id
               | Some body -> Request.bodySerialized body
            |> getResponse

        response |> testSuccessStatusCode

        Expect.equal response.headers[NonStandard "Foo"] "Bar" ""

        Expect.sequenceEqual (List.sort actual) (expected |> List.sort) ""
    }


[<Tests>]
let tests =
    testList "Field usage tracker" [

        let basicTestFieldsAndIncludesForPrimaryA =
            "include=relB,relBNone&fields[a]=attrA,relB,constraints"

        let basicTestExpectedResultForPrimaryA = [
            ("a", "attrA"), FieldUsage.Explicit
            ("a", "relCDToOne"), FieldUsage.Excluded
            ("a", "relCDToOneNullable"), FieldUsage.Excluded
            ("a", "relCDToMany"), FieldUsage.Excluded
            ("a", "relB"), FieldUsage.Explicit
            ("a", "relBNone"), FieldUsage.Explicit
            ("a", "relBEmpty"), FieldUsage.Excluded
            ("a", "relC"), FieldUsage.Excluded
            ("a", "relD"), FieldUsage.Excluded
            ("a", "constraints"), FieldUsage.Explicit
            ("b", "a"), FieldUsage.Implicit
            ("b", "b"), FieldUsage.Implicit
            ("b", "relC"), FieldUsage.Implicit
        ]

        let basicTestFieldsAndIncludesForPrimaryB = "include=relC&fields[b]=a"

        let basicTestExpectedResultForPrimaryB = [
            ("b", "a"), FieldUsage.Explicit
            ("b", "b"), FieldUsage.Excluded
            ("b", "relC"), FieldUsage.Explicit
            ("c", "c"), FieldUsage.Implicit
            ("c", "relToOne"), FieldUsage.Implicit
            ("c", "relToOneNullable"), FieldUsage.Implicit
            ("c", "relToMany"), FieldUsage.Implicit
        ]

        testJob "GET collection: Basic test" {
            do!
                doTest
                    Get
                    ("http://example.com/as?" + basicTestFieldsAndIncludesForPrimaryA)
                    None
                    basicTestExpectedResultForPrimaryA
        }

        testJob "GET polymorphic collection: Tracks fields and includes from all possible types" {
            do!
                doTest Get "http://example.com/cds?include=relToOneNullable&fields[c]=c,relToOne" None [
                    ("c", "c"), FieldUsage.Explicit
                    ("c", "relToOne"), FieldUsage.Explicit
                    ("c", "relToOneNullable"), FieldUsage.Explicit
                    ("c", "relToMany"), FieldUsage.Excluded
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Explicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }


        testJob
            "GET polymorphic collection when including polymorphic relationship: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/ghs?include=r" None [
                    ("g", "r"), FieldUsage.Explicit
                    ("h", "r"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Implicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }


        testJob
            "GET polymorphic collection when resources belong to another collection: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/ghsAlt" None [
                    ("g", "r"), FieldUsage.Implicit
                    ("h", "r"), FieldUsage.Implicit
                ]
        }

        testJob "POST collection: Basic test" {
            do!
                doTest
                    Post
                    ("http://example.com/as?" + basicTestFieldsAndIncludesForPrimaryA)
                    (Some {| data = {| ``type`` = "a" |} |})
                    basicTestExpectedResultForPrimaryA
        }

        testJob "POST collection: Tracks request fields" {
            do!
                doTest
                    Post
                    "http://example.com/as"
                    (Some {|
                        data = {|
                            ``type`` = "a"
                            attributes = {| attrA = "" |}
                        |}
                    |})
                    [
                        ("a", "attrA"), FieldUsage.Explicit
                        ("a", "relCDToOne"), FieldUsage.Implicit
                        ("a", "relCDToOneNullable"), FieldUsage.Implicit
                        ("a", "relCDToMany"), FieldUsage.Implicit
                        ("a", "relB"), FieldUsage.Implicit
                        ("a", "relBNone"), FieldUsage.Implicit
                        ("a", "relBEmpty"), FieldUsage.Implicit
                        ("a", "relC"), FieldUsage.Implicit
                        ("a", "relD"), FieldUsage.Implicit
                        ("a", "constraints"), FieldUsage.Implicit
                    ]
        }

        testJob "POST collection 202: Tracks only request fields" {
            do!
                doTest
                    Post
                    "http://example.com/cds"
                    (Some {|
                        data = {|
                            ``type`` = "c"
                            relationships = {|
                                relToOneNullable = {| data = null |}
                            |}
                        |}
                    |})
                    [ ("c", "relToOneNullable"), FieldUsage.Explicit ]
        }

        testJob "POST collection custom: Tracks request fields" {
            do!
                doTest
                    Post
                    "http://example.com/bs"
                    (Some {|
                        data = {|
                            ``type`` = "b"
                            attributes = {| a = ""; b = "" |}
                        |}
                    |})
                    [
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Explicit
                        ("b", "relC"), FieldUsage.Implicit
                    ]
        }

        testJob "POST collection custom 202: Tracks only request fields, including ID" {
            do!
                doTest
                    Post
                    "http://example.com/cds"
                    (Some {|
                        data = {|
                            ``type`` = "d"
                            id = "1"
                            attributes = {| c = "" |}
                        |}
                    |})
                    [ ("d", "id"), FieldUsage.Explicit; ("d", "c"), FieldUsage.Explicit ]
        }

        testJob "POST collection backRef: Tracks backreference" {
            do!
                doTest
                    Post
                    "http://example.com/es"
                    (Some {|
                        data = {|
                            ``type`` = "e"
                            relationships = {| r = {| data = null |} |}
                        |}
                    |})
                    [ ("e", "a"), FieldUsage.Implicit; ("e", "r"), FieldUsage.Explicit ]
        }

        testJob "POST collection backRef parser overload: Tracks backreference" {
            do!
                doTest
                    Post
                    "http://example.com/fs"
                    (Some {|
                        data = {|
                            ``type`` = "f"
                            relationships = {| r = {| data = null |} |}
                        |}
                    |})
                    [ ("f", "a"), FieldUsage.Implicit; ("f", "r"), FieldUsage.Explicit ]
        }

        testJob "POST to polymorphic collection: Only tracks the requested type" {
            do!
                doTest Post "http://example.com/ghs" (Some {| data = {| ``type`` = "g" |} |}) [
                    ("g", "r"), FieldUsage.Implicit
                ]
        }

        testJob "GET resource: Basic test" {
            do!
                doTest
                    Get
                    ("http://example.com/as/1?" + basicTestFieldsAndIncludesForPrimaryA)
                    None
                    basicTestExpectedResultForPrimaryA
        }

        testJob "GET resource in polymorphic collection: Only tracks the requested type" {
            do! doTest Get "http://example.com/ghs/G1" None [ ("g", "r"), FieldUsage.Implicit ]
        }

        testJob "PATCH resource: Basic test" {
            do!
                doTest
                    Patch
                    ("http://example.com/as/1?" + basicTestFieldsAndIncludesForPrimaryA)
                    (Some {|
                        data = {| ``type`` = "a"; id = "1" |}
                    |})
                    basicTestExpectedResultForPrimaryA
        }

        testJob "PATCH resource: Tracks request fields, including custom setters and Set2" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1"
                    (Some {|
                        data = {|
                            ``type`` = "a"
                            id = "1"
                            attributes = {|
                                attrA = ""
                                attrB = ""
                                attrC = ""
                                attrD = ""
                            |}
                            relationships = {| relBNone = {| data = null |} |}
                        |}
                    |})
                    [
                        ("a", "attrA"), FieldUsage.Explicit
                        ("a", "attrB"), FieldUsage.Explicit
                        ("a", "attrC"), FieldUsage.Explicit
                        ("a", "attrD"), FieldUsage.Explicit
                        ("a", "relCDToOne"), FieldUsage.Implicit
                        ("a", "relCDToOneNullable"), FieldUsage.Implicit
                        ("a", "relCDToMany"), FieldUsage.Implicit
                        ("a", "relB"), FieldUsage.Implicit
                        ("a", "relBNone"), FieldUsage.Explicit
                        ("a", "relBEmpty"), FieldUsage.Implicit
                        ("a", "relC"), FieldUsage.Implicit
                        ("a", "relD"), FieldUsage.Implicit
                        ("a", "constraints"), FieldUsage.Implicit
                    ]
        }

        testJob "PATCH resource 202: Tracks only request fields" {
            do!
                doTest
                    Patch
                    "http://example.com/es/1"
                    (Some {|
                        data = {|
                            ``type`` = "e"
                            id = "1"
                            attributes = {| a = "" |}
                        |}
                    |})
                    [ ("e", "a"), FieldUsage.Explicit ]
        }

        testJob "PATCH resource in polymorphic collection: Only tracks the requested type" {
            do!
                doTest
                    Patch
                    "http://example.com/ghs/G1"
                    (Some {|
                        data = {| ``type`` = "g"; id = "G1" |}
                    |})
                    [ ("g", "r"), FieldUsage.Implicit ]
        }

        testJob "Custom operation with WithEntity: Basic test" {
            do!
                doTest
                    Get
                    ("http://example.com/as/1/customOp?" + basicTestFieldsAndIncludesForPrimaryB)
                    None
                    basicTestExpectedResultForPrimaryB
        }

        testJob "Custom operation with WithOptEntity: Basic test" {
            do!
                doTest
                    Post
                    ("http://example.com/as/1/customOp?" + basicTestFieldsAndIncludesForPrimaryB)
                    None
                    basicTestExpectedResultForPrimaryB
        }

        testJob "Custom operation with WithEntities: Basic test" {
            do!
                doTest
                    Patch
                    ("http://example.com/as/1/customOp?" + basicTestFieldsAndIncludesForPrimaryB)
                    None
                    basicTestExpectedResultForPrimaryB
        }

        testJob "Custom operation with WithPolymorphicEntities: Basic test" {
            do!
                doTest
                    Delete
                    "http://example.com/as/1/customOp?include=relB,relBNone,relC&fields[a]=attrA,relB,constraints&fields[c]="
                    None
                    [
                        ("a", "attrA"), FieldUsage.Explicit
                        ("a", "relCDToOne"), FieldUsage.Excluded
                        ("a", "relCDToOneNullable"), FieldUsage.Excluded
                        ("a", "relCDToMany"), FieldUsage.Excluded
                        ("a", "relB"), FieldUsage.Explicit
                        ("a", "relBNone"), FieldUsage.Explicit
                        ("a", "relBEmpty"), FieldUsage.Excluded
                        ("a", "relC"), FieldUsage.Explicit
                        ("a", "relD"), FieldUsage.Excluded
                        ("a", "constraints"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Implicit
                        ("b", "b"), FieldUsage.Implicit
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Excluded
                        ("c", "relToOne"), FieldUsage.Excluded
                        ("c", "relToOneNullable"), FieldUsage.Excluded
                        ("c", "relToMany"), FieldUsage.Excluded
                    ]
        }

        testJob "Custom operation in polymorphic collection: Only tracks the requested type" {
            do! doTest Get "http://example.com/ghs/G1/customOp" None [ ("g", "r"), FieldUsage.Implicit ]
        }

        testJob "GET to-one relationship related: Basic test" {
            do!
                doTest
                    Get
                    ("http://example.com/as/1/relB?" + basicTestFieldsAndIncludesForPrimaryB)
                    None
                    ([ ("a", "relB"), FieldUsage.Explicit ] @ basicTestExpectedResultForPrimaryB)
        }

        testJob "GET to-one polymorphic relationship related: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/as/1/relCDToOne" None [
                    ("a", "relCDToOne"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Implicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-one relationship self: Basic test" {
            do!
                doTest Get "http://example.com/as/1/relationships/relB?include=relB.relC&fields[b]=a" None [
                    ("a", "relB"), FieldUsage.Explicit
                    ("b", "a"), FieldUsage.Explicit
                    ("b", "b"), FieldUsage.Excluded
                    ("b", "relC"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-one relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest Get "http://example.com/as/1/relationships/relB?include=relD,relB.relC&fields[b]=a" None [
                    ("a", "relB"), FieldUsage.Explicit
                    ("b", "a"), FieldUsage.Explicit
                    ("b", "b"), FieldUsage.Excluded
                    ("b", "relC"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-one polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/as/1/relationships/relCDToOne?include=relCDToOne" None [
                    ("a", "relCDToOne"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Implicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob
            "GET to-one relationship related in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Get "http://example.com/cds/C1/relToOne" None [
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Explicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob
            "GET to-one relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Get "http://example.com/cds/C1/relationships/relToOne" None [
                    ("c", "relToOne"), FieldUsage.Explicit
                ]
        }

        testJob "PATCH to-one relationship self: Basic test" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relB?include=relB.relC&fields[b]=a"
                    (Some {|
                        data = {| ``type`` = "b"; id = "1" |}
                    |})
                    [
                        ("a", "relB"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "PATCH to-one relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relB?include=relD,relB.relC&fields[b]=a"
                    (Some {|
                        data = {| ``type`` = "b"; id = "1" |}
                    |})
                    [
                        ("a", "relB"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "PATCH to-one polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relCDToOne?include=relCDToOne"
                    (Some {|
                        data = {| ``type`` = "c"; id = "1" |}
                    |})
                    [
                        ("a", "relCDToOne"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                        ("d", "c"), FieldUsage.Implicit
                        ("d", "relToOne"), FieldUsage.Implicit
                        ("d", "relToOneNullable"), FieldUsage.Implicit
                        ("d", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob
            "PATCH to-one relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest
                    Patch
                    "http://example.com/cds/C1/relationships/relToOne"
                    (Some {|
                        data = {| ``type`` = "c"; id = "1" |}
                    |})
                    [ ("c", "relToOne"), FieldUsage.Explicit ]
        }

        testJob "GET to-one nullable relationship related: Basic test" {
            do!
                doTest
                    Get
                    ("http://example.com/as/1/relBNone?" + basicTestFieldsAndIncludesForPrimaryB)
                    None
                    ([ ("a", "relBNone"), FieldUsage.Explicit ] @ basicTestExpectedResultForPrimaryB)
        }

        testJob
            "GET to-one nullable polymorphic relationship related: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/as/1/relCDToOneNullable" None [
                    ("a", "relCDToOneNullable"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Implicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-one nullable relationship self: Basic test" {
            do!
                doTest Get "http://example.com/as/1/relationships/relBNone?include=relBNone.relC&fields[b]=a" None [
                    ("a", "relBNone"), FieldUsage.Explicit
                    ("b", "a"), FieldUsage.Explicit
                    ("b", "b"), FieldUsage.Excluded
                    ("b", "relC"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-one nullable relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest Get "http://example.com/as/1/relationships/relBNone?include=relD,relBNone.relC&fields[b]=a" None [
                    ("a", "relBNone"), FieldUsage.Explicit
                    ("b", "a"), FieldUsage.Explicit
                    ("b", "b"), FieldUsage.Excluded
                    ("b", "relC"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-one nullable polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/as/1/relationships/relCDToOneNullable?include=relCDToOneNullable" None [
                    ("a", "relCDToOneNullable"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Implicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob
            "GET to-one nullable relationship related in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Get "http://example.com/cds/C1/relToOneNullable" None [
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Explicit
                    ("c", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob
            "GET to-one nullable relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Get "http://example.com/cds/C1/relationships/relToOneNullable" None [
                    ("c", "relToOneNullable"), FieldUsage.Explicit
                ]
        }

        testJob "PATCH to-one nullable relationship self: Basic test" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relBNone?include=relBNone.relC&fields[b]=a"
                    (Some {| data = null |})
                    [
                        ("a", "relBNone"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "PATCH to-one nullable relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relBNone?include=relD,relBNone.relC&fields[b]=a"
                    (Some {| data = null |})
                    [
                        ("a", "relBNone"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob
            "PATCH to-one nullable polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relCDToOneNullable?include=relCDToOneNullable"
                    (Some {| data = null |})
                    [
                        ("a", "relCDToOneNullable"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                        ("d", "c"), FieldUsage.Implicit
                        ("d", "relToOne"), FieldUsage.Implicit
                        ("d", "relToOneNullable"), FieldUsage.Implicit
                        ("d", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob
            "PATCH to-one nullable relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Patch "http://example.com/cds/C1/relationships/relToOneNullable" (Some {| data = null |}) [
                    ("c", "relToOneNullable"), FieldUsage.Explicit
                ]
        }

        testJob "GET to-many relationship related: Basic test" {
            do!
                doTest
                    Get
                    ("http://example.com/as/1/relBEmpty?" + basicTestFieldsAndIncludesForPrimaryB)
                    None
                    ([ ("a", "relBEmpty"), FieldUsage.Explicit ] @ basicTestExpectedResultForPrimaryB)
        }

        testJob "GET to-many polymorphic relationship related: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/as/1/relCDToMany" None [
                    ("a", "relCDToMany"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Implicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-many relationship self: Basic test" {
            do!
                doTest Get "http://example.com/as/1/relationships/relBEmpty?include=relBEmpty.relC&fields[b]=a" None [
                    ("a", "relBEmpty"), FieldUsage.Explicit
                    ("b", "a"), FieldUsage.Explicit
                    ("b", "b"), FieldUsage.Excluded
                    ("b", "relC"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob "GET to-many relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest
                    Get
                    "http://example.com/as/1/relationships/relBEmpty?include=relD,relBEmpty.relC&fields[b]=a"
                    None
                    [
                        ("a", "relBEmpty"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "GET to-many polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest Get "http://example.com/as/1/relationships/relCDToMany?include=relCDToMany" None [
                    ("a", "relCDToMany"), FieldUsage.Explicit
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Implicit
                    ("d", "c"), FieldUsage.Implicit
                    ("d", "relToOne"), FieldUsage.Implicit
                    ("d", "relToOneNullable"), FieldUsage.Implicit
                    ("d", "relToMany"), FieldUsage.Implicit
                ]
        }

        testJob
            "GET to-many relationship related in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Get "http://example.com/cds/C1/relToMany" None [
                    ("c", "c"), FieldUsage.Implicit
                    ("c", "relToOne"), FieldUsage.Implicit
                    ("c", "relToOneNullable"), FieldUsage.Implicit
                    ("c", "relToMany"), FieldUsage.Explicit
                ]
        }

        testJob
            "GET to-many relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Get "http://example.com/cds/C1/relationships/relToMany" None [
                    ("c", "relToMany"), FieldUsage.Explicit
                ]
        }

        testJob "POST to-many relationship self: Basic test" {
            do!
                doTest
                    Post
                    "http://example.com/as/1/relationships/relBEmpty?include=relBEmpty.relC&fields[b]=a"
                    (Some {| data = [||] |})
                    [
                        ("a", "relBEmpty"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "POST to-many relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest
                    Post
                    "http://example.com/as/1/relationships/relBEmpty?include=relD,relBEmpty.relC&fields[b]=a"
                    (Some {| data = [||] |})
                    [
                        ("a", "relBEmpty"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "POST to-many polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest
                    Post
                    "http://example.com/as/1/relationships/relCDToMany?include=relCDToMany"
                    (Some {| data = [||] |})
                    [
                        ("a", "relCDToMany"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                        ("d", "c"), FieldUsage.Implicit
                        ("d", "relToOne"), FieldUsage.Implicit
                        ("d", "relToOneNullable"), FieldUsage.Implicit
                        ("d", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob
            "POST to-many relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Post "http://example.com/cds/C1/relationships/relToMany" (Some {| data = [||] |}) [
                    ("c", "relToMany"), FieldUsage.Explicit
                ]
        }

        testJob "PATCH to-many relationship self: Basic test" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relBEmpty?include=relBEmpty.relC&fields[b]=a"
                    (Some {| data = [||] |})
                    [
                        ("a", "relBEmpty"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "PATCH to-many relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relBEmpty?include=relD,relBEmpty.relC&fields[b]=a"
                    (Some {| data = [||] |})
                    [
                        ("a", "relBEmpty"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "PATCH to-many polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest
                    Patch
                    "http://example.com/as/1/relationships/relCDToMany?include=relCDToMany"
                    (Some {| data = [||] |})
                    [
                        ("a", "relCDToMany"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                        ("d", "c"), FieldUsage.Implicit
                        ("d", "relToOne"), FieldUsage.Implicit
                        ("d", "relToOneNullable"), FieldUsage.Implicit
                        ("d", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob
            "PATCH to-many relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Patch "http://example.com/cds/C1/relationships/relToMany" (Some {| data = [||] |}) [
                    ("c", "relToMany"), FieldUsage.Explicit
                ]
        }

        testJob "DELETE to-many relationship self: Basic test" {
            do!
                doTest
                    Delete
                    "http://example.com/as/1/relationships/relBEmpty?include=relBEmpty.relC&fields[b]=a"
                    (Some {| data = [||] |})
                    [
                        ("a", "relBEmpty"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "DELETE to-many relationship self: Ignores other relationships on the parent resource" {
            do!
                doTest
                    Delete
                    "http://example.com/as/1/relationships/relBEmpty?include=relD,relBEmpty.relC&fields[b]=a"
                    (Some {| data = [||] |})
                    [
                        ("a", "relBEmpty"), FieldUsage.Explicit
                        ("b", "a"), FieldUsage.Explicit
                        ("b", "b"), FieldUsage.Excluded
                        ("b", "relC"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob "DELETE to-many polymorphic relationship self: Tracks correct usage for all related resource types" {
            do!
                doTest
                    Delete
                    "http://example.com/as/1/relationships/relCDToMany?include=relCDToMany"
                    (Some {| data = [||] |})
                    [
                        ("a", "relCDToMany"), FieldUsage.Explicit
                        ("c", "c"), FieldUsage.Implicit
                        ("c", "relToOne"), FieldUsage.Implicit
                        ("c", "relToOneNullable"), FieldUsage.Implicit
                        ("c", "relToMany"), FieldUsage.Implicit
                        ("d", "c"), FieldUsage.Implicit
                        ("d", "relToOne"), FieldUsage.Implicit
                        ("d", "relToOneNullable"), FieldUsage.Implicit
                        ("d", "relToMany"), FieldUsage.Implicit
                    ]
        }

        testJob
            "DELETE to-many relationship self in polymorphic collection: Tracks only the relationship on the requested type" {
            do!
                doTest Delete "http://example.com/cds/C1/relationships/relToMany" (Some {| data = [||] |}) [
                    ("c", "relToMany"), FieldUsage.Explicit
                ]
        }

        testJob "Works across null relationships" {
            do!
                doTest Get "http://example.com/as/1?include=relBNone" None [
                    ("a", "attrA"), FieldUsage.Implicit
                    ("a", "relCDToOne"), FieldUsage.Implicit
                    ("a", "relCDToOneNullable"), FieldUsage.Implicit
                    ("a", "relCDToMany"), FieldUsage.Implicit
                    ("a", "relB"), FieldUsage.Implicit
                    ("a", "relBNone"), FieldUsage.Explicit
                    ("a", "relBEmpty"), FieldUsage.Implicit
                    ("a", "relC"), FieldUsage.Implicit
                    ("a", "relD"), FieldUsage.Implicit
                    ("a", "constraints"), FieldUsage.Implicit
                    ("b", "a"), FieldUsage.Implicit
                    ("b", "b"), FieldUsage.Implicit
                    ("b", "relC"), FieldUsage.Implicit
                ]
        }

        testJob "Works across empty relationships" {
            do!
                doTest Get "http://example.com/as/1?include=relBEmpty" None [
                    ("a", "attrA"), FieldUsage.Implicit
                    ("a", "relCDToOne"), FieldUsage.Implicit
                    ("a", "relCDToOneNullable"), FieldUsage.Implicit
                    ("a", "relCDToMany"), FieldUsage.Implicit
                    ("a", "relB"), FieldUsage.Implicit
                    ("a", "relBNone"), FieldUsage.Implicit
                    ("a", "relBEmpty"), FieldUsage.Explicit
                    ("a", "relC"), FieldUsage.Implicit
                    ("a", "relD"), FieldUsage.Implicit
                    ("a", "constraints"), FieldUsage.Implicit
                    ("b", "a"), FieldUsage.Implicit
                    ("b", "b"), FieldUsage.Implicit
                    ("b", "relC"), FieldUsage.Implicit
                ]
        }

        testJob "Ignores types that don't exist" {
            do!
                doTest Get "http://example.com/as/1?fields[invalidType]=foo" None [
                    ("a", "attrA"), FieldUsage.Implicit
                    ("a", "relCDToOne"), FieldUsage.Implicit
                    ("a", "relCDToOneNullable"), FieldUsage.Implicit
                    ("a", "relCDToMany"), FieldUsage.Implicit
                    ("a", "relB"), FieldUsage.Implicit
                    ("a", "relBNone"), FieldUsage.Implicit
                    ("a", "relBEmpty"), FieldUsage.Implicit
                    ("a", "relC"), FieldUsage.Implicit
                    ("a", "relD"), FieldUsage.Implicit
                    ("a", "constraints"), FieldUsage.Implicit
                ]
        }

        testJob "Ignores fields that don't exist" {
            do!
                doTest Get "http://example.com/as/1?fields[a]=invalidField" None [
                    ("a", "attrA"), FieldUsage.Excluded
                    ("a", "relCDToOne"), FieldUsage.Excluded
                    ("a", "relCDToOneNullable"), FieldUsage.Excluded
                    ("a", "relCDToMany"), FieldUsage.Excluded
                    ("a", "relB"), FieldUsage.Excluded
                    ("a", "relBNone"), FieldUsage.Excluded
                    ("a", "relBEmpty"), FieldUsage.Excluded
                    ("a", "relC"), FieldUsage.Excluded
                    ("a", "relD"), FieldUsage.Excluded
                    ("a", "constraints"), FieldUsage.Excluded
                ]
        }

        testJob "Ignores fields on types that are not included" {
            do!
                doTest Get "http://example.com/as/1?fields[b]=a&fields[c]=c" None [
                    ("a", "attrA"), FieldUsage.Implicit
                    ("a", "relCDToOne"), FieldUsage.Implicit
                    ("a", "relCDToOneNullable"), FieldUsage.Implicit
                    ("a", "relCDToMany"), FieldUsage.Implicit
                    ("a", "relB"), FieldUsage.Implicit
                    ("a", "relBNone"), FieldUsage.Implicit
                    ("a", "relBEmpty"), FieldUsage.Implicit
                    ("a", "relC"), FieldUsage.Implicit
                    ("a", "relD"), FieldUsage.Implicit
                    ("a", "constraints"), FieldUsage.Implicit
                ]
        }

        testJob "Attributes with RequireExplicitInclude are tracked if specified using sparse fieldsets" {
            do!
                doTest
                    Get
                    "http://example.com/xs?fields[x]=nonNullableRequiresExplicitInclude,nonNullableRequiresExplicitIncludeTrue,nonNullableRequiresExplicitIncludeFalse,nullableRequiresExplicitInclude,nullableRequiresExplicitIncludeTrue,nullableRequiresExplicitIncludeFalse"
                    None
                    [
                        ("x", "nonNullableRequiresExplicitInclude"), FieldUsage.Explicit
                        ("x", "nonNullableRequiresExplicitIncludeTrue"), FieldUsage.Explicit
                        ("x", "nonNullableRequiresExplicitIncludeFalse"), FieldUsage.Explicit
                        ("x", "nullableRequiresExplicitInclude"), FieldUsage.Explicit
                        ("x", "nullableRequiresExplicitIncludeTrue"), FieldUsage.Explicit
                        ("x", "nullableRequiresExplicitIncludeFalse"), FieldUsage.Explicit
                    ]
        }

        testJob "Attributes with RequireExplicitInclude are not tracked if not specified using sparse fieldsets" {
            do!
                doTest Get "http://example.com/xs" None [
                    ("x", "nonNullableRequiresExplicitInclude"), FieldUsage.Excluded
                    ("x", "nonNullableRequiresExplicitIncludeTrue"), FieldUsage.Excluded
                    ("x", "nonNullableRequiresExplicitIncludeFalse"), FieldUsage.Implicit
                    ("x", "nullableRequiresExplicitInclude"), FieldUsage.Excluded
                    ("x", "nullableRequiresExplicitIncludeTrue"), FieldUsage.Excluded
                    ("x", "nullableRequiresExplicitIncludeFalse"), FieldUsage.Implicit
                ]
        }

    ]
