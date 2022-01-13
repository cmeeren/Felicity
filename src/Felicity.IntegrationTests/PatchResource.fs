module ``PATCH resource``

open System
open Microsoft.Net.Http.Headers
open Expecto
open HttpFs.Client
open Swensen.Unquote
open Giraffe
open Felicity


type A = {
  Id: string
  ReadOnly: string
  A: bool
  X: string
  Nullable: string option
  NullableNotNullWhenSet: string option
}

type B = {
  Id: string
  ReadOnly: string
  B: int
  Y: string
}

type ABC = A of A | B of B | C of A

module ADomain =

  let setX x a =
    { a with X = x }

  let setNullable x a =
    { a with Nullable = x }

  let setNullableNotNullWhenSet x a =
    { a with NullableNotNullWhenSet = Some x }


module BDomain =

  let setB x b =
    { b with B = x }

  let setY x b =
    { b with Y = x }


type Db () =
  let mutable ABs : Map<string, ABC> =
    Map.empty
    |> Map.add "a1" (A { Id = "a1"; ReadOnly = "qwerty"; A = false; X = ""; Nullable = Some "foo"; NullableNotNullWhenSet = None })
    |> Map.add "b2" (B { Id = "b2"; ReadOnly = "qwerty"; B = 1; Y = "" })
    |> Map.add "c0" (C { Id = "c0"; ReadOnly = "qwerty"; A = false; X = ""; Nullable = None; NullableNotNullWhenSet = None })

  member _.SaveA (a: A) =
    ABs <- ABs.Add (a.Id, A a)

  member _.SaveB (b: B) =
    ABs <- ABs.Add (b.Id, B b)

  member _.TryGet id =
    ABs.TryFind id

  member _.GetAOrFail id =
    ABs.TryFind id |> Option.defaultWith (fun () -> failwithf "Could not find ID %s" id)


type MappedCtx = {
  ModifyAResponse: A -> HttpHandler
  ModifyBResponse: B -> HttpHandler
  Db: Db
  SetA: bool -> A -> Result<A, Error list>
  BeforeUpdateA: A -> Result<A, Error list>
  AfterUpdateA: A -> A -> unit
}


type Ctx = {
  ModifyAResponse: A -> HttpHandler
  ModifyBResponse: B -> HttpHandler
  Db: Db
  SetA: bool -> A -> Result<A, Error list>
  BeforeUpdateA: A -> Result<A, Error list>
  AfterUpdateA: A -> A -> unit
  MapCtx: Ctx -> Result<MappedCtx, Error list>
} with
  static member WithDb db = {
    ModifyAResponse = fun _ -> fun next ctx -> next ctx
    ModifyBResponse = fun _ -> fun next ctx -> next ctx
    Db = db
    SetA = fun x a -> Ok { a with A = x }
    BeforeUpdateA = fun x -> Ok x
    AfterUpdateA = fun _ a -> db.SaveA a
    MapCtx = fun ctx -> Ok {
      ModifyAResponse = ctx.ModifyAResponse
      ModifyBResponse = ctx.ModifyBResponse
      Db = ctx.Db
      SetA = ctx.SetA
      BeforeUpdateA = ctx.BeforeUpdateA
      AfterUpdateA = ctx.AfterUpdateA
    }
  }


module A =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("abs")

  let readonly =
    define.Attribute
      .SimpleString()
      .Get(fun (a: A) -> a.ReadOnly)

  let a =
    define.Attribute
      .SimpleBool()
      .Get(fun a -> a.A)
      .SetRes(fun ctx -> ctx.SetA)

  let aMapped =
    define.Attribute
      .MapSetContextRes(fun ctx -> ctx.MapCtx ctx)
      .SimpleBool()
      .Get(fun a -> a.A)
      .SetRes(fun (ctx: MappedCtx) -> ctx.SetA)

  let x =
    define.Attribute
      .SimpleString()
      .Get(fun a -> a.X)
      .Set(ADomain.setX)
  
  let nullable =
    define.Attribute
      .Nullable
      .SimpleString()
      .Get(fun a -> a.Nullable)
      .Set(ADomain.setNullable)

  let nullableMapped =
    define.Attribute
      .Nullable
      .MapSetContextRes(fun ctx -> ctx.MapCtx ctx)
      .SimpleString()
      .Get(fun a -> a.Nullable)
      .Set(fun (_ctx: MappedCtx) x a -> ADomain.setNullable x a)
  
  let nullableNotNullWhenSet =
    define.Attribute
      .Nullable
      .SimpleString()
      .Get(fun a -> a.NullableNotNullWhenSet)
      .SetNonNull(ADomain.setNullableNotNullWhenSet)

  let get = define.Operation.GetResource()

  let patch =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .Patch()
      .BeforeUpdateRes(fun (ctx: MappedCtx) a -> ctx.BeforeUpdateA a)
      .AfterUpdate(fun (ctx: MappedCtx) a -> ctx.AfterUpdateA a)
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyAResponse)



module B =

  let define = Define<Ctx, B, string>()
  let resId = define.Id.Simple(fun (b: B) -> b.Id)
  let resDef = define.Resource("b", resId).CollectionName("abs")

  let readonly =
    define.Attribute
      .SimpleString()
      .Get(fun b -> b.ReadOnly)

  let b =
    define.Attribute
      .SimpleInt()
      .Get(fun b -> b.B)
      .Set(BDomain.setB)

  let y =
    define.Attribute
      .SimpleString()
      .Get(fun b -> b.Y)
      .Set(BDomain.setY)

  let get = define.Operation.GetResource()

  let patch =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .Patch()
      .AfterUpdate(fun (ctx: MappedCtx) b -> ctx.Db.SaveB b)
      .Return202Accepted()
      .ModifyResponse(fun (ctx: MappedCtx) -> ctx.ModifyBResponse)


module C =

  let define = Define<Ctx, A, string>()
  let resId = define.Id.Simple(fun _ -> failwith "not used")
  let resDef = define.Resource("c", resId).CollectionName("abs")



module AB =

  let define = Define<Ctx, ABC, string>()

  let resId = define.Id.Simple(function A a -> a.Id | B b -> b.Id | C c -> c.Id)

  let resDef =
    define.PolymorphicResource(resId)
      .CollectionName("abs")

  let lookup =
    define.Operation
      .ForContextRes(fun ctx -> ctx.MapCtx ctx)
      .Polymorphic
      .Lookup(
        (fun (ctx: MappedCtx) id -> ctx.Db.TryGet id),
        function
          | A a -> A.resDef.PolymorphicFor a
          | B b -> B.resDef.PolymorphicFor b
          | C c -> C.resDef.PolymorphicFor c
      )


type Ctx2 = Ctx2

module A2 =

  let define = Define<Ctx2, A, string>()
  let resId = define.Id.Simple(function a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("abs")
  let lookup = define.Operation.Lookup(fun _ -> None)



type Ctx3 = Ctx3

module A3 =

  let define = Define<Ctx3, A, string>()
  let resId = define.Id.Simple(fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("abs")


type Ctx4 = Ctx4

module A4 =

  let define = Define<Ctx4, A, string>()
  let resId = define.Id.Simple(fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> Some { Id = "a1"; ReadOnly = ""; A = false; X = ""; Nullable = None; NullableNotNullWhenSet = None })
  let get = define.Operation.GetResource()
  let patch = define.Operation.Patch().AfterUpdate(ignore)
  let preconditions = define.Preconditions.ETag(fun _ -> EntityTagHeaderValue.FromString false "valid-etag")


type Ctx5 = Ctx5

module A5 =

  let define = Define<Ctx5, A, string>()
  let resId = define.Id.Simple(fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> Some { Id = "a1"; ReadOnly = ""; A = false; X = ""; Nullable = None; NullableNotNullWhenSet = None })
  let get = define.Operation.GetResource()
  let patch = define.Operation.Patch().AfterUpdate(ignore)
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, 999, TimeSpan.Zero))  // Should ignore milliseconds


type Ctx6 = Ctx6

module A6 =

  let define = Define<Ctx6, A, string>()
  let resId = define.Id.Simple(fun a -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")
  let lookup = define.Operation.Lookup(fun _ -> Some { Id = "a1"; ReadOnly = ""; A = false; X = ""; Nullable = None; NullableNotNullWhenSet = None })
  let get = define.Operation.GetResource()
  let patch = define.Operation.Patch().AfterUpdate(ignore)
  let preconditions = define.Preconditions.LastModified(fun _ -> DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)).Optional


type Ctx7 = Ctx7 of Db

module A7 =

  let define = Define<Ctx7, A, string>()
  let resId = define.Id.Simple(fun (a: A) -> a.Id)
  let resDef = define.Resource("a", resId).CollectionName("as")

  let readonly =
    define.Attribute
      .SimpleString()
      .Get(fun (a: A) -> a.ReadOnly)

  let x =
    define.Attribute
      .SimpleString()
      .Get(fun a -> a.X)
      .Set(fun _ _ -> failwith<A> "not used")

  let y =
    define.Attribute
      .SimpleString()
      .Get(fun _ -> "test")

  let lookup =
    define.Operation
      .Lookup (fun (Ctx7 db) id -> match db.TryGet id with Some (A a) -> Some a | _ -> None)
  
  let get = define.Operation.GetResource()

  let patch =
    define.Operation
      .Patch()
      .AddCustomSetter(fun ctx a parser ->
        parser
          .For(a)
          .Add((fun x readonly a -> { a with X = defaultArg readonly "DEFAULT" + x }), x, readonly.Optional)
      )
      .AddCustomSetter(fun ctx a parser ->
        parser
          .For(a)
          .Add((fun readonly a -> { a with ReadOnly = readonly + readonly }), readonly)
      )
      .AddCustomSetter(fun ctx a parser ->
        parser
          .For(a)
          .Add((fun _ _ a -> a), y, x)
      )
      .AfterUpdate(fun (Ctx7 db) a -> db.SaveA a)


type D = {
  NonNullable: DateTimeOffset
  Nullable: DateTimeOffset option
}

type Ctx8 = Ctx8 of D ref

module A8 =

  let define = Define<Ctx8, D, string>()
  let resId = define.Id.Simple(fun _a -> "d1")
  let resDef = define.Resource("d", resId).CollectionName("ds")
  let lookup = define.Operation.Lookup(fun (Ctx8 d) _ -> Some !d)

  let nonNullable =
    define.Attribute
      .SimpleDateTimeOffset()
      .Get(fun d -> d.NonNullable)
      .Set(fun v d -> { d with NonNullable = v })

  let nonNullableAllowMissingOffset =
    define.Attribute
      .SimpleDateTimeOffsetAllowMissingOffset()
      .Get(fun d -> d.NonNullable)
      .Set(fun v d -> { d with NonNullable = v })

  let nullable =
    define.Attribute
      .Nullable
      .SimpleDateTimeOffset()
      .Get(fun d -> d.Nullable)
      .Set(fun v (d: D) -> { d with Nullable = v })

  let nullableAllowMissingOffset =
    define.Attribute
      .Nullable
      .SimpleDateTimeOffsetAllowMissingOffset()
      .Get(fun d -> d.Nullable)
      .Set(fun v (d: D) -> { d with Nullable = v })

  let get = define.Operation.GetResource()
  let patch = define.Operation.Patch().AfterUpdate(fun (Ctx8 d) res -> d := res)


[<Tests>]
let tests =
  testList "PATCH resource" [

    testJob "Update A: Returns 200, runs setters and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|a = true
                      x = "abc"
                      nullable = null
                      nullableNotNullWhenSet = "bar"
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "a" @>
      test <@ json |> getPath "data.id" = "a1" @>
      test <@ json |> getPath "data.attributes.readonly" = "qwerty" @>
      test <@ json |> getPath "data.attributes.a" = true @>
      test <@ json |> getPath "data.attributes.x" = "abc" @>
      test <@ json |> getPath "data.attributes.nullable" = null @>
      test <@ json |> getPath "data.attributes.nullableNotNullWhenSet" = "bar" @>
      test <@ json |> getPath "data.links.self" = "http://example.com/abs/a1" @>

      test <@ response.headers[NonStandard "Foo"] = "Bar" @>

      let a = 
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"
      test <@ a.Id = "a1" @>
      test <@ a.ReadOnly = "qwerty" @>
      test <@ a.A = true @>
      test <@ a.X = "abc" @>
      test <@ a.Nullable = None @>
      test <@ a.NullableNotNullWhenSet = Some "bar" @>
    }

    testJob "Insensitive to trailing slashes" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1/"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|a = true
                      x = "abc"
                      nullable = null
                      nullableNotNullWhenSet = "bar"
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
    }

    testJob "Update A mapped: Succeeds if mapping succeeds" {
      let db = Db ()
      let ctx = Ctx.WithDb db
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {|data = {|``type`` = "a"; id = "a1"; attributes = {| aMapped = true |} |} |}
        |> getResponse

      response |> testSuccessStatusCode

      let a = 
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"

      test <@ a.A = true @>
    }

    testJob "Update A mapped: Returns errors returned by mapSetCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "a1"; attributes = {| aMapped = true |} |} |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Update nullable mapped: Succeeds if mapping succeeds" {
      let db = Db ()
      let ctx = Ctx.WithDb db
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {|data = {|``type`` = "a"; id = "a1"; attributes = {| nullableMapped = "foobar" |} |} |}
        |> getResponse

      response |> testSuccessStatusCode

      let a = 
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"

      test <@ a.Nullable = Some "foobar" @>
    }

    testJob "Update nullable mapped: Returns errors returned by mapSetCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "a1"; attributes = {| nullableMapped = "foobar" |} |} |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Update B: Returns 202, runs setters and returns correct data if successful" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyBResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/b2"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "b"
                  id = "b2"
                  attributes =
                    {|b = 2
                      y = "abc"
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 202
      let! json = response |> Response.readBodyAsString
      test <@ json = "" @>

      test <@ response.headers[NonStandard "Foo"] = "Bar" @>

      let b =
        match db.GetAOrFail "b2" with
        | B b -> b
        | _ -> failwith "Invalid type"
      test <@ b.Id = "b2" @>
      test <@ b.ReadOnly = "qwerty" @>
      test <@ b.B = 2 @>
      test <@ b.Y = "abc" @>
    }

    testJob "Runs custom setter and does not run normal setters for consumed fields" {
      let db = Db ()
      let ctx = Ctx7 db
      let! response =
        Request.patch ctx "/as/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|readonly = "foo"
                      x = "bar"
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "a" @>
      test <@ json |> getPath "data.id" = "a1" @>
      test <@ json |> getPath "data.attributes.readonly" = "foofoo" @>
      test <@ json |> getPath "data.attributes.x" = "foobar" @>

      let a =
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"
      test <@ a.Id = "a1" @>
      test <@ a.ReadOnly = "foofoo" @>
      test <@ a.X = "foobar" @>
    }

    testJob "Correctly handles optional values in custom setter" {
      let db = Db ()
      let ctx = Ctx7 db
      let! response =
        Request.patch ctx "/as/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|x = "bar"
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.type" = "a" @>
      test <@ json |> getPath "data.id" = "a1" @>
      test <@ json |> getPath "data.attributes.readonly" = "qwerty" @>
      test <@ json |> getPath "data.attributes.x" = "DEFAULTbar" @>

      let a =
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"
      test <@ a.Id = "a1" @>
      test <@ a.ReadOnly = "qwerty" @>
      test <@ a.X = "DEFAULTbar" @>
    }

    testJob "Returns 400 if required parameter is missing from custom setter" {
      let db = Db ()
      let ctx = Ctx7 db
      let! response =
        Request.patch ctx "/as/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|y = "foo"
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Attribute 'x' is required for this operation" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when using stringified numbers" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyBResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/b2"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "b"
                  id = "b2"
                  attributes = {| b = "2" |}
                |}
            |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Invalid JSON for attribute 'b': Cannot get the value of a token type 'String' as a number." @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/b" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Calls AfterUpdate with the initial and new entity" {
      let db = Db ()
      let aOrig =
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"

      let aExpected = { Id = "a1"; ReadOnly = "qwerty"; A = true; X = "abc"; Nullable = None; NullableNotNullWhenSet = Some "bar" }

      let mutable called = false
      let afterUpdate before after =
        called <- true
        test <@ before = aOrig @>
        test <@ after = aExpected @>

      let ctx = { Ctx.WithDb db with AfterUpdateA = afterUpdate }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|a = true
                      x = "abc"
                      nullable = null
                      nullableNotNullWhenSet = "bar"
                    |}
                |}
            |}
        |> getResponse

      response |> testSuccessStatusCode

      let called' = called
      test <@ called' = true @>

    }

    testJob "Correctly handles precondition validation using ETag" {
      let! response =
        Request.patch Ctx4 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Match header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx4 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
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
        Request.patch Ctx4 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> Request.setHeader (IfMatch "\"valid-etag\"")
        |> getResponse
      test <@ response.headers[ETag] <> "\"valid-etag\"" @>
      response |> testStatusCode 200
    }

    testJob "Correctly handles precondition validation using If-Unmodified-Since" {
      let! response =
        Request.patch Ctx5 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> getResponse
      response |> testStatusCode 428
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "428" @>
      test <@ json |> getPath "errors[0].detail" = "This operation requires a precondition to be specified using the If-Unmodified-Since header" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx5 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx5 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "Correctly handles optional precondition validation" {
      let! response =
        Request.patch Ctx6 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> getResponse
      response |> testStatusCode 200

      let! response =
        Request.patch Ctx6 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Fri, 31 Dec 1999 23:59:59 GMT"))
        |> getResponse
      response |> testStatusCode 412
      test <@ response.headers.ContainsKey LastModified = false @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "412" @>
      test <@ json |> getPath "errors[0].detail" = "The precondition specified in the If-Unmodified-Since header failed" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let! response =
        Request.patch Ctx6 "/as/a1"
        |> Request.bodySerialized {| data = {|``type`` = "a"; id = "a1" |} |}
        |> Request.setHeader (Custom ("If-Unmodified-Since", "Sat, 01 Jan 2000 00:00:00 GMT"))
        |> getResponse
      test <@ response.headers.ContainsKey LastModified = false @>
      response |> testStatusCode 200
    }

    testJob "Uses entity returned by BeforeUpdate" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with BeforeUpdateA = fun a -> Ok { a with Nullable = Some "lorem ipsum" } }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes = {| x = "abc" |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.attributes.nullable" = "lorem ipsum" @>

      let a = 
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"
      test <@ a.Nullable = Some "lorem ipsum" @>
    }

    testJob "Returns errors and does not call AfterUpdate if BeforeUpdate returns an error" {
      let db = Db ()
      let ctx = {
        Ctx.WithDb db with
          BeforeUpdateA = fun _ -> Error [Error.create 422 |> Error.setCode "custom"]
          AfterUpdateA = fun _ _ -> failwith "Should not be called"
      }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|a = true
                      x = "abc"
                    |}
                |}
            |}
        |> getResponse

      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors and does not call AfterUpdate if a setter returns an error" {
      let db = Db ()
      let aOrig =
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"

      let ctx = { Ctx.WithDb db with SetA = fun _ _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|a = true
                      x = "abc"
                    |}
                |}
            |}
        |> getResponse

      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/a" @>
      test <@ json |> hasNoPath "errors[1]" @>

      let a =
        match db.GetAOrFail "a1" with
        | A a -> a
        | _ -> failwith "Invalid type"
      test <@ a = aOrig @>
    }

    testJob "Returns 403 when read-only" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes = {| readonly = "foo" |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Attribute 'readonly' is read-only" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/readonly" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if nullable attribute is set to null when not supported" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes = {| nullableNotNullWhenSet = null |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Attribute 'nullableNotNullWhenSet' may not be set to null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullableNotNullWhenSet" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when missing body" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Request to update resources must contain a single resource object as primary data" @>
      test <@ json |> getPath "errors[0].source.pointer" = "" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when JSON is invalid" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/abs/a1"
        |> Request.bodyString """
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
      test <@ json |> getPath "errors[0].detail" = "'\"' is invalid after a property name. Expected a ':'. Path: $.test | LineNumber: 3 | BytePositionInLine: 19." @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when missing data" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {| meta = obj() |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Missing required member 'data'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when data is null" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {| data = null |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Request to update resources must contain a single resource object as primary data" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when missing type" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|id="a1"
                  attributes = {| a = true |} |}
            |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Missing required member 'type'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when type is null" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = null
                  id="a1"
                  attributes = {| a = true |}
                |}
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
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {|data = {| ``type`` = "a" |} |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Request to update resources must contain a resource ID" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when ID is null" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = null
                |}
            |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'id' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when attributes is null" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes = null
                |}
            |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'attributes' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when relationships is null" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  relationships = null
                |}
            |}
        |> getResponse

      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Member 'relationships' may not be null" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/relationships" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Ignores unknown members and relationships" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized
            {|data =
                {|``type`` = "a"
                  id = "a1"
                  attributes =
                    {|a = true
                      nonExistentAttribute = "foo"
                    |}
                  relationships =
                    {|nonExistentRelationship =
                        {|data = null
                        |}
                    |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 200
    }

    testJob "Returns 400 when setting a non-nullable DateTimeOffset attribute without offset" {
      let d = {
        NonNullable = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
        Nullable = None
      }
      let ctx = Ctx8 (ref d)
      let! response =
        Request.patch ctx "/ds/d1"
        |> Request.bodySerialized
            {| data =
                {| ``type`` = "d"
                   id = "d1"
                   attributes = {| nonNullable = "2000-01-01T15:49:23" |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Received invalid value for attribute 'nonNullable': Missing offset (e.g. 'Z' or '+01:00')" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nonNullable" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 400 when setting a nullable DateTimeOffset attribute without offset" {
      let d = {
        NonNullable = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
        Nullable = None
      }
      let ctx = Ctx8 (ref d)
      let! response =
        Request.patch ctx "/ds/d1"
        |> Request.bodySerialized
            {| data =
                {| ``type`` = "d"
                   id = "d1"
                   attributes = {| nullable = "2000-01-01T15:49:23" |}
                |}
            |}
        |> getResponse
      response |> testStatusCode 400
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "400" @>
      test <@ json |> getPath "errors[0].detail" = "Received invalid value for attribute 'nullable': Missing offset (e.g. 'Z' or '+01:00')" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/attributes/nullable" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Can set DateTimeOffset attributes with offset" {
      let d = {
        NonNullable = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
        Nullable = None
      }
      let dRef = ref d
      let ctx = Ctx8 dRef
      let! response =
        Request.patch ctx "/ds/d1"
        |> Request.bodySerialized
            {| data =
                {| ``type`` = "d"
                   id = "d1"
                   attributes =
                    {|
                      nonNullable = "2000-01-01T15:49:23Z"
                      nullable = "2000-01-01T15:49:23+05:00"
                    |}
                |}
            |}
        |> getResponse
      response |> testSuccessStatusCode
      test <@ (!dRef).NonNullable = DateTimeOffset(2000, 1, 1, 15, 49, 23, TimeSpan.Zero) @>
      test <@ (!dRef).Nullable = Some (DateTimeOffset(2000, 1, 1, 15, 49, 23, TimeSpan.FromHours 5.)) @>
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "data.attributes.nonNullable" = DateTimeOffset(2000, 1, 1, 15, 49, 23, TimeSpan.Zero) @>
      test <@ json |> getPath "data.attributes.nullable" = DateTimeOffset(2000, 1, 1, 15, 49, 23, TimeSpan.FromHours 5.) @>
    }

    testJob "Can set DateTimeOffset attributes without offset if allowed" {
      let d = {
        NonNullable = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.Zero)
        Nullable = None
      }
      let dRef = ref d
      let ctx = Ctx8 dRef
      let! response =
        Request.patch ctx "/ds/d1"
        |> Request.bodySerialized
            {| data =
                {| ``type`` = "d"
                   id = "d1"
                   attributes =
                    {|
                      nonNullableAllowMissingOffset = "2000-01-01T15:49:23"
                      nullableAllowMissingOffset = "2000-01-01T15:49:23"
                    |}
                |}
            |}
        |> getResponse
      response |> testSuccessStatusCode
    }

    testJob "Returns 409 when type is incorrect" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {|data = {| ``type`` = "invalid"; id = "a1" |} |}
        |> getResponse
      response |> testStatusCode 409
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "409" @>
      test <@ json |> getPath "errors[0].detail" = "Expected resource type 'a', but got 'invalid'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/type" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 409 when ID is incorrect" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with ModifyAResponse = fun _ -> setHttpHeader "Foo" "Bar" }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {|data = {| ``type`` = "a"; id = "a2" |} |}
        |> getResponse
      response |> testStatusCode 409
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "409" @>
      test <@ json |> getPath "errors[0].detail" = "Expected resource ID 'a1', but got 'a2'" @>
      test <@ json |> getPath "errors[0].source.pointer" = "/data/id" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 404 when resource is not found" {
      let db = Db ()
      let! response =
        Request.patch (Ctx.WithDb db) "/abs/a2"
        |> Request.bodySerialized {|data = {| ``type`` = "a"; id = "a2" |} |}
        |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not contain a resource with ID 'a2'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns errors returned by mapCtx" {
      let db = Db ()
      let ctx = { Ctx.WithDb db with MapCtx = fun _ -> Error [Error.create 422 |> Error.setCode "custom"] }
      let! response =
        Request.patch ctx "/abs/a1"
        |> Request.bodySerialized {| data = {| ``type`` = "a"; id = "a1" |} |}
        |> getResponse
      response |> testStatusCode 422
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "422" @>
      test <@ json |> getPath "errors[0].code" = "custom" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported by resource" {
      let db = Db()
      let! response =
        Request.patch (Ctx.WithDb db) "/abs/c0"
        |> Request.bodySerialized {|data = {| ``type`` = "c"; id = "c0" |} |}
        |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "PATCH is not supported for resource type 'c' (it may be supported for other resource types in collection 'abs')" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if not supported at all" {
      let! response =
        Request.patch Ctx2 "/abs/a1"
        |> Request.bodySerialized {|data = {| ``type`` = "a"; id = "a1" |} |}
        |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "PATCH is not supported for any resource in collection 'abs'" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns 403 if missing lookup" {
      let! response = Request.patch Ctx3 "/abs/a1" |> getResponse

      response |> testStatusCode 403
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "403" @>
      test <@ json |> getPath "errors[0].detail" = "Collection 'abs' does not support any resource-specific operations" @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

    testJob "Returns error if collection case does not match" {
      let ctx = Ctx.WithDb (Db ())
      let! response = Request.patch ctx "/Abs/a1" |> getResponse
      response |> testStatusCode 404
      let! json = response |> Response.readBodyAsString
      test <@ json |> getPath "errors[0].status" = "404" @>
      test <@ json |> getPath "errors[0].detail" = "The path '/Abs/a1' does not exist, but differs only by case from the existing path '/abs/a1'. Paths are case sensitive." @>
      test <@ json |> hasNoPath "errors[0].source" @>
      test <@ json |> hasNoPath "errors[1]" @>
    }

  ]
