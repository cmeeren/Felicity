Felicity
==============

<img src="https://raw.githubusercontent.com/cmeeren/Felicity/master/felicity-logo.png" width="300" align="right" />

**Boilerplate-free, idiomatic JSON:API for your beautiful, idiomatic F# domain model. Optimized for developer happiness.**

### Elevator pitch

Felicity is a framework that allows you to expose your functional F# domain logic as an API
following [the JSON:API specification](https://jsonapi.org/), with no boilerplate.

Core features:

* Based on ASP.NET Core and [Giraffe](https://github.com/giraffe-fsharp/Giraffe)
* Thoroughly tested
* Highly succinct, declarative and discoverable fluent-style syntax
* Designed for with idiomatic, immutable F# domain logic, including DU wrappers and smart constructors
* Automatic support for sparse fieldsets and included resources
* Automatic routing and link generation for resources and relationships
* Any attribute, relationship, or operation may be async or return errors
* Loads included resources on-demand and in parallel
* Easily pass strongly typed context data (e.g. authentication information) through Felicity to any part of your code
* Over 100 common error cases handled automatically; you only have to care about your application-specific errors
* If it compiles and starts, it works
* Support for polymorphic resource collections and relationships
* Built-in support for resource-level precondition validation using ETags and modification dates (requiring the client
  to supply `If-Match` and `If-Unmodified-Since` to avoid “mid-air collisions”)
* Resource-level locking and operation queueing to ensure thread safety (can also lock “parent” resources and plug into
  external locking mechanisms)
* Supports sideposting (a.k.a. sideloading) to create a related resource hierarchy in a single POST request (not in
  official JSON:API spec)

### Production readiness

This framework contains ~1000 end-to-end tests checking success-path and error-path functionality of all operations,
and is used in several mission-critical production APIs at our company. I’m not claiming it’s perfect, or even bug-free,
but it’s well tested, and I have a vested interest in keeping this framework working properly.

The framework is still at 0.x because it's still fairly new and I'm still discovering improvements that require breaking
changes every now and then. However, do not take 0.x to mean that it’s a buggy mess, or that the API will radically
change every other week.

### A note on versioning

While at 0.x, I’ll try to increment the minor version for breaking changes and the patch version for anything else.

Note that “breaking changes”, at least for now, only consider idiomatic use of the framework. For example, you should
never need to use type annotations for the many “builder types” used by Felicity, so I may rename and refactor those and
not consider it a breaking change as long as the final intended syntax stays the same.

Contributing
------------

Contributions and ideas are welcome! Please
see [Contributing.md](https://github.com/cmeeren/Felicity/blob/master/.github/CONTRIBUTING.md) for details.

Documentation
-------------

[Documentation is in progress.](https://github.com/cmeeren/Felicity/blob/master/DOCUMENTATION.md) A lot is already done.

I also highly recommend you check out
the [sample API](https://github.com/cmeeren/Felicity/tree/master/src/Felicity.SampleApi) in this repo, which is a simple
but complete and almost-production-ready example API implementation. Open the main solution in VS, start at the topmost
file in the sample API, and read through the project in compilation order. There are lots of comments along the way to
explain what’s going on.

Feel free to open an issue if you have questions.

Quick start
-----------

### Assumptions about your domain logic

Your core logic must be “pure” in the sense that it must not cause observable state changes (such as mutate objects or
persist changes to a database). In other words, field “setters” should have signatures like
`'arg -> 'entity -> 'entity`, returning a new entity (typically an updated record). This is a requirement because any
setter may potentially return an error, in which case an error response should be returned, which means that no
observable state changes must have taken place while executing the setters.

Any part of your domain logic may be asynchronous and/or return `Result`, and may accept an API-specific context type
you define (that may, for example, contain an authenticated user object). For example, the general signature for a
“setter” is

```f#
'ctx -> 'arg -> 'entity -> Async<Result<'entity, Error list>>
```

where `Error` is a Felicity-defined type representing a JSON:API error. (Your domain logic may of course return error
DUs which you map to `Error` objects at a higher, API-specific level.) Felicity has tons of overloads for simpler
signatures for all operations (e.g. without context, no async, no result, etc.). The goal is to enable you to simply
plug your existing domain functions directly into Felicity without needing to use lambdas or lifting to Result or async.

Here is an example of simple domain logic that works well with Felicity:

```f#
type PersonId = PersonId of Guid
type FirstName = FirstName of string
type LastName = LastName of string

type Person = {
    Id: PersonId
    FirstName: FirstName
    LastName: LastName
}

module Person =

    let create firstName lastName = {
        Id = Guid.NewGuid() |> PersonId
        FirstName = firstName
        LastName = lastName
    }

    let setFirstName firstName (person: Person) = { person with FirstName = firstName }

    let setLastName lastName (person: Person) = { person with LastName = lastName }
```

### Installation

Install [Felicity](https://www.nuget.org/packages/Felicity) from NuGet.

### Usage

I highly recommend you check out
the [sample API](https://github.com/cmeeren/Felicity/tree/master/src/Felicity.SampleApi) in this repo, which is a simple
but complete and almost-production-ready example API implementation. Open the main solution in VS, start at the topmost
file in the sample API, and read through the project in compilation order. There are lots of comments along the way to
explain what’s going on.

As a very short introduction, I hope the steps below are useful, but bear in mind that they only skim the surface (
however, while Felicity has many more features than shown below, it doesn’t really get much more complicated).

#### 0. Open Felicity

Everything is available under the `Felicity` namespace.

```f#
open Felicity
```

#### 1. Define errors you need to return

You don’t actually do this *first*; just define errors as you need them. We need an error for the example in the next
section.

```f#
[<AutoOpen>]
module Errors =

    let unauthorized =
        Error.create 401
        |> Error.setTitle "Unauthorized"
        |> Error.setDetail "The authorization was missing or invalid for this operation"
```

#### 2. Define your global context type and how to create it from `HttpContext`

Felicity allows you to map the ASP.NET Core `HttpContext` to a value you can optionally access in all operations, called
a “context” (or “ctx” for short). This value may for example contain authentication data. You can place anything you
want here.

```f#
type Principal =
    | Anonymous
    | Authenticated of Username

type Context = { Principal: Principal }

module Context =

    // Simulate asynchronous authentication (e.g. DB or external auth service)
    let getCtx (ctx: HttpContext) = async {
        if false then
            return Error [ unauthorized ]
        else
            return Ok { Principal = Anonymous }
    }
```

#### 3. Define resource modules

Each module corresponds to a resource. See
the [sample API](https://github.com/cmeeren/Felicity/tree/master/src/Felicity.SampleApi) for helpful comments for each
definition; they are removed below for brevity.

```f#
module Article =

    let define = Define<Context, Article, ArticleId>()

    let resId =
        define.Id.ParsedOpt(ArticleId.toString, ArticleId.fromString, (fun a -> a.Id))

    let resourceDef = define.Resource("article", resId).CollectionName("articles")

    let title =
        define.Attribute
            .Parsed(ArticleTitle.toString, ArticleTitle.fromString)
            .Get(fun a -> a.Title)
            .Set(Article.setTitle)

    let body =
        define.Attribute
            .Parsed(ArticleBody.toString, ArticleBody.fromString)
            .Get(fun a -> a.Body)
            .Set(Article.setBody)

    let articleType =
        define.Attribute
            .Enum(ArticleType.toString, ArticleType.fromStringMap)
            .Get(fun a -> a.Type)
            .Set(Article.setType)

    let createdAt = define.Attribute.Simple().Get(fun a -> a.CreatedAt)

    let updatedAt = define.Attribute.Nullable.Simple().Get(fun a -> a.UpdatedAt)

    let author =
        define.Relationship
            .ToOne(Person.resourceDef)
            .GetAsync(Db.Person.authorForArticle)
            .Set(Article.setAuthor)

    let comments =
        define.Relationship
            .ToMany(Comment.resourceDef)
            .GetAsync(Db.Comment.allForArticle)

    let getCollection =
        define.Operation.GetCollection(fun ctx parser ->
            parser
                .For(ArticleSearchArgs.empty)
                .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
                .Add(ArticleSearchArgs.setTypes, Filter.Field(articleType).List)
                .Add(ArticleSearchArgs.setSort, Sort.Enum(ArticleSort.fromStringMap))
                .Add(ArticleSearchArgs.setOffset, Page.Offset)
                .Add(ArticleSearchArgs.setLimit, Page.Limit.Max(20))
                .BindAsync(Db.Article.search))

    let post =
        define.Operation
            .Post(fun ctx parser -> parser.For(Article.create, author, title, body))
            .AfterCreateAsync(Db.Article.save)

    let lookup = define.Operation.LookupAsync(Db.Article.byId)

    let get = define.Operation.GetResource()

    let patch =
        define.Operation
            .Patch()
            .AfterUpdateAsync(fun a -> async {
                let a = a |> Article.setUpdated (Some DateTimeOffset.Now)
                do! Db.Article.save a
                return a
            })

    let delete = define.Operation.DeleteAsync(Db.Article.delete)
```

#### 5. Register a JSON:API handler for the context type in `ConfigureServices` and register/add the routes

```f#
type Startup() =

    member _.ConfigureServices(services: IServiceCollection) : unit =
        services
            .AddGiraffe()
            .AddRouting()
            .AddJsonApi()
            .GetCtxAsyncRes(Context.getCtx)
            .Add()
            .AddOtherServices( (* ... *) )

    member _.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) : unit =
        app
            .UseGiraffeErrorHandler(fun ex _ ->
                Log.Error(ex, "Unhandled exception while executing request")
                returnUnknownError)
            .UseRouting()
            .UseJsonApiEndpoints<Context>()
        |> ignore
```

#### 6. Enable server garbage collection

For performance reasons, you should enable server GC if you are running the API on multiple cores. (Cursory testing on
my dev machine indicates a ~30% speedup in Felicity code just from this change alone, which is noticeable for very large
responses.) Refer to your target environment documentation (Azure, IIS, etc.) for details on whether this is enabled by
default, and how to enable it if not. You can find some general
information [here](https://docs.microsoft.com/en-us/dotnet/core/run-time-config/garbage-collector).

#### 7. Profit!

That’s it! You now have a wonderfully compliant JSON:API exposing your wonderfully functional F# domain logic.

Release notes
-------------

[RELEASE_NOTES.md](https://github.com/cmeeren/Felicity/blob/master/RELEASE_NOTES.md)
