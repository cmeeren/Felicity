Felicity documentation
==============

---

### This is a work in progress

---

<img src="https://raw.githubusercontent.com/cmeeren/Felicity/master/felicity-logo.png" width="300" align="right"/>

The aim of this document is to explain how to use Felicity. For a complete, working example of a simple API, check out the [sample API](https://github.com/cmeeren/Felicity/tree/master/src/Felicity.SampleApi) in this repo. For a very brief overview, check out the [Quick Start section](https://github.com/cmeeren/Felicity/blob/master/README.md#quick-start) of the readme.

This documentation assumes working F# knowledge. If you’re new to F#, Scott Wlaschin’s blog [F# for fun and profit](https://fsharpforfunandprofit.com/) is a great place to start (and continue) learning the ins and outs of F# and functional programming. His book [Domain Modeling Made Functional](https://pragprog.com/book/swdddf/domain-modeling-made-functional) is also a great resource for learning F# (and in particular how it can  be used for domain modeling). You can find many more excellent resources at [fsharp.org](https://fsharp.org).

This documentation also assumes some knowledge of ASP.NET Core, [Giraffe](https://github.com/giraffe-fsharp/Giraffe), and of course [JSON:API](https://jsonapi.org/format/).

Suggestions for improvements are welcome. For large changes, please  open an issue. For small changes (e.g. typos), simply submit a PR.

Table of contents
-----------------

TODO

Basics
------

Felicity is centered around the concept of a **resource module**. For each resource type, simply define a module and use Felicity’s fluent-style API to define public `let`-bound values representing attributes, relationships, operations, etc. (Felicity will ignore non-public values.)

On startup, Felicity will use reflection to parse all resource modules and set up suitable routing/handlers.

### Scope

The goal of Felicity is to enable you to easily implement an **idiomatic JSON:API** on top of your **functional F# domain logic** with **minimal boilerplate**.

Felicity is a framework, not a library. It places constraints on what you can do and is opinionated regarding how you should do it. **If you find the Felicity API to be unnecessarily limiting or awkward for a certain use-case, please open an issue so we can discuss it.** That said, if you want full control over everything, at the expense of a lot more boilerplate, you should use a library instead of a framework, such as [FSharp.JsonApi](https://github.com/cmeeren/FSharp.JsonApi/) (no longer actively maintained).

### Namespaces

You get access to the entire Felicity API using `open Felicity`.

### An example resource module

Below is an example of a simple resource module from the [sample API](https://github.com/cmeeren/Felicity/tree/master/src/Felicity.SampleApi). Each definition (attribute, relationship, operation) mostly uses the bare minimum of configuration (which is often all that’s needed).

The rest of the documentation explains everything shown below as well as additional features.

```f#
module Article =

  let define = Define<Context, Article, ArticleId>()

  let id = define.Id.ParsedOpt(ArticleId.toString, ArticleId.fromString, fun a -> a.Id)

  let resourceDef = define.Resource("article", id).CollectionName("articles")

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

  let created =
    define.Attribute
      .Simple()
      .Get(fun a -> a.Created)

  let updated =
    define.Attribute
      .Nullable
      .Simple()
      .Get(fun a -> a.Updated)

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
    define.Operation
      .GetCollection(fun ctx parser ->
        parser.For(ArticleSearchArgs.empty)
          .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
          .Add(ArticleSearchArgs.setTypes, Filter.Field(articleType).List)
          .Add(ArticleSearchArgs.setSort, Sort.Enum(ArticleSort.fromStringMap))
          .Add(ArticleSearchArgs.setOffset, Page.Offset)
          .Add(ArticleSearchArgs.setLimit, Page.Limit.Max(20))
          .BindAsync(Db.Article.search)
      )

  let post =
    define.Operation
      .Post(fun ctx parser -> parser.For(Article.create, author, title, body))
      .AfterCreateAsync(Db.Article.save)

  let lookup =
    define.Operation
      .LookupAsync(Db.Article.byId)

  let get =
    define.Operation
      .GetResource()

  let patch =
    define.Operation
      .Patch()
      .AfterUpdateAsync(fun a -> async {
        let a = a |> Article.setUpdated (Some DateTimeOffset.Now)
        do! Db.Article.save a
        return a
      })

  let delete =
    define.Operation
      .DeleteAsync(Db.Article.delete)
```

### Context

All parts of Felicity’s API gives you optional access to a globally defined context type you define (often abbreviated `'ctx` in the API). This context type may, for example, authentication information, as demonstrated below. Felicity also needs to know how to create your context type from the ASP.NET Core `HttpContext`.

```f#
type Principal =
  | Anonymous
  | Authenticated of Username

type Context =
  { Principal: Principal }

module Context =

  // Simulate asynchronous authentication (e.g. DB or external auth service)
  let getCtx (ctx: HttpContext) =
    async {
      if false then return Error [unauthorized]
      else return Ok { Principal = Anonymous }
    }
```

You use the `getCtx` function when setting up Felicity in `ConfigureServices` as explained in the next section. IN the code above, `unauthorized` represents a JSON:API error object created using Felicity’s API, as shown further below.

### Startup configuration

You need to set up Felicity in `ConfigureServices`. The below code shows an example of a `Startup` class. You must supply the base URL used in resource/relationship links, as well as the function that creates your context.

```f#
type Startup() =

  member _.ConfigureServices(services: IServiceCollection) : unit =
    services
      .AddGiraffe()
      .AddJsonApi()
        .BaseUrl(Uri("http://localhost:5000"))
        .GetCtxAsyncRes(Context.getCtx)
        .Add()
      .AddOtherServices(..)

  member _.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) : unit =
    app
      .UseGiraffeErrorHandler(fun ex _ ->
        Log.Error(ex, "Unhandled exception while executing request")
        returnUnknownError
      )
      .UseGiraffe(mainHandler)
```

You may call `AddJsonApi` multiple times for different context types. This may be useful if you have some collections/resource types that are only accessible to privileged users, which you can model with a different context type.

Note that Felicity also supports transforming the context (e.g. for authorization) for individual operations; see the section TODO for details.

#### Handling uncaught errors

Giraffe has `UseGiraffeErrorHandler` which you can use to handle uncaught exceptions. If you want to return a JSON:API error object with status 500 and a generic “An unknown error has occurred” message, you can use `returnUnknownError`. If you want to return custom errors, you can use `returnErrorDocument` which accepts a list of errors to return.

#### Configuring the JSON serializer

Felicity uses System.Text.Json and [FSharp.SystemTextJson](https://github.com/Tarmil/FSharp.SystemTextJson) for serialization. If you need to configure the serialization options, you can add `.ConfigureSerializerOptions` to the chain shown above, which accepts a parameter of type `JsonSerializerOptions -> unit`.

### Routing

Use the `jsonApi<'ctx>` handler to insert the JSON:API endpoints for a specific context type wherever you want. Requests to unknown collections will fall through. For example, in the code below, if the only resource module that uses `Context` has collection name `articles`, then `jsonApi<Context>` will handle any request starting with `/articles`, but any other request will fall through to `someOtherHandler`.

```f#
let mainHandler : HttpHandler =
  choose [
    jsonApi<Context>
    someOtherHandler
  ]
```

Note that you can place the `jsonApi` in a subroute, too, and everything works just like you’d expect.

### Errors

Returning helpful errors to API clients is important, and robust and helpful error handling is one of Felicity’s main strengths. Felicity already returns helpful errors for almost 100 common error conditions you don't need to think about, from content negotiation to referencing non-existent related resources to errors while parsing your custom attribute types. The only errors you need to define and return are the custom errors your API needs to return.

#### Defining errors

You define errors like this:

```f#
let unauthorized =
  Error.create 401
  |> Error.setTitle "Unauthorized"
  |> Error.setDetail "The authorization was missing or invalid for this operation"
```

You never need to set the Error object's source pointer/parameter. Felicity will take care of that wherever relevant, even for your custom errors.

Furthermore, all errors already have the `id` property set to a random GUID (formatted without dashes to make it easier to select/copy). You can override the `id` property if you need to.

For convenience, there is also an `Error.setDetailf` function that works similar to `sprintf`.

#### Returning errors

Almost all parts of Felicity’s API allows you to return `Result<'a, Error list>`, meaning that if you return a list of errors, the request will fail and the specified errors will be returned to the client. (Your domain logic may of course return error DUs which you map to `Error` objects at a higher, API-specific level.)

The status code for the entire response will be most frequent status code among all the returned error objects (often, there is only one error anyway, or all error objects share the same status code). Please open an issue if this turns out not to be sufficient for you.

#### Returning headers in error responses

Felicity allows you to modify all success responses, but not directly error responses. However, you can still return custom HTTP headers in error responses using `Error.addHeader`. The error response will contain any headers added to any of the returned error objects.

#### Logging returned errors

All returned errors are automatically logged at information level using the ASP.NET Core logger you have configured. The log message includes the `id`, `status`, `code`, `title`, and `detail`. 

If you want to suppress error logs, the category name is `Felicity.ErrorHandler`.

### Compile-time safety

Most of Felicity’s API is type-safe, meaning you’ll get compile-time errors if you try to do things that don’t make sense. However, to allow the convenient syntax demonstrated above, some sacrifices had to be made. Rest assured that the errors that are not caught at compile-time will be caught immediately at startup.

An example of an error that will be caught at startup is if a resource module contains a resource-specific operation such as PATCH, but does not contain a lookup operation (meaning Felicity has no way of actually retrieving the entity with ID `1` given `PATCH /articles/1`).

Only the following very specific errors can occur later at run-time (if you haven’t read the rest of the documentation, these may not make much sense at this point):

* If you define a polymorphic relationship without ID parsers (meaning the relationship is read-only), and then try to manually parse the relationship from a response body in a request parser
* If you define a polymorphic relationship and then try to use this to parse an ID in a `filter` query parameter in a request parser (since there is no resource type information in the query parameter, there is no way to know which ID parser to use)

### Requirements for your domain logic

Felicity is a framework, not a library, and is opinionated on how your domain logic works. Fortunately, Felicity drives you toward a good, functional design.

Your core logic must be “pure” in the sense that it must not cause observable state changes (such as mutate objects or persist changes to a database). For example, field “setters” should have signatures like `'arg -> 'entity -> 'entity`, returning a new entity (typically an updated record). This is a requirement because any setter is allowed to return an error, in which case an error response should be returned, which means that no observable state changes must have taken place while executing the setters.

It’s no problem for setters to be asynchronous; for example, a setter (or even just an attribute parser) may require a database lookup to ensure the value is valid. The only requirement is that your domain logic should not cause observable state changes, in case the request fails and Felicity needs to throw away the updated entity.

Therefore, any part of your domain logic may be asynchronous and/or return `Result`. It may also accept the context type you define. For example, the general signature for a “setter” is

```f#
'ctx -> 'arg -> 'entity -> Async<Result<'entity, Error list>>
```

Felicity has tons of overloads for simpler signatures for all operations (e.g. without context, no async, no result, etc.). The goal is to enable you to simply plug your existing domain functions directly into Felicity without needing to use lambdas or lifting to Async or Result.

Here is an example of simple domain logic that works well with Felicity:

```f#
type PersonId = private PersonId of Guid with
  static member toString (PersonId x) = string x
  static member fromString = Guid.tryParse >> Option.map PersonId

type FirstName = private FirstName of string with
  static member toString (FirstName x) = x
  static member fromString = FirstName

type LastName = private LastName of string with
  static member toString (LastName x) = x
  static member fromString = LastName

type Person = {
  Id: PersonId
  FirstName: FirstName
  LastName: LastName
}

module Person =

  let create firstName lastName = {
    Id = Guid.NewGuid () |> PersonId
    FirstName = firstName
    LastName = lastName
  }

  let setFirstName firstName (person: Person) =
    { person with FirstName = firstName }

  let setLastName lastName (person: Person) =
    { person with LastName = lastName }
```

### Sparse fieldsets and included resources

Felicity automatically supports sparse fieldsets and includes for all operations (currently except resource `self` endpoints; please open an issue if you need that functionality). By default, all fields are included, and no related resources are included.

Included resources are fetched asynchronously and on-demand. If your related resources are fetched from the database when needed, you may encounter the “N+1 problem”; for example fetching a list of 1000 resources with an included relationship will cause 1000 queries to the database to fetch the related resource(s) for each of the main data resources. The problem gets even worse for multi-level includes. There are no trivial solutions, but it might be fairly simple to write (more complicated) batched SQL queries and use e.g. [BatchIt](https://github.com/cmeeren/BatchIt) to abstract away the batching in code.

Resource ID and definition
--------------------------

TODO

Attributes
----------

TODO

* Non-nullable
* Nullable
* Domain vs. serialized
* Simple
* Parsed
* Enum

### Skippable attributes

TODO

Relationships
-------------

TODO

* Setter: ID vs. related, must check for existence self if using ID setter and return a suitable error
* To-many vs. complete replacement

### Skippable relationships

TODO

### PATCH/POST/DELETE relationship self

TODO

#### Modifying the response

TODO

#### Returning 202 Accepted

TODO

#### BeforeUpdateModifySelf

TODO

#### HTTP preconditions

TODO refer to separate section

#### Execution order

TODO

Attribute/relationship constraints
----------------------------------

TODO

GET collection operation
------------------------

TODO

### Parsing parameters

TODO

### Modifying the response

TODO

### Execution order

TODO

POST collection operation
-------------------------

TODO

* Will patch optional settable fields

### Parsing parameters and fields

TODO

* Parsed fields will not be used in patcher

### Client-supplied ID

TODO

### Modifying the response

TODO

### Returning 202 Accepted

TODO

### BeforeCreate

TODO

### Execution order

TODO

### HTTP preconditions

TODO refer to separate section

ID lookup operation
-------------------

TODO

GET resource operation
----------------------

TODO

### Modifying the response

TODO

### Execution order

TODO

PATCH resource operation
------------------------

TODO

### Modifying the response

TODO

### Returning 202 Accepted

TODO

### BeforeUpdate

TODO

### HTTP preconditions

TODO refer to separate section

### Execution order

TODO

DELETE resource operation
-------------------------

TODO

### Modifying the response

TODO

### Returning 202 Accepted

TODO

### BeforeDelete

TODO

### HTTP preconditions

TODO refer to separate section

### Execution order

TODO

Custom operations/links
-----------------------

TODO

### Conditions

TODO

### Meta

TODO

### When is the link present?

* TODO: If mapCtx succeeds, and either condition is true, or condition is false and has meta (then href is null). If condition is false and has no meta, no link

### HTTP preconditions

TODO

* POST, PATCH, DELETE

### Execution order

TODO

Operation-specific authorization
--------------------------------

TODO

* Context mapping

Request parser
--------------

TODO

* Parse vs. map/bind
* Simple use of GetRequired/GetOptional
* Prohibit
* Context-dependent parsing
* Relationships
* Filter parameters
* Attributes vs. strings
* Nullable attributes
* Bool override filter
* Query parameters
  * Filter
  * Single vs. List
  * Sort
  * Page
  * Custom
    * Show how to use .Name for fields
  * Parsed vs. enum
* Headers
* Using optional getters as required option-wrapped params

Conditional GET/HEAD (`ETag`/`If-None-Match`)
---------------------------------------------

TODO

* hashes response body
* success responses
* 304 not modified

Conditional POST/PATCH/DELETE (`If-Match`/`If-Unmodified-Since`)
------------------------------------------------

TODO

Polymorphism
------------

TODO

Current limitations
-------------------

TODO

* Included resources in relationship self requests
* Sorting of relationships
* Relationship meta
* Resource meta
* top-level `jsonapi` member
* top-level links
* top-level meta

FAQ
---

TODO

* Why `Func`