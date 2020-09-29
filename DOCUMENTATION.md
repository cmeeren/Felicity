Felicity documentation
==============

---

### This is a work in progress

---

<img src="https://raw.githubusercontent.com/cmeeren/Felicity/master/felicity-logo.png" width="300" align="right" />

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

  let resId = define.Id.ParsedOpt(ArticleId.toString, ArticleId.fromString, fun a -> a.Id)

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

  let createdAt =
    define.Attribute
      .SimpleDateTimeOffset()
      .Get(fun a -> a.CreatedAt)

  let updatedAt =
    define.Attribute
      .Nullable
      .SimpleDateTimeOffset()
      .Get(fun a -> a.UpdatedAt)

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

You need to set up Felicity in `ConfigureServices`. The below code shows an example of a `Startup` class. You must supply the function that creates your context.

```f#
type Startup() =

  member _.ConfigureServices(services: IServiceCollection) : unit =
    services
      .AddGiraffe()
      .AddJsonApi()
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

#### Base URLs

JSON:API responses contain resource/relationship links. By default, Felicity infers these links from the HTTP request. For example, if your API is available both on `https://example.com` and `https://something-else.com`, then `GET https://example.com/articles` will return resources with `example.com` in their links, and for `GET https://something-else.com/articles` the resources will have `something-else.com` in their links.

If you want, you can use the `.BaseUrl` method after `.AddJsonApi()` to specify the base URL that will be used for all links.

#### Multiple context types

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

#### Pacing the JSON:API routes in a subroute

You can place the `jsonApi` in a subroute, too. For example, the following route

```f#
let mainHandler : HttpHandler =
  choose [
    subRoute "/foo" (subRoute "/bar" jsonApi<Context>)
  ]
```

means that clients call `https://example.com/foo/bar/articles/1` to get the `article` with ID `1`.

If you do this, you need to use the `.RelativeJsonApiRoot` method after `.AddJsonApi()` in your startup code to specify the relative root in order to get correct links in the JSON:API responses, like this (leading/trailing slashes doesn’t matter):

```f#
member _.ConfigureServices(services: IServiceCollection) : unit =
  services
    .AddGiraffe()
    .AddJsonApi()
      .GetCtxAsyncRes(Context.getCtx)
      .RelativeJsonApiRoot("foo/bar")
      .Add()
    .AddOtherServices(..)
```

Alternatively you may use `.BaseUrl` to explicitly specify the whole base URL as described earlier.

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

It’s no problem for your domain logic to be asynchronous; for example, a setter (or even just an attribute parser) may require a database lookup to ensure the value is valid. The only requirement is that your domain logic should not cause observable state changes, in case the request fails and Felicity needs to throw away the updated entity.

Therefore, any part of your domain logic may be asynchronous and/or return `Result`. It may also accept the context type you define. For example, the general signature for a “setter” is

```f#
'ctx -> 'arg -> 'entity -> Job<Result<'entity, Error list>>
```

(`Job` is from [Hopac](https://github.com/Hopac/Hopac).) Felicity has tons of overloads for simpler/alternative signatures for all operations (e.g. without context, `Async` instead of `Job`, synchronous, no `Result`, etc.). The goal is to enable you to simply plug your existing domain functions directly into Felicity without needing to use lambdas or lifting to `Job`, `Async` or `Result`.

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

Included resources are fetched asynchronously and on-demand. If your related resources are fetched from the database when needed, you may encounter the “N+1 problem”; for example fetching a list of 1000 resources with an included relationship will cause 1000 queries to the database to fetch the related resource(s) for each of the main data resources. The problem gets even worse for multi-level includes. There are no trivial solutions, but it might be relatively simple to write (more complicated) batched SQL queries and use e.g. [BatchIt](https://github.com/cmeeren/BatchIt) to abstract away the batching in code.

Resource ID and resource definition
--------------------------

The very first thing you should define in a resource module is a “definition helper” that fixes the types of the context, entity, and ID, and is used to define everything else in the module:

```f#
let define = Define<Context, Article, ArticleId>()
```

You should then define the how the resource ID is converted to/from a string, and how it is obtained from the entity:

```f#
let resId = define.Id.ParsedOpt(ArticleId.toString, ArticleId.fromString, fun a -> a.Id)
```

Above, we use `ParsedOpt` because (implied in this example) `ArticleId.fromString` returns `ArticleId option`. There are `Parsed*` methods that allow you to use a function that returns  a raw string, async, result, option, or a combination of these. In the event that your ID type is a simple `string`, you can use `define.Id.Simple`.

The final core definition is called the “resource definition”, and it is where you specify the resource type name and, optionally, a collection name:

```f#
let resourceDef = define.Resource("article", resId).CollectionName("articles")
```

Above, we define a resource with type name `article` where the ID is parsed/obtained as specified in `resId`, and we further specify that this resource has a `self` URL using the collection name `articles`. In other words, its self URL is `https://base.url/articles/{id}`.

### When is a collection name required?

You may define resources without collection names. These resources will not have `self` links, and may only be included as related resources in compound documents.

For example, if

* `article` is defined with collection name `articles`,
* `person` is defined without a collection name, and
* `article` has a relationship `author` to `person`,

then you can `GET /articles/{id}?include=author` to get the article with its author, but you can not fetch any persons directly using `GET /persons`, because there is no such collection.

The following table describes the definitions supported and not supported for modules without a collection name:

|      | Definition                                          | Supported |
| ---- | --------------------------------------------------- | --------- |
| ❌    | GET collection                                      | No        |
| ❌    | POST collection                                     | No        |
| ❌    | GET resource                                        | No        |
| ❌    | PATCH resource                                      | No        |
| ❌    | DELETE resource                                     | No        |
| ❌    | Custom links                                        | No        |
| ✅    | Relationship getters                                | Yes       |
| ❌    | Relationship setters (including to-many add/remove) | No        |

Attributes
----------

You define attributes using `define.Attribute`:

```f#
let title =
  define.Attribute
    .Parsed(ArticleTitle.toString, ArticleTitle.fromString)
    .Get(fun a -> a.Title)
    .Set(Article.setTitle)
```

Here, we have defined an attribute with the domain type `ArticleTitle` which uses the functions `ArticleTitle.toString : ArticleTitle -> string` and `ArticleTitle.fromString : string -> ArticleTitle` to convert between `ArticleTitle` and `string`.

The attribute above is defined with a getter of type `Article -> ArticleTitle`, and a setter `Article.setTitle : ArticleTitle -> Article -> Article`.

`Parsed` and `Set` has overloads accepting returning `Async` and/or `Result`.

`Get` has `Async` overloads, but not `Result`, because an attribute getter must never fail – if that was the case, you could end up in a situation where you perform and persist changes, but no success response can be returned to the client because a getter fails.

### Domain vs. serialized types

There are two important type when defining attributes: The domain type, e.g. a DU wrapper, and the “raw” type that is used for serializing and deserializing, for example primitives like `string` and `int`, or non-JSON types like `DateTimeOffset` if you’re happy with the default serialization of these or are otherwise willing to configure it. Remember that you can always explicitly convert e.g. a `DateTimeOffset` to and from `string` manually to have full control over the serialized representation.

### Simple attributes

If the domain type can be serialized and deserialized directly (e.g. in the case of an unwrapped `DateTimeOffset`), you can use `Simple` instead of `Parsed`.

### Enum attributes

If the attribute value can only accept a limited set of string values (e.g. if the backing domain type is a field-less DU), and you want these values automatically mentioned in the error message if they try to set it to an invalid value, you can use `Enum` instead of `Parsed`.

`Enum` works like `Parsed`, but instead of the second parameter being `string -> DomainType option` (or `Result`) you supply a `(string * DomainType) list`. Felicity will map the input values to your `DomainType` according to this list, and will mention all possible values in the error message returned to the client if an invalid value is encountered.

Note that the automatic error message is the only benefit of `Enum` over `Parsed`.

Note also that the first parameter to `Enum` is the same as with `Parsed`, namely `DomainType -> string`. While Felicity could swap the mapping and use it to also transform the other way, that would fail if the domain type had an invalid value, and you’d have no compile-time guarantees that the mapping contained all values. Furthermore, you might want to restrict the settable values to a subset of the possible values.

### Skippable attributes

`Get` has overloads allowing you to return a value wrapped in `Skippable<_>`, defined by FSharp.SystemTextJson in namesapce `System.Text.Json.Serialization`. This type is similar to `Option<_>` and has the cases `Skip` and `Include of 'a`, but whereas `Option` indicates `null` (more on nullable attributes below), `Skippable` indicates that a value should not be present at all in the response.

One use-case for this is if the requesting user has partial access to a resource, in the sense that the user has access to only some of a resource’s fields. Then you can have the getter return `Skip` if the user does not have access to the field, and the field will not appear in the response.

Note however that clients may be surprised to only get some of the fields they expect. Document well and use with care.

### Nullable attributes

To define a nullable attribute, it must be wrapped in `option` on the domain side. Then, simply insert `.Nullable` after `define.Attribute`:

```f#
let updatedAt =
  define.Attribute
    .Nullable
    .SimpleDateTimeOffset()
    .Get(fun a -> a.UpdatedAt)
```

Here, the getter returns the field `Article.UpdatedAt : DateTimeOffset option`.

### Read-only/write-only attributes

To define a read-only attribute, simply define an attribute without a setter. Errors will be returned if the client supplies the field in a request. The `updatedAt` attribute shown above is a read-only attribute.

You may still use read-only fields when parsing responses for e.g. POST requests, as described in section TODO. That means you can have a read-only attribute that may not be set in PATCH requests, but which you can still use as a parameter in POST requests when creating new resources.

To define a write-only attribute (e.g. a user’s password, or Base64-encoded file contents for upload), simply define an attribute without a getter.

You can even define an attribute with neither a getter nor a setter. This attribute may then only be used when parsing, and will never be returned to the client.

Relationships
-------------

Much of what is said above for attributes is relevant for relationships, too, and won’t be repeated here. Additionally, relationships add further levels of complexity in several ways. Thankfully, Felicity makes it almost as simple to define relationships as it does attributes.

A basic relationship definition looks like this:

```f#
let author =
  define.Relationship
    .ToOne(Person.resourceDef)
    .GetAsync(Db.Person.authorForArticle)
    .Set(Article.setAuthor)
```

You start with `define.Relationship` and can choose between `ToOne`, `ToOneNullable`, or `ToMany`. Each of these accepts the resource definition for the related resource.

The relationship above automatically supports `include` (all gettable relationships do), GET for its `related` and `self` links, and `PATCH` to the relationship’s `self` link (as well as `PATCH` to the resource’s `self` link, of course).

The getter above is asynchronous and loads the related resource from DB whenever it’s included. See section TODO for notes about the N+1 problem and possible solutions.

### ID setters vs. related resource setters

The setter above has signature `AuthorId -> Article -> Article`. However, the safest and most convenient option is using a setter accepting the full entity:

```f#
.Set(Article.lookup, Article.setAuthor)
```

The setter above accepts a lookup operation (see section TODO) and a setter with signature `Author -> Article -> Article`. Even if you don’t need more than the ID, using this variant provides the benefit that Felicity will return a suitable error if the resource is not found.

If you instead use an ID setter as shown first, you yourself have to ensure that `Article.setAuthor` checks if the ID actually corresponds to an existing resource (including, if relevant, whether the requesting user has access to it, which is assumedly built into `Article.lookup` above) and return a suitable 404 error. Furthermore, you can‘t easily add a pointer to that error, because the setter may be run as part of either a PATCH resource request or a PATCH relationship `self` link request, and you don’t know which (and therefore the pointer to use).

### Modifying to-many relationships

To-many relationships does not have `Set`. They instead have the following:

* `SetAll`, which is just `Set` by another name, and completely replaces the relationship members
* `Add`, which adds support for POST to the relationship’s `self` link to add members
* `Remove`, which adds support for DELETE to the relationship’s `self` link to remove members

As always, simply define what you want to be available, and Felicity will take care of returning suitable errors for invalid requests.

### Skippable relationships

As with attributes, relationships getters may return `Skippable`-wrapped values. However, an important caveat is that **you must never return Skip after a relationship has been updated**. If you do, and if the update happened via the relationship’s `self` link, no success response can be returned to the client, as required by JSON:API. Instead an error will be logged and an embarrassing 500 error will be returned to the client.

### GET/PATCH/POST/DELETE relationship self

Relationships have `self` links which support GET, PATCH, POST, and DELETE operations. As with other operations (detailed later), you can configure how these work.

#### Persisting changes

Use `AfterModifySelf` to persist the changes. You can normally pass the same function here as you pass to `AfterCreate` and `AfterUpdate` for POST and PATCH, respectively.

`AfterModifySelf` is the only function that may cause observable state change.

An exception will be thrown at startup if you don’t specify `AfterModifySelf`.

#### Returning 202 Accepted

If you need PATCH/POST/DELETE to return `202 Accepted`, simply add `ModifySelfReturn202Accepted()` to the relationship definition. This makes all of these three operations return `202 Accepted` instead of `200 OK`.

#### Modifying the response

If you need to modify the response, e.g. to add cache headers, specify one or more of these functions:

* `ModifyPatchSelfOkResponse`
* `ModifyPatchSelfAcceptedResponse`
* `ModifyPostSelfOkResponse`
* `ModifyPostSelfAcceptedResponse`
* `ModifyDeleteSelfOkResponse`
* `ModifyDeleteSelfAcceptedResponse`

All of them allow you to modify the `HttpContext`, either directly or by using a Giraffe `HttpHandler`.

#### Performing pre-update work

Use `BeforeModifySelf` to perform (potentially failing and/or asynchronous) work before modifying the relationship. Note that this work must not cause observable state changes; the request may still fail after this stage.

#### HTTP preconditions

See section TODO for how to do precondition validation (using `ETag`/`Last-Modified` and `If-Match`/`If-Unmodified-Since`) for these requests.

#### Execution order

1. Get the context
2. Validate preconditions
3. `BeforeModifySelf`
4. Parse ID(s) of relationship data
5. Related resource lookup (if using related setter and not ID setter)
6. `Set`/`SetAll`/`Add`/`Remove`
7. `AfterModifySelf`
8. `Modify*Response`

Attribute/relationship constraints
----------------------------------

Felicity supports informational field constraints as described in [this post](https://discuss.jsonapi.org/t/dynamic-constraints-a-proposal-for-a-general-specification/1744). In an attribute or relationship definition, use `AddConstraints` or its alternatives to add constraints to the field. If you use this feature, you may not define a separate field called `constraints`.

GET collection operation
------------------------

At its simplest, a GET collection operation just needs to get a list of entities to return:

```f#
let getCollection = define.Operation.GetCollectionAsync(Db.Article.getAll)
```

### Parsing parameters

It may be useful to allow the client to filter the collection. For this, you use the request parser overload. You can read more about request parsing in section TODO, but for parsing filters with GET collection operations, it can look like this:

```f#
let getCollection =
  define.Operation
    .GetCollection(fun ctx parser ->
      parser.For(ArticleSearchArgs.empty)
        .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
        .Add(ArticleSearchArgs.setTypes, Filter.Field(articleType).List)
        .Add(ArticleSearchArgs.setOffset, Page.Offset)
        .Add(ArticleSearchArgs.setLimit, Page.Limit.Max(20))
        .BindAsync(Db.Article.search)
    )
```

### Modifying the response

If you need to modify the response, e.g. to add cache headers, use `ModifyResponse`. This function allows you to modify the `HttpContext`, either directly or by using a Giraffe `HttpHandler`.

### Execution order

1. Get the context
2. Transform the context if specified (see section TODO)
3. Get the collection (including any request parsing)
4. `ModifyResponse`

POST collection operation
-------------------------

At its simplest, a POST operation simply requires a function that creates an entity, and a function to persist the changes:

```f#
let post =
  define.Operation
    .Post(fun _ -> Article.defaultArticle)
    .AfterCreateAsync(Db.Article.save)
```

### Parsing fields and parameters

It is likely that some fields are required when creating a resource.  For this, use the request parser overload. You can read more about request parsing in section TODO, but for creating resources where some attributes and relationships are required, it can look like this:

```f#
let post =
  define.Operation
    .Post(fun ctx parser -> parser.For(Article.create, author, title, body))
    .AfterCreateAsync(Db.Article.save)
```

Above, `author`, `title`, and `body` are attributes and relationships in the `Article` resource module.

### Additional settable fields

After creating the resource, the POST operation will mimic the PATCH operation by running the setters for any extra fields present in the request. Fields which have been parsed as shown above will be ignored.

### Client-generated ID

To support client-generated IDs, simply parse the resource ID. If you do not parse the resource ID, Felicity returns a suitable error to the client if an ID is supplied.

### Modifying the response

If you need to modify the response, e.g. to add cache headers, use `ModifyResponse`. This function allows you to modify the `HttpContext`, either directly or by using a Giraffe `HttpHandler`.

### Returning 202 Accepted

If you need the operation to return `202 Accepted`, simply add `Return202Accepted()` to the operation definition.

### Execution order

1. Get the context
2. Transform the context if specified (see section TODO)
3. Create the entity (including any request parsing)
4. `AfterCreate`
5. `ModifyResponse`

Custom POST collection operation
--------------------------------

The normal POST operation described above requires you to supply at least two functions – one for creating an entity that does not cause any observable state change, and one for persisting the entity. You also have to choose whether or not to *always* return `202 Accepted`.

This is very simple, but you may come across use-cases where this at best would require you to twist your domain logic quite a bit, or at worst is not sufficient at all. For these use-cases, you can use a custom POST operation instead. Here you are entirely free, and you get a helper that you can use for common POST tasks, but you’re not guided as much as with the normal POST operation.

### Example

Let’s say you have an account service with these requirements:

* A user can have multiple emails
* Emails must be verified before being added
* For anonymity/misuse reasons, you can not return an error indicating that an email address is taken – instead, when adding an email address, you always send an email to the specified address, either with a verification code/link, or with a message indicating that the email is already registered.

There are undoubtedly many ways to model this. One way is like this:

* You have a `user` resource with an `emails` relationship which is to-many `email`
* You have an `email` resource with attributes `email` (the address) and `token` (write-only, used when verifying), and a relationship `user` (back-reference for use when creating using POST)
* Add email, step 1: `POST /emails` with fields `email` and `user`. Returns `202 Accepted`. Internally a token is generated and stored along with the email address and user ID, and an email is sent to the specified address.
* Add email, step 2: `POST /emails` with field `token`. The “pending email request” is looked up by the token, and the email is added to the user. Returns `201 Created` with the new `email` resource.

The custom POST operation might look like this (note that all of the steps below are optional – you can do whatever you want):

```f#
let post =
  define.Operation
    // PostCustomAsync accepts context, parser helper, and a special helper for
    // the PostCustom operation, and returns Async<Result<HttpHandler, Error list>>
    // just like custom operations.
    .PostCustomAsync(fun ctx parser helper ->
      asyncResult {
        // First set up a parser. The implication below is that User.addEmail
        // returns Async<Result<Choice<unit,Email>, Error list>>. It returns
        // unit after stage 1, Email after stage 2, and errors if there is an invalid
        // combination of parameters.
        let parser =
          parser.ForAsyncRes(
            User.addEmail, email.Optional, user.Optional, token.Optional
          )
        // Then validate the request (pass in the parser to indicate which fields
        // are used). For example, if you don't parse the resource ID, an error is
        // returned if the ID is present in the request.
        do! helper.ValidateRequest parser
        // Parse and call User.addEmail
        match! parser.ParseAsync() with
        | Choice1Of2 () ->
            // This simply returns 202 Accepted
            return helper.Return202Accepted ()
        | Choice2Of2 email ->
        		// Returns 201 with the entity, and also sets the Location header if relevant
        		return helper.ReturnCreatedEntity email
      }
    )
```

### Parsing fields and parameters

The parser works just like it does in the normal POST operation. However, you have to call `.ParseAsync()` yourself when you need it.

### Checking for client-generated IDs

Call `helper.ValidateRequest`, optionally with the parser you have created if you are parsing the request. If you have not parsed the resource ID and the ID is present in the request, an error is returned.

### Running additional setters

Call `helper.RunSettersAsync(entity)` (or `RunSettersJob`). It optionally also accepts a parser, which you should supply if you have parsed any fields. It will skip fields you have already parsed.

### Returning 201 or 202

Call `helper.ReturnCreatedEntity(entity)` to return a proper JSON:API `201 Created` response (automatically supporting sparse fieldsets and includes as usual), or `helper.Return202Accepted()` to return an empty `202 Accepted` response.

ID lookup operation
-------------------

This is not a HTTP operation; the lookup operation simply tells Felicity how to find a resource with a given ID. For example, in the request `GET /articles/123`, the lookup operation tells Felicity how to get the article with ID `123`.

The lookup operation simply needs a function `'id -> 'entity` (which may return `Async` and/or `Result`):

```f#
let lookup = define.Operation.LookupAsync(Db.Article.byId)
```

A lookup operation is required for any operations against the resource’s `self` link or its relationships’ `self` links.

The following table describes the definitions supported and not supported for modules without a lookup operation:

|      | Definition                                          | Supported |
| ---- | --------------------------------------------------- | --------- |
| ✅    | GET collection                                      | Yes       |
| ✅    | POST collection                                     | Yes       |
| ❌    | GET resource                                        | No        |
| ❌    | PATCH resource                                      | No        |
| ❌    | DELETE resource                                     | No        |
| ❌    | Custom links                                        | No        |
| ✅    | Relationship getters                                | Yes       |
| ❌    | Relationship setters (including to-many add/remove) | No        |

GET resource operation
----------------------

The GET resource operation does not accept any parameters:

```f#
let get = define.Operation.GetResource()
```

Like the lookup operation, a GET resource operation is a fundamental operation that is required for any all other operations against the resource’s `self` link or its relationships’ `self` links.

The following table describes the definitions supported and not supported for modules without a GET resource operation:

|      | Definition                                          | Supported |
| ---- | --------------------------------------------------- | --------- |
| ✅    | GET collection                                      | Yes       |
| ✅    | POST collection                                     | Yes       |
| ❌    | PATCH resource                                      | No        |
| ❌    | DELETE resource                                     | No        |
| ❌    | Custom links                                        | No        |
| ✅    | Relationship getters                                | Yes       |
| ❌    | Relationship setters (including to-many add/remove) | No        |

### Modifying the response

If you need to modify the response, e.g. to add cache headers, use `ModifyResponse`. This function allows you to modify the `HttpContext`, either directly or by using a Giraffe `HttpHandler`.

### Execution order

1. Get the context
2. Resource lookup
3. Transform the context if specified (see section TODO)
4. `ModifyResponse`

PATCH resource operation
------------------------

The PATCH operation automatically runs all field setters, and returns suitable errors for failing setters. It simply requires a function to persist the changes:

```f#
let patch = define.Operation.Patch().AfterUpdateAsync(Db.Article.save)
```

### Custom setters

You may come across the need for PATCH requests that can not be cleanly described using separate field setters. For example, a set operation may require two fields simultaneously. There are several ways to accommodate this by adapting or wrapping your domain code, but the point of Felicity is to allow you to write write idiomatic, functional F# domain code and use that directly.

You can use `AddCustomSetter` to create a custom setter where you can parse anything from the request (see section TODO for details about the request parser and its capabilities). For example, let’s say you have a resource with two attributes, `protectedValue` and `authorizationCode`, and if you want to set `protectedValue`, you have to supply `authorizationCode`, too. You could add a custom setter like this:

```f#
let patch =
  define.Operation
    .Patch()
    .AddCustomSetter(fun ctx entity parser ->
      parser
        .For(entity)
        .Add(Entity.setProtected, protectedValue, authorizationCode)
    )
    .AfterUpdate(...)
```

Here, `setProtected` has signature `ProtectedValue -> AuthCode -> Entity -> Entity`, i.e. an “immutable setter” that takes not one, but two “value arguments”. The setter is only run if `protectedValue` is present in the request, and will fail if `protectedValue` is present but `authorizationCode` is not.

If you want to make the additional argument(s) optional, simply append `.Optional` (to `authorizationCode` above) and you’ll get an `Option`-wrapped value instead. If you need higher-arity `Add` variants than what is available, please open an issue.

The fields you parse in the manner shown above do not need their own setters. Any such setters, if defined, will be skipped if they are used in the custom setter.

You can add as many custom setters as you want in a PATCH request.

### Modifying the response

If you need to modify the response, e.g. to add cache headers, use `ModifyResponse`. This function allows you to modify the `HttpContext`, either directly or by using a Giraffe `HttpHandler`.

### Returning 202 Accepted

If you need the operation to return `202 Accepted`, simply add `Return202Accepted()` to the operation definition.

### Performing pre-update work

Use `BeforeUpdate` to perform (potentially failing and/or asynchronous) work before modifying the resource. Note that this work must not cause observable state changes; the request may still fail after this stage.

### HTTP preconditions

See section TODO for how to do precondition validation (using `ETag`/`Last-Modified` and `If-Match`/`If-Unmodified-Since`) for PATCH requests.

### Execution order

1. Get the context
2. Resource lookup
3. Transform the context if specified (see section TODO)
4. Validate preconditions
5. `BeforeUpdate`
6. Custom setters
7. Normal field setters (only those not already used in custom setters)
8. `AfterUpdate`
9. `ModifyResponse`

DELETE resource operation
-------------------------

The DELETE operation simply needs the function that performs the deletion:

```f#
let delete = define.Operation.DeleteAsync(Db.Article.delete)
```

Note that if you use a `Result` returning variant and you return `Error`, no observable state changes must have taken place.

### Modifying the response

If you need to modify the response, e.g. to add cache headers, use `ModifyResponse`. This function allows you to modify the `HttpContext`, either directly or by using a Giraffe `HttpHandler`.

### Returning 202 Accepted

If you need the operation to return `202 Accepted`, simply add `Return202Accepted()` to the operation definition.

### Performing pre-update work

Use `BeforeDelete` to perform (potentially failing and/or asynchronous) work before modifying the resource. Note that this work must not cause observable state changes; the request may still fail after this stage.

### HTTP preconditions

See section TODO for how to do precondition validation (using `ETag`/`Last-Modified` and `If-Match`/`If-Unmodified-Since`) for PATCH requests.

### Execution order

1. Get the context
2. Resource lookup
3. Transform the context if specified (see section TODO)
4. Validate preconditions
5. `BeforeDelete`
6. Delete
7. `ModifyResponse`

Custom operations/links
-----------------------

Custom resource operations/links allow you to do more or less anything you want, and may be useful for operations which do not easily map to a CRUD model. You have access to the same request parser as in several other requests (see section TODO), as well as a helper that writes a JSON:API document based on a resource (automatically supporting includes and sparse fieldsets as normal). This helper returns a Giraffe `HttpHandler`, making it easy to combine with other handlers. Your operation returns `Async<Result<HttpHandler, Error list>>`, meaning that you don’t have to think about how to format a proper error response; Felicity does that for you.

```f#
let linkName =
  define.Operation
    .CustomLink()
    .Post(fun ctx parser respond entity ->
      async {
        let! someParam = parser.GetRequiredAsync(Query.Bool("someQueryParam"))
        let! updatedEntity = doSomeUpdate someParam entity
        let handler =
          respond.WithEntity updatedEntity
          >=> setHttpHeader "foo" "bar"
        return Ok handler
      }
    )
```

As defined above, the resource will have a link named `linkName`, and the URL will be the resource’s self URL plus `/linkName`.

### Conditional availability

Using `Condition`, you can specify a condition for when the operation is available. You can return either `bool` (causing Felicity to return a generic error message) or a custom `Error list`.

### Meta

You can add link meta by using `AddMeta` and `AddMetaOpt`. The former allows you to specify the meta key, value, and an optional condition for when to add it, and the latter allows you to return an `option`-wrapped value where the meta item will only be added if it is `Some`.

If links have meta, they use the `href/meta` object form; otherwise they are simple strings.

### When is the link present on the resource?

If the context is successfully transformed (if applicable; see section TODO) and the condition is `true`/`Ok`, then the link is present.

If the context is successfully transformed, the condition is `false`/`Error`, and the link has meta, then the link is present with `"href": null` and the specified meta.

If the context is not successfully transformed, or if the condition is `false`/`Error` and the link does not have any meta, the resource’s `links` member does not contain the link at all.

### HTTP preconditions

See section TODO for how to do precondition validation (using `ETag`/`Last-Modified` and `If-Match`/`If-Unmodified-Since`). This applies to POST/PATCH/DELETE requests (not GET).

### Execution order

1. Get the context
2. Resource lookup
3. Transform the context if specified (see section TODO)
4. `Condition`
5. Validate preconditions
6. Your custom operation

Operation-specific authorization
--------------------------------

As mentioned previously, the context is a good place to put your authenticated user data. However, you may have different types of authentication, e.g. anonymous (no user), normal users, and administrators.

One solution is to access control using Felicity is to design two different context types – in this case, one for all users, and one for administrators:

```f#
type Principal =
  | Anonymous
  | Normal of User
  | Admin of Administrator
  
type Context = { Principal: Principal }

type AdminContext = { Admin: Administrator }
```

If you have resources/collections that are in their entirety only accessible by administrators, you can simply use `AdminContext` as the context for these resources (remember to separately register it in `Startup.cs` and insert it in your routes, as mentioned previously).

However, you may have a resource that is available to all users of your API, but where some operations, say PATCH, are only available to administrators.

Felicity allows you to transform the context for arbitrary operations, and specify an error to be returned if the transformation fails. For example:

```f#
let unauthorized =
  Error.create 403
  |> Error.setTitle "Unauthorized"
  |> Error.setDetail "You do not have access to this operation"

let toAdminCtx = function
  | { Principal = Admin a } -> Ok { Admin = a }
  | _ -> Error [unauthorized]
```

Using this, you can define operations like this:

```f#
let patch =
  define.Operation
    .ForContextRes(toAdminCtx)
    .Patch()
    .AfterCreateAsync(fun ctx a -> Db.Article.saveForAdmin ctx.Admin a)
```

`ForContext` has several variants, including one allowing you to return an `Option`-wrapped value, where Felicity will return a generic “operation not available” error message if it returns None.

Request parser
--------------

Felicity provides a request parser that allows you to parse query parameters, headers, and response body attributes/relationships in a declarative way, and which automatically handles and combines any errors for the parsed values.

The request parser is supported by the following operations:

* GET collection
* POST collection
* DELETE resource
* Custom operations

The operations above have overloads that provide you access to the request parser.

For example, here is the GET collection operation from earlier that parses several optional filter parameters:

```f#
let getCollection =
  define.Operation
    .GetCollection(fun ctx parser ->
      parser.For(ArticleSearchArgs.empty)
        .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
        .Add(ArticleSearchArgs.setTypes, Filter.Field(articleType).List)
        .Add(ArticleSearchArgs.setOffset, Page.Offset)
        .Add(ArticleSearchArgs.setLimit, Page.Limit.Max(20))
        .BindAsync(Db.Article.search)
    )
```

And here is the POST collection operation from earlier that parses several required resource fields in the response body:

```f#
let post =
  define.Operation
    .Post(fun ctx parser -> parser.For(Article.create, author, title, body))
    .AfterCreateAsync(Db.Article.save)
```

### The underlying concept

In the general case, the parser is used to build up an object of your choice that represents the parsed arguments. You can have required parameters (passed to the function that creates the object) and optional parameters (represented as “immutable setters” for the object, just like attribute/relationship setters).

You use `parser.For` to specify either an empty object or a function that creates an object and accepts some required parameters that you then also supply in the call to `For`.

In the GET collection example above, we specify `ArticleSearchArgs.empty`, which does not require any parameters. We then add a series of optional parameters using “immutable setters” for the values parsed on the right. Finally, because `GetCollection ` requires us to supply a request parser for `Article list` and not for `ArticleSearchArgs`, we transform using `.BindAsync(Db.Article.search)`, where `Db.Article.search` has signature `ArticleSearchArgs -> Article list`.

In the POST collection example, we create a parser for the function `Article.create`, which has signature `PersonId -> ArticleTitle -> ArticleBody -> Article`. We then supply parsers for the three required arguments: The `author` relationship parses the corresponding relationship ID (a `PersonId`), `title` is an attribute that parses to `ArticleTitle`, and `body` is an attribute that parses to `ArticleBody`. In the POST request, we must return a parse for `Author`, and since `Article.create` returns `Author`, we don’t need to transform the returned value as we did with the GET collection example.

### Parsing higher-arity immutable setters

The `.Add` methods have higher-arity overloads that accept secondary parameters. An example of this was shown in the PATCH section (TODO link):

```f#
parser
  .For(entity)
  .Add(Entity.setProtected, protectedValue, authorizationCode)
```

 When the primary parameter is included (`protectedValue` above), the secondary parameters are parsed, too, and passed to the immutable setter. If the primary parameter exists but the secondary parameters do not, an error will be returned. You may append `.Optional` to the secondary parameters to make then optional and `Option`-wrapped. 

### Parsing higher arities than is currently available

If you need to parse higher arities than are currently available for the `For` or `Add` methods, please open an issue and/or submit a PR. As a workaround, you can always fall back to monadic parsing using the `AsyncRes` overloads and use a suitable `asyncResult` CE (e.g. from [FsToolkit.ErrorHandling.JobResult](https://www.nuget.org/packages/FsToolkit.ErrorHandling.JobResult/)). `JobRes` overloads are also available. For example:

```f#
parser.ForAsyncRes(
  asyncResult {
    let! a = parser.GetRequiredAsync fieldA
    let! b = parser.GetRequiredAsync fieldB
    ...
    return domainFuncTakingManyParams a b ...
  }
)
```

The only drawback to this method is that the errors won’t be combined; you will only get the first error.

### Parsing in custom operations

For standard operations as shown above, you have to return a request parser for the needed type, and not the needed type itself. This is because the request parser contains important runtime information about which parameters/fields have been parsed, which Felicity needs in order to determine e.g. if an operation supports sorting or client-generated IDs (JSON:API mandates that errors be returned if these are supplied but not supported).

However, for custom operations, you should have more freedom, and the Felicity API may not be as restrictive.

As we saw earlier, a custom operation can look like this:

```f#
let linkName =
  define.Operation
    .CustomLink()
    .Post(fun ctx parser respond entity ->
      async {
        let! someParam = parser.GetRequiredAsync(Query.Bool("someQueryParam"))
        let! updatedEntity = doSomeUpdate someParam entity
        let handler =
          respond.WithEntity updatedEntity
          >=> setHttpHeader "foo" "bar"
        return Ok handler
      }
    )
```

This demonstrates one possible way of using the parser in custom operations: The `GetRequired` overloads simply parse a single required parameter. There are also `GetOptional` overloads that returns an `Option`-wrapped value which is `None` if the parameter was not present.

The other possible way to use the parser in custom operation is to use it exactly like in the standard operations, i.e. `parser.For(…).Add(…)`, but end with `.ParseAsync()`. This returns `Async<Result<'a, Error list>>`, meaning you can bind it using `let!` in a custom operation. For example:

```f#
let linkName =
  define.Operation
    .CustomLink()
    .Post(fun ctx parser respond entity ->
      asyncResult {  // asyncResult CE from e.g. FsToolkit.ErrorHandling
        let! myArgs =
          parser.For(MyArgs.create, someAttr)
            .Add(MyArgs.setFoo, Query.Int("foo"))
            .Add(...)
            .ParseAsync()
        ...
      }
    )
```

### Parsing response body attributes and relationships

You parse attributes and relationships from the response body simply by using the attribute and relationship directly in the parser. Below, `author` is a relationship that will be parsed to `PersonId` (which is the ID type of the related resource), and `title` and `body` are attributes that will be parsed to `ArticleTitle` and `ArticleBody` (since they are the domain types of the two attributes).

```f#
let post =
  define.Operation
    .Post(fun ctx parser -> parser.For(Article.create, author, title, body))
    .AfterCreateAsync(Db.Article.save)
```

You may append `.Optional` to an attribute or relationship if you want an optional parser that returns `None` when the field is not present, though depending on the use-case it may be clearer to use the parser’s `.Add` methods to add optional parameters using “immutable setters” as shown previously. You can, for example, use this to add optional, read-only fields that may be used in POST requests but not in PATCH requests.

#### Parsing related entities and projections

For relationships, you may also append e.g. `.Related(Person.lookup)` to get a parser that returns the actual entity, and not just its ID. For example:

```f#
parser.For(Article.create, author.Related(Person.lookup))
```

Here, `Article.create` accepts a `Person` parameter instead of a `PersonId`.

But what if `Article.create` doesn’t need the whole `Person` entity, only a subset of it? Or what if it only needs the `PersonId`, but you still want Felicity to ensure that the resource exists? Then you simply define another lookup operation just for the entity projection you want, and use that instead:

```f#
let customLookup =
  Define<Context, MyPersonProjection, PersonId>()
    .Operation.LookupAsync(myProjectionLookup)
```

Above, `myProjectionLookup` has signature `PersonId -> MyPersonProjection option`. This `customLookup` is then used in `author.Related` just like `Person.lookup`. `Article.create` can then accept `MyPersonProjection` instead of `Person`. Note that you have to create a new `Define` instance because the `define` value you already have in the resource module is a different generic instantiation (has different generic parameters).

### Parsing filter query parameters

The static class `Filter` is the entry point for parsing filter query parameters.

The previously shown GET collection example shows several filter parameters:

```f#
let getCollection =
  define.Operation
    .GetCollection(fun ctx parser ->
      parser.For(ArticleSearchArgs.empty)
        .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
        .Add(ArticleSearchArgs.setTypes, Filter.Field(articleType).List)
        .Add(ArticleSearchArgs.setOffset, Page.Offset)
        .Add(ArticleSearchArgs.setLimit, Page.Limit.Max(20))
        .BindAsync(Db.Article.search)
    )
```

To filter on a resource field, use `Filter.Field` with a resource field, as shown above. The query parameter will automatically be named correctly, e.g. `filter[title]` and `filter[articleType]` for the filters shown above.

Query parameters are always strings. This means that, in general, when you use `Filter.Field` with attributes, you need to supply a function that transforms from `string` to the attribute’s serialized type (from that point, Felicity can do the rest of the parsing). For convenience, Felicity can perform transformations from `string` if this transformation is fairly standard, i.e. if the serialized field value is `string`, `bool`, `int`, or `float`.

This is not relevant for relationships, since relationship IDs are always serialized as `string`.

To filter on a related field, simply include the relationship path and the actual field to filter on:

``` f#
Filter.Field(author, Person.firstName)
```

The query parameter will be named `filter[author.firstName]`.

#### Lists

By default, `Filter.Field()` will parse a single value which may not contain commas. To parse a list of comma-separated values, simply append `.List` as shown in the example above.

#### Optional

As with parsing optional resource fields, you may append `.Optional`, but as mentioned previously, it may be clearer to instead use the parser’s `.Add` methods to add optional parameters using “immutable setters”.

#### Operators

If you want to add an “operator” to the filter parameter name, e.g. `filter[firstName][contains]`, you can use `.Operator("contains")`. This is just a parameter name change; it has no effect on the parsing.

If you use an operator that indicates the query parameter should accept a boolean value, you can append `.Bool` and the parser will be replaced with a `bool` parser. In other words, the field(s) you specify in `Filter.Field()` are then only used to construct the filter parameter’s name, not for parsing. For example, the following will parse a query parameter `filter[firstName][isNull]` which may be `true` or `false`.

```f#
Filter.Field(firstName).Operator("isNull").Bool
```

#### Filtering on nullable attributes and relationships

Query strings have no standard representation of `null` which can be used unambiguously. Therefore, Felicity only allows you to filter on non-`null` values of nullable attributes and relationships. The parsed attribute value will then not be `Option`-wrapped.

If you need a filter for whether the value is null, you can use e.g.

```f#
Filter.Field(updatedAt).Operator("isNull").Bool
```

which will give you a filter parameter called `filter[updatedAt][isNull]` accepting `true` or `false`.

#### Filtering on to-many relationships

Filtering on to-many relationships work just like to-one and to-one nullable relationships. **Each relationship type only provides information about how to parse a single string to the related resource’s ID type.** It is up to you to append `.List` to the filter if you want multiple values, and to determine what single or multiple values mean in each case (AND, OR, contains, identical, ordered/unordered, etc.).

#### Custom filter parameters

You can use `Filter.Parsed(...)` to specify fully custom names and parsing behavior. You may find it useful to use the `.Name` property of the resource fields to create the filter parameter name. For example:

```f#
Filter.ParsedRes(firstName.Name, myResultReturningParseFunction)
```

#### Filtering on polymorphic relationship IDs

You may not use polymorphic relationships in `Filter.Field`. This is because the query parameter contains just the ID value and no type information, and thus Felicity has no way of knowing which of the possible ID types it should parse the value to. This is one of the very few cases where Felicity will throw at runtime:

```f#
// Will throw at runtime, don't do this!
Filter.Field(myPolyRel)
```

Instead, you have to use a custom parser as shown above:

```f#
Filter.Parsed(myPolyRel.Name, myPolyIdParser)
```

Possible options include:

* Parse to a type that encompasses all the possible ID types, and let the rest of the code deal with the ambiguity. Using `string` will of course always work, or you could parse to another type like `GUID` or `int` if all resource types in the relationship use compatible ID types in the database. Note that if using anything else than a `GUID` (e.g. `int`) this may not be feasible, since multiple resource types may have a resource with the specified ID.

* Actually look up in the database during the parsing to see which resource type contains a match for the specified ID. This requires the IDs to be unique among all the types in the relationship. Depending on the use-case for the parsing, this may not give you much benefit over the previous method.

* Require type information in another parameter, e.g. by using the higher-arity overloads of `RequestParser.Add` (or by requiring both parameters in `RequestParser.For`). Here is a very simple example that requires `myArgsSetter` to handle the case when the type filter is not set to a valid type:

  ```f#
  parser
    .For(...)
    .Add(
      myArgsSetter,
      Filter.Parsed(myPolyRel.Name, id),
      Filter.Parsed(myPolyRel.Name + ".type", id)
  )
  ```

  This will accept a query like `?filter[myPolyRel]=someId&filter[myPolyRel.type]=someTypeName` where the string values `"someId"` and `"someTypeName"` are passed to `myArgsSetter`. This removes all ambiguity, but is more verbose for the API client.
  
  Here is a more sophisticated example the uses `Query.Enum`  so that Felicity handles the case when the type is not valid:
  
  ```f#
  let setVehicleType resId parseResId args =
    VehicleArgs.setVehicleType (parseResId resId) args
  
  let vehicleTypeMap = [
    "car", CarId.fromString >> VehicleId.Car
    "truck", TruckId.fromString >> VehicleId.Truck
  ]
  
  parser
    .For(...)
    .Add(
      myArgsSetter,
      Filter.Parsed(vehicle.Name, id),
      Query.Enum(sprintf "filter[%s.type]" vehicle.Name, vehicleTypeMap)
  )
  ```

### Parsing other query parameters

Much of what is said above about filter parameters are relevant here, too, and won’t be repeated.

#### Sort

The static `Sort` class is the entry point for parsing the JSON:API `sort` parameter.

Usually there would be a limited set of sorting options available. Use `Sort.Enum` for this. Section TODO contains relevant notes about enum parsing. You can also use `Sort.Parsed` to specify custom parsing behavior.

When parsed, you get a tuple with the parsed sort value and a boolean that indicates the sort direction. The boolean is `false` for ascending and `true` for descending. In other words, the boolean indicates whether the JSON:API `-` prefix was present on the sort column.

#### Page

The static `Page` class is the entry point for parsing the JSON:API `page[]` parameter family.

There are predefined parsers for `page[offset]`, `page[limit]`, `page[number]`, and `page[size]`. They are set up as `int` parsers with sensible limits, e.g. `offset` must be non-negative, `limit` must be positive, etc. To override the limits, append e.g. `.Min(10)` and `.Max(20)`.

#### Custom query parameters

The static `Query` class is the entry point for parsing arbitrary query parameters. `Query.Parsed` is the general method, and there are convenience methods for parsing `string`, `int`, `bool`, `float`, and string enums.

### Parsing headers

The static `Header` class is the entry point for parsing HTTP request headers. It works the same way as `Query`, described above.

### Prohibiting parameters

You can also use the request parser to disallow certain parameters, i.e. return a generic “parameter not allowed” error if they are included in the request. For example, to allow normal users to list only their own articles, and admins to list all, you can do this:

```f#
let getCollection =
  define.Operation
    .GetCollection(fun ctx parser ->
      match ctx with
      | { Principal = User u } ->
          parser.For(ArticleSearchArgs.createForUser u.AuthorId)
            .Prohibit(Filter.Field(author))
            .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
            ...
      | { Principal = Administrator a } ->
          parser.For(ArticleSearchArgs.empty)
          	.Add(ArticleSearchArgs.setAuthorId, Filter.Field(author))
            .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
            ...
    )
```

In the example above, if authenticated as a normal user and they include the `filter[author]` query parameter, they will get an error.

Conditional GET/HEAD (`ETag`/`If-None-Match`)
---------------------------------------------

Felicity automatically hashes all success responses and sets an `ETag` header accordingly. This means that clients may use `If-None-Match` to potentially get a `304 Not Modified` response if they already have an up-to-date version of the response to that particular request.

Note that this is not related to JSON:API resources; the ETag works on the HTTP level. It can therefore not be used for verifying preconditions before updating a particular resource (see below for more on that). It is only a way to avoid transferring (potentially large) response bodies the client has already seen.

Conditional POST/PATCH/DELETE (`If-Match`/`If-Unmodified-Since`)
------------------------------------------------

You can define HTTP preconditions in a resource module:

```F#
let preconditions =
  define.Preconditions
    .ETag(fun entity -> EntityTagHeaderValue.FromString false entity.ETag)
    .LastModified(fun entity -> entity.LastModified)   
```

You can supply either one or both.

Clients must then supply either the `If-Match` or the `If-Unmodified-Since` header in the request in order to perform requests that modify the resource.

These values **must be communicated to the client as resource attributes** (or meta). They will not be set as HTTP headers, because headers apply to the whole response, and preconditions as shown above are resource-specific.

You can also append `.Optional` in order to make the precondition validation optional. If so, clients may perform request without the precondition headers, but if present, they will be validated.

Resource locking
----------------

Even if you use preconditions as described above, resource operations may still conflict with each other due to thread safety. For example, if two requests for the same resource arrive at the same time, the same representation is fetched from the database and the preconditions for both requests pass. However, the operation that finishes last will overwrite the changes of the operation that finishes first.

This becomes even more problematic if persisting resources involve other non-atomic operations such as deleting or moving files on disk: The first operation may remove a file reference from your resource and delete the file, and the second operation may overwrite the newly updated DB row with conflicting data that still references the file, corrupting the system state. (Yes, I have experienced this very problem.)

To combat this problem, Felicity can easily take care of locking and queueing all write access to your resources. Only one non-safe (POST/PATCH/DELETE) request will be allowed at a time for any given resource; other requests are queued and will be executed in the order they arrived, or will get a 503 error if the lock times out.

In the simplest sense, simply append `.Lock()` to your resource definition (after `CollectionName`):

```f#
let resDef =
  define.Resource("article", resId)
    .CollectionName("articles")
    .Lock()
```

`Lock()` optionally accepts a custom timeout. The default is 10 seconds.

### Locking dependent resources

You may have resources that are “child” entities and belong to another resource (see section TODO). In this case, it is generally the “parent” resource that should be locked. For example, when modifying an `orderline`, the shared state that should be locked may be the parent `order`.

To set up this, simply use the `LockOther()` method and pass in the following three arguments:

* The parent resource definition
* A function that, given the child ID, returns the parent ID
* Optionally the relationship from the child to the parent resource (required in order to lock POST requests to create child resources)

Felicity then locks the specified parent resource instead of the child resource:

```f#
let order = define.Relationship.ToOne(Order.resDef)

let resDef =
  define.Resource("orderline", resId)
    .CollectionName("orderlines")
    .LockOther(Order.resDef, getOrderIdForOrderLine, order)
```

Above, `getOrderIdForOrderLine` has the signature `OrderLineId -> Async<OrderLine option>`. If the ID lookup function returns `None`, no locking is performed (it is then likely that the resource doesn’t exist, which means the request will fail anyway).

### External locking mechanisms

The locking demonstrated above is handled entirely within Felicity. However, you may need to plug into external locking system, e.g. because there are background operations being triggered for the same resources that are modifiable through the API, or because you need distributed locking across multiple instances of an API (see e.g. [DistributedLock](https://github.com/madelson/DistributedLock)).

In this case, use the `CustomLock` or `CustomLockOther` methods. They correspond to the variants above, but you must pass a `getLock` function. This function accepts the (strongly typed) resource ID, and should return `None` if the lock times out, or `Some` with an `IDisposable` that releases the lock when disposed.

### Limitations

Felicity locks the resource before fetching it from the database (to ensure that preconditions work correctly and that the up-to-date entity is used for all queued operations without requiring extra trips to the DB). Therefore, if you have polymorphic collections (see section TODO), Felicity doesn’t know which resource type any given ID corresponds to; it only knows the collection name and resource ID. As a consequence, you may only have one `Lock` or `LockOther` per collection name.

Furthermore, if you use `LockOther` or `CustomLockOther`, it is assumed that the related ID returned by `getId` will never change. The `getId` function is first called to get the ID to lock. Then the operation is performed. If the output of `getId` changes between checking/locking and performing the operation, locking may not work:

* If it changes between two different `Some` values, the incorrect resource is locked
* If it changes from `None` to `Some`, no locking is performed
* If it changes from `Some` to `None`, you’re probably fine

Polymorphism
------------

You may encounter the need to have collections or resources that contain several resource types.

First, I would urge you to consider whether you can change your resource model to avoid it. Polymorphic collections/relationships is a complication both in implementation and conceptually.

If you do need to use it though, Felicity has you covered. This is an advanced use-case, though, and while I have tried to make the API both as simple and flexible as I can, compile-time safety can not be guaranteed in all cases.

### Some fundamental notes

Up until now, we have considered the resource module as the fundamental unit of Felicity. We have considered each resource module  in isolation, and an underlying assumption has been that each resource module has a unique collection name.

This need not be the case. There are good reasons (namely, simplicity) for making the public API of Felicity resource-centered, but in the Felicity internals *collections* is the fundamental unit. This is needed since, generally, a collection may contain one or more resources.

There are three places where polymorphism enters the picture in Felicity.

* **Polymorphic GET collection operations.** For example, consider a collection `/vehicles` with types `truck` and `car`, represented by correspondingly named domain types and with a common `Vehicle` discriminated union that wraps them. For `GET /vehicles`, Felicity must know how to “unwrap” each `Vehicle` returned by the search to `Truck` or `Car`, and whether to use the `Truck` or `Car` resource module for that unwrapped resource.

* **Polymorphic resource lookup for a collection.** For `GET /vehicles/123`, Felicity must know how to parse `123` to  `VehicleId` it can use in the lookup. Once it obtains the `Vehicle` with that ID, the same as above applies, too (i.e., how to unwrap it and use the correct resource module).

* **Polymorphic relationships.** This is kind of a combination of the points above. If the relationship is gettable, Felicity must know how to unwrap and select the correct resource module. If the relationship is settable, Felicity must know how to parse the IDs given the relationship data item’s `type`.

### Polymorphic collections

TODO

### Polymorphic resource lookup

TODO

### Polymorphic relationships

TODO

Sideposting (a.k.a sideloading)
-------------------------------

A common complaint against JSON:API is that it does not support “sideposting”, i.e. creating multiple related resources in a single POST request.

JSON:API will likely support this in a future version, but in the meantime, Felicity already supports this in a fairly straightforward fashion.

The request should contain the necessary related resources in the `included` member, and all relevant relationships should have resource linkage with temporary IDs to indicate “what goes where”. For example:

```json
{
  "data": {
    "type": "parent",
    "relationships": {
      "child": {
        "data": {
          "type": "child",
          "id": "temporaryId"
        }
      }
    }
  },
  "included": [
    {
      "type": "child",
      "id": "temporaryId",
      "attributes": {
        "name": "some value"
      }
    }
  ]
}
```

To parse such a request, simply use the request parser for the POST operation, and append `.Included` to a relationship that you want to have sideposted. This gives you access to a nested request parser for the related entity type. For example:

```f#
let post =
  define.Operation
    .Post(fun ctx parser ->
      parser.For(
        Parent.create,
        child.Included(fun childParser ->
          childParser.For(Child.create, Child.name)
        )
      )
    )
    .AfterCreate(...)
```

Above, `Parent.create` has signature `Child -> Parent`, and `child` is the relationship to the child resource with entity type `Child`, and `Child.create`.

You can parse arbitrarily deep resource graphs this way (i.e., the included `child` resource above may have further sideposted relationships, and so on).

As with polymorphism, I would urge you to first consider whether you can change your resource model to avoid the need for sideposting. Sideposting as shown above is not covered by JSON:API, and the feature as implemented in Felicity does currently not support polymorphic relationships (there is currently no way to specify different parsers for different types).

Tips for use with “dependent” entities that are not DDD aggregate roots
-----------------------------------------------------------------------

TODO

* In `Define` (and everywhere else), make `'entity` be a tuple containing both the parent and sub-entity
* Have a domain function `Parent.withChild: 'childId -> 'parent -> 'parent * 'child` that looks up a known child in the parent (and throws if not found)
* If needed, use `define.Operation.PostBackRef` instead of `Post` to get access to the (pre-create) parent entity in `AfterCreate`

Current limitations
-------------------

TODO

* Sorting of relationships
* Relationship meta
* Resource meta
* top-level `jsonapi` member
* top-level links
* top-level meta
* sideposting for polymorphic relationships

FAQ
---

TODO

* Why `Func`
  * To help overload resolution so you don’t need as many type annotations
  * Slightly worse support for partial application (not much)
