// We use a recursive module because we have cyclic relationships. This causes F# to emit
// a warning which we can ignore.
//
// Another solution is to place the resource definitions at the top and define them as
// internal or private, and refer to them in each resource module.

module rec JsonApi
#nowarn "40"

// This module contains almost all Felicity-related code.

open Microsoft.AspNetCore.Http
open System
open Felicity
open Domain


// Here we define any custom errors we need to return. Felicity already returns helpful
// errors for almost 100 common error conditions, which you don't need to think about.
//
// You do not need to set the Error object's source pointer/parameter. Felicity will take
// care of that wherever relevant, even for your custom errors.
//
// Furthermore, all returned errors already have the 'id' property set to a random GUID,
// and all returned errors are automatically logged (including the ID, status, and
// message), so you don't need to think about that, either.

[<AutoOpen>]
module Errors =

  let unauthorized =
    Error.create 401
    |> Error.setTitle "Unauthorized"
    |> Error.setDetail "The authorization was missing or invalid for this operation"
    // You can even add headers to an error object. These headers will be returned
    // whenever a response contains this error.
    |> Error.addHeader "Foo" "Bar"

  let invalidDateTimeOffset invalidValue =
    Error.create 400
    |> Error.setTitle "Invalid query parameter value"
    |> Error.setDetailf "Could not parse '%s' as a date-time value" invalidValue


[<AutoOpen>]
module Converters =

  let parseDateTimeOffset str =
    match DateTimeOffset.tryParse str with
    | None -> Error [invalidDateTimeOffset str]
    | Some dto -> Ok dto



// Felicity allows you to map the ASP.NET Core HttpContext to a value you can access in
// all operations, called a "context" (or "ctx" for short). This value may for example
// contain authentication data. You can place anything you want here. While not
// recommended, you could even place the entire HttpContext here (if so, make sure you
// don't read the request body, since Felicity needs to control that).

type Principal =
  | Anonymous
  | Authenticated of username: string

type Context =
  { Principal: Principal }

module Context =

  // Simulate asynchronous authentication (e.g. DB or external auth service)
  let getCtx (ctx: HttpContext) =
    async {
      if false then return Error [unauthorized]
      else return Ok { Principal = Anonymous }
    }


// Our first resource module! Each module contains all the definitions for a single
// resource.

module Article =

  // First, define a definition helper like this where you specify the types for the
  // context, domain entity, and domain entity ID.
  let define = Define<Context, Article, ArticleId>()

  // Felicity needs to know how to parse and stringify the resource ID as well as how to
  // retrieve it from the domain entity.
  let id = define.Id.ParsedOpt(ArticleId.toString, ArticleId.fromString, fun a -> a.Id)

  // All resource modules must contain a resource definition specifying the resource type
  // name and ID parser. (Modules without a resource definitions are ignored entirely.)
  let resourceDef = define.Resource("article", id).CollectionName("articles")

  // Define attributes using define.Attribute. Easy to remember, right? The attributes
  // have the same name as the value ("title" below), but that can be overridden by
  // passing a final string argument to .Parsed. Just like the resource ID, Felicity must
  // know how to transform between the attribute's domain type and the type to be
  // serialized (typically a primitive such as string, int, float, bool, etc.).
  //
  // There are variants of Parsed and Set that allow parsing and setting to fail.

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

  // .Enum can be used to automatically return a useful error message with allowed values
  // if an invalid value is received. The second parameter isn't a function, but a list
  // containing string-domain pairs.

  let articleType =
    define.Attribute
      .Enum(ArticleType.toString, ArticleType.fromStringMap)
      .Get(fun a -> a.Type)
      .Set(Article.setType)

  // By not defining a setter, the attribute is read-only in PATCH requests (and in POST
  // requests unless explicitly used).
  //
  // If an attribute can be serialized and deserialized as-is (such as with
  // DateTimeOffset), use .Simple(). (If we wanted more control over the date-time
  // format, we could use .Parsed as above.)

  let createdAt =
    define.Attribute
      .Simple()
      .Get(fun a -> a.CreatedAt)

  // Nullable attributes are defined using define.Attribute.Nullable. The only difference
  // between nullable and non-nullable attributes is that nullable attributes require
  // Option<_>-wrapping on the domain side.

  let updatedAt =
    define.Attribute
      .Nullable
      .Simple()
      .Get(fun a -> a.UpdatedAt)

  // Relationships are defined using a similar syntax. You use either ToOne,
  // ToOneNullable, and ToMany, and specify the resource definition of the related
  // resource (hence the need for the top-level module in this file to be recursive). The
  // relationship data will be available using 'include' as well as at the relationship's
  // related/self links. If you define a setter, the relationship supports PATCH. To-many
  // relationships can also define Add and Remove to support POST and DELETE for its self
  // link, but this isn't used in this example API.

  let author =
    define.Relationship
      .ToOne(Person.resourceDef)
      .GetAsync(Db.Person.authorForArticle)
      .Set(Article.setAuthor)

  let comments =
    define.Relationship
      .ToMany(Comment.resourceDef)
      .GetAsync(Db.Comment.allForArticle)

  // In its most simple form, the GET collection operation just needs a list of entities.
  // However, you'll often want to allow clients to filter the results. Relevant query
  // parameters (and headers, and even response body fields as shown in POST below) can
  // be obtained using a request parser. We have a type ArticleSearchArgs that represent
  // the search parameters. We use parser.Add below to add optional parameters using
  // "immutable setters" for the ArticleSearchArgs type together with the values to set.
  // The values are, if possible, automatically parsed.

  let getCollection =
    define.Operation
      .GetCollection(fun ctx parser ->
        parser.For(ArticleSearchArgs.empty)
          // Filter.Field(title) will give a filter parameter called filter[title].
          .Add(ArticleSearchArgs.setTitle, Filter.Field(title))
          // Add .List after the filter definition to allow multiple comma-separated
          // values. The setter must then of course accept a list.
          .Add(ArticleSearchArgs.setTypes, Filter.Field(articleType).List)
          // Two new things here. Firstly, the 'created' attribute is serialized as a
          // DateTimeOffset, but all query parameters are strings. As a convenience,
          // Felicity can handle query parameter value conversion to int/float/bool
          // parameters since these have reasonable default conversion, but for other
          // types, such as DateTimeOffset, you must supply your own converter. (If, as
          // mentioned above the 'created' attribute definition, we wanted more control
          // and serialized the attribute to/from a string, this extra step would not be
          // needed here.) Secondly, we add an operator "ge" (greater-equals) to the
          // query parameter name. This means the query parameter will be called
          // filter[createdAt][ge]. The operator only affects the name of the parameter.
          .Add(ArticleSearchArgs.setCreatedAfter, Filter.Field(createdAt, parseDateTimeOffset).Operator("ge"))
          .Add(ArticleSearchArgs.setCreatedBefore, Filter.Field(createdAt, parseDateTimeOffset).Operator("le"))
          // The Sort class is used to parse the 'sort' query parameter. For example, it
          // can be parsed to a DU representing the fields available for sorting. It is
          // parsed to 'a * bool where 'a is the domain type and the bool indicates
          // whether to sort descending ('-' before the sort field).
          .Add(ArticleSearchArgs.setSort, Sort.Enum(ArticleSort.fromStringMap))
          // The Page class has parsers for the 'page[]' query parameter, parsing values
          // as ints and checking that they are within sensible default bounds (e.g.
          // non-negative or positive, depending on which page parameter it is). The
          // limits can be overridden as shown below.
          .Add(ArticleSearchArgs.setOffset, Page.Offset)
          .Add(ArticleSearchArgs.setLimit, Page.Limit.Max(20))
          // Now we have a parser for ArticleSearchArgs. We need a parser for a list of
          // domain entities, so we call the search function.
          .BindAsync(Db.Article.search)
      )

  // A POST operation creates an entity using the specified function. Below, the related
  // author's ID, title, and body are all required when creating a person. We pass in the
  // relevant attributes and relationships, and everything is parsed and passed to the
  // create function. (If no requried parameters are needed, Post also has simpler
  // overloads.)
  //
  // After calling the 'create' function, the POST operation automatically runs all
  // attribute/relationship setters for any additional fields present in the request,
  // just like it would do for PATCH.
  //
  // The create function and setters must all be "pure" in the sense that they must not
  // cause observable state changes, since the request may still fail up until the last
  // setter has run. In AfterCreate we specify the function that persists the entity.

  let post =
    define.Operation
      .Post(fun ctx parser -> parser.For(Article.create, author, title, body))
      .AfterCreateAsync(Db.Article.save)

  // The lookup operation tells Felicity how to get a resource by ID for requests like
  // GET /articles/123 and is required if you define any resource-specific operation
  // (GET/PATCH/DELETE resource, POST/DELETE to-many relationship self links, and custom
  // links). You'll get an exception at startup if you forget to define it in a module
  // where you have such resource-specific operations.

  let lookup = define.Operation.LookupAsync(Db.Article.byId)

  // A GET resource operation allows operations like GET /articles/123 and can be defined
  // as simply as this (the actual lookup is performed using the lookup operation defined
  // above). You can also chain further calls after Get() (or any other operation) to
  // modify it. A GET operation is required if you have any other resource-specific
  // operation (GET/PATCH/DELETE resource, POST/DELETE to-many relationship self links,
  // and custom links).

  let get =
    define.Operation
      .GetResource()

  // A PATCH resource operation enables operations like PATCH /articles/123.

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



module Person =

  let define = Define<Context, Person, PersonId>()

  let id = define.Id.ParsedOpt(PersonId.toString, PersonId.fromString, fun a -> a.Id)

  let resourceDef = define.Resource("person", id).CollectionName("persons")

  let firstName =
    define.Attribute
      .Parsed(FirstName.toString, FirstName.fromString)
      .Get(fun p -> p.FirstName)
      .Set(Person.setFirstName)

  let lastName =
    define.Attribute
      .Parsed(LastName.toString, LastName.fromString)
      .Get(fun p -> p.LastName)
      .Set(Person.setLastName)

  let twitter =
    define.Attribute
      .Nullable
      .Parsed(TwitterHandle.toString, TwitterHandle.fromString)
      .Get(fun p -> p.Twitter)
      .Set(Person.setTwitter)

  let gender =
    define.Attribute
      .Nullable
      .Enum(Gender.toString, Gender.fromStringMap)
      .Get(fun p -> p.Gender)
      .Set(Person.setGender)

  let articles =
    define.Relationship
      .ToMany(Article.resourceDef)
      .GetAsync(Db.Article.allByAuthor)

  let getCollection =
    define.Operation
      .GetCollection(fun ctx parser ->
        parser.For(PersonSearchArgs.empty)
          .Add(PersonSearchArgs.setFirstName, Filter.Field(firstName))
          .Add(PersonSearchArgs.setLastName, Filter.Field(lastName))
          // There is no standard representation of a null query parameter value, so if
          // you want to filter on a nullable attribute value, you currently have to use
          // Filter.FieldAsNonNullable. If you need a filter for whether the value is
          // null, you can use e.g.
          //
          //   Filter.FieldAsNonNullable(twitter).Operator("isNull").Bool
          //
          // which will give you a filter parameter called filter[twitter][isNull]
          // accepting "true" or "false".
          .Add(PersonSearchArgs.setTwitter, Filter.FieldAsNonNullable(twitter))
          .Add(PersonSearchArgs.setGenders, Filter.FieldAsNonNullable(gender).List)
          .Add(PersonSearchArgs.setSort, Sort.Enum(PersonSort.fromStringMap))
          .Add(PersonSearchArgs.setOffset, Page.Offset)
          .Add(PersonSearchArgs.setLimit, Page.Limit.Max(20))
          .BindAsync(Db.Person.search)
      )

  let post =
    define.Operation
      .Post(fun ctx parser -> parser.For(Person.create, firstName, lastName))
      .AfterCreateAsync(Db.Person.save)

  let lookup = define.Operation.LookupAsync(Db.Person.byId)

  let get =
    define.Operation
      .GetResource()

  let patch =
    define.Operation
      .Patch()
      .AfterUpdateAsync(Db.Person.save)

  let delete =
    define.Operation
      .DeleteAsync(Db.Person.delete)



module Comment =

  let define = Define<Context, Comment, CommentId>()

  let id = define.Id.ParsedOpt(CommentId.toString, CommentId.fromString, fun a -> a.Id)

  let resourceDef = define.Resource("comment", id).CollectionName("comments")

  let body =
    define.Attribute
      .Parsed(CommentBody.toString, CommentBody.fromString)
      .Get(fun c -> c.Body)
      .Set(Comment.setBody)

  let createdAt =
    define.Attribute
      .Simple()
      .Get(fun c -> c.CreatedAt)

  let updatedAt =
    define.Attribute
      .Nullable
      .Simple()
      .Get(fun c -> c.UpdatedAt)

  let author =
    define.Relationship
      .ToOne(Person.resourceDef)
      .GetAsync(Db.Person.authorForComment)

  let article =
    define.Relationship
      .ToOne(Article.resourceDef)
      .GetAsync(Db.Article.forComment)

  let getCollection =
    define.Operation
      .GetCollection(fun ctx parser ->
        parser.For(CommentSearchArgs.empty)
          .Add(CommentSearchArgs.setAuthorId, Filter.Field(author))
          // This will parse a query parameter named filter[author.firstName], using the
          // information in Person.firstName for parsing the value.
          .Add(CommentSearchArgs.setAuthorFirstName, Filter.Field(author, Person.firstName))
          .Add(CommentSearchArgs.setSort, Sort.Enum(CommentSort.fromStringMap))
          .Add(CommentSearchArgs.setOffset, Page.Offset)
          .Add(CommentSearchArgs.setLimit, Page.Limit.Max(20))
          .BindAsync(Db.Comment.search)
      )

  // Since the Comment.create requires an article object and not just an article ID, we
  // use the article relationship's .Related function, which accepts a lookup operation
  // for the related resource type.

  let post =
    define.Operation
      .Post(fun ctx parser -> parser.For(Comment.create, author, article.Related(Article.lookup), body))
      .AfterCreateAsync(Db.Comment.save)

  let lookup = define.Operation.LookupAsync(Db.Comment.byId)

  let get =
    define.Operation
      .GetResource()

  let patch =
    define.Operation
      .Patch()
      .AfterUpdateAsync(fun c -> async {
        let c = c |> Comment.setUpdated (Some DateTimeOffset.Now)
        do! Db.Comment.save c
        return c
      })

  let delete =
    define.Operation
      .DeleteAsync(Db.Comment.delete)
