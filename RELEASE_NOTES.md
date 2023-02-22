Release notes
==============

### Unreleased

* Fixed `ArgumentOutOfRangeException` when logging request body after deserialization fails
  and `LogInvalidJsonRequestBodies` is used with `maxSize` set

### 0.21.7 (2023-01-24)

* Reduced memory usage for responses with very many resources
* Updated FSharp.SystemTextJson from 1.0.7 to 1.1.23

### 0.21.6 (2022-12-07)

* Added support for logging request bodies with invalid JSON using `LogInvalidJsonRequestBodies` when configuring
  Felicity. See the method's doc comment for more details.
* Updated FSharp.SystemTextJson from 1.0.6 to 1.0.7

### 0.21.5 (2022-11-09)

* Added `Filter.Field` overloads accepting parsers that return `Result<_, string>` and `Result<_, string list>`

### 0.21.4 (2022-11-08)

* Fixed 500 Internal Server Error when primary data contains duplicate resources in GET collection and GET to-many
  relationship related. For these operations as well as GET to-many relationship self, now logs a warning instead (since
  this should not happen in client code) and returns distinct resources.

### 0.21.3 (2022-11-02)

* The invalid value is now included in the error message for attribute parse errors. The value is limited to 200
  characters. * Parse error messages for attributes defined with overloads that return user-defined error messages (
  e.g. `Result<_, string>`) are generally of the
  form `Attribute 'name' got invalid value 'invalidValue': <User-defined error message>`

### 0.21.2 (2022-10-28)

* Fixed inadvertently returning a 500 error instead of a 400 error when a request has `null` items in a to-many
  relationship's `data` array or in a resource identifier collection document's primary `data` array.

### 0.21.1 (2022-10-09)

* Updated FSharp.SystemTextJson from 0.19.13 to 1.0.6. There is no change in behavior in Felicity, but note that this
  comes with a breaking change if you use it in other places: When deserializing missing fields of type `option`
  or `voption`, an error will now be returned instead of deserializing to `null`. Either wrap such fields
  in `Skippable`, or enable the option `IgnoreNullValues = true`. You can
  find [more details here](https://tarmil.fr/article/2022/9/25/systemtextjson-v1.0).

### 0.20.12 (2022-09-18)

* Fix: Strict mode now correctly recognizes query parameters parsed in arity 2 `RequestParserHelper.For...` methods.

### 0.20.11 (2022-09-17)

* Fix: Make strict mode work with query parameters parsed using request parser's `ParseAsync`/`ParseTask`
* Added `SkipLink` to custom operations, which will make Felicity not add the link to the resource's `links` object.
  Using this is [required in order to be JSON:API compliant](https://github.com/json-api/json-api/issues/1656), but it
  is not enabled by default for backward compatibility reasons.
* Now supports strict mode validation of query parameters in custom operations using `ValidateStrictModeQueryParams` (
  search the documentation for details)

### 0.20.10 (2022-09-16)

* Fix: Make strict mode ignore link skip query parameters (as set up by `SkipStandardLinksQueryParamName`
  and `SkipCustomLinksQueryParamName`)

### 0.20.9 (2022-09-14)

* Added support for strict mode using `EnableUnknownFieldStrictMode` and `EnableUnknownQueryParamStrictMode` when
  configuring Felicity. These methods will cause errors to be returned or warnings to be logged when encountering
  unknown fields and query parameters, respectively.
* Added a new method `RequireExplicitInclude` to attributes which makes them be present in the response only if
  explicitly specified using sparse fieldsets
* Improved errors returned when parsing values in query, header, or body:
  * Parse errors for comma-separated query parameters now indicate which item had an invalid value
  * Parse errors for query parameters now always have the parameter name in the message (in addition to `source.param`)
  * Parse errors for query parameters, headers, and resource IDs (not attributes) always have an error message that
    includes the invalid value
  * Parse error messages for query parameters, headers, and resource IDs defined with overloads that return user-defined
    error messages (e.g. `Result<_, string>`) are generally of the
    form `Query parameter 'name' got invalid value 'invalidValue': <User-defined error message>`
* Added overloads of attribute and relationship `Get...Skip` methods without `'ctx`

### 0.20.8 (2022-08-24)

* Added a new method `SkipRelationshipIf` to relationships that allows specifying when a relationship will be entirely
  omitted from the resource (i.e., no relationship links will be present). Use this together with the `Get...Skip`
  methods. If not (as has always been the case), the links will be present, but `GET` operations against them will
  return errors if the relationship's getter returns `Skip`.

### 0.20.7 (2022-08-18)

* Errors returned for invalid JSON no longer uses the exception message, which means less implementation details are
  leaked (but the message may be somewhat less useful)

### 0.20.6 (2022-08-18)

* The error response log message now contains the error's `source`

### 0.20.5 (2022-08-17)

* Custom operations now have methods for disabling the standard validation of the `Accept` and `Content-Type` request
  headers as well as query parameter names that are illegal according to JSON:API. Consider using these if the custom
  link operation does not conform to JSON:API.

### 0.20.4 (2022-08-16)

* Now supports tracking usage of resource fields. Search the documentation for "tracking" for details.

### 0.20.3 (2022-08-11)

* The error `detail` returned when a request body contains a relationship with a resource identifier pointing to a
  non-existent resource now contains the resource `type` and `id`.

### 0.20.2 (2022-06-21)

* Updated FSharp.SystemTextJson from 0.18.24 to 0.19.13

### 0.20.1 (2022-06-13)

* Updated FSharp.SystemTextJson from 0.17.4 to 0.18.24

### 0.20.0 (2022-05-02)

* Removed Hopac dependency in favor of native tasks. The primary reason is that Hopac doesn't play nice
  with `System.Diagnostics.Activity`'s call context flowing, breaking e.g. distributed tracing.
* Removed previously deprecated methods

### 0.19.0 (2022-04-19)

* Updated Giraffe from 5.0.0 to 6.0.0

### 0.18.3 (2022-02-28)

* Added context transformation overloads (`ForContext` and `MapSetContext`) accepting the current entity for all
  entity-specific operations (GET, PATCH, DELETE, custom operations, attribute/relationship setters, and `Set2`).
* Added `Filter` convenience methods to parse known types (similar to `Query`, but wraps the name
  in `filter[...]`): `String`, `Bool`, `Int`, `Float`, `DateTime`, `DateTimeOffset`,
  and `DateTimeOffsetAllowMissingOffset`

### 0.18.2 (2022-02-11)

* Fixed bug introduced in 0.18.1 where main resources that were also included in a relationship could be `null` due to a
  race condition in the response builder

### 0.18.1 (2022-02-03)

* Added the ability to configure query parameters that, if present, will cause links to be omitted from the response.
  Search the documentation for `SkipStandardLinksQueryParamName` or `SkipCustomLinksQueryParamName` for details.
* Performance improvements in response building, particularly in collection responses and with many shared included
  resources (actual improvement varies greatly by response characteristics, but tests indicate a 15-30% time reduction
  and 20-45% reduced memory consumption)

### 0.18.0 (2021-11-29)

* Now targets only .NET 6
* Updated Hopac from 0.5.0 to 0.5.1

### 0.17.10 (2021-11-24)

* Removed ignored call to `GetLinkageIfNotIncluded` for to-many relationships that return `Skip`

### 0.17.9 (2021-11-11)

* Added `Error.appendPointer` to append a value to an error's (current or future) `source.pointer`. This can be used
  e.g. to specify a sub-path for object- or array-valued attributes while still relying on Felicity's automatic error
  pointers.

### 0.17.8 (2021-11-04)

* It is now possible to use both `BaseUrl` and `RelativeJsonApiRoot` together in order to place the JSON:API endpoints
  at a different path than the one specified in the base URL. See the documentation for details.

### 0.17.7 (2021-11-02)

* Fixed startup error when using `BaseUrl` with a URL without path

### 0.17.6 (2021-11-02)

* `verifyPathCase` now correctly handles the path `/`

### 0.17.5 (2021-10-27)

* Added `.AllowCommas` to single filters to allow commas in the query parameter value (can be useful for filters with
  user input)

### 0.17.4 (2021-10-26)

* Added optional argument to `UseJsonApiEndpoints` to modify endpoints, e.g. using Giraffe’s `applyBefore`
  and `applyAfter`.

### 0.17.3 (2021-10-26)

* Improved error message if call to `IApplicationBuilder.UseJsonApiEndpoints` has no explicit type parameter, or if the
  context type is not registered using `IServiceCollection.AddJsonApi`

### 0.17.2 (2021-10-26)

* `Set2` and `Set2SameNull` now works correctly with context mapping (`define.Operation.ForContext(...)`)

### 0.17.1 (2021-10-26)

* Added `define.Operation.Set2` and Added `define.Operation.Set2SameNull` to set two fields at the same time. Use this
  as a better alternative to `CustomSetter` if the only thing you want is to set two fields simultaneously.
* Documented important limitations of `CustomSetter`. See documentation for details.

### 0.17.0 (2021-10-25)

* **Breaking:** Felicity now uses Giraffe.EndpointRouting (and, by extension, ASP.NET Core’s built-in endpoint routing).
  To migrate:

  * Remove the `jsonApi<'ctx>` handler from your Giraffe routes
  * Call `.AddRouting()` in `ConfigureServices`
  * Call `.UseRouting().UseJsonApiEndpoints<MyContextType>()` in `Configure`

  You are of course free to continue using non-Felicity Giraffe routes/handlers or any other routes like before.

* Added `Routing.verifyPathCase`; see the documentation for details

### 0.16.4 (2021-10-13)

* Fixed a bug where `CustomLock` would cause a `503` lock timeout response to be returned instead of the correct `404`
  response when the URL contained an invalid resource ID.
* Fixed a bug where `LockOtherForModification` would cause the other resource’s creation lock to be called when the URL
  contained an invalid resource ID.
* Fixed a bug where `LockOtherForResourceCreation` would cause the other resource’s creation lock to be called when the
  relationship was null.

### 0.16.3 (2021-10-01)

* Fixed a bug where `LockOtherForModification` and `LockOtherForCreation` would be invoked when they should not
* Fixed a bug where `LockOtherForModification` and `LockOtherForCreation` would invoke incorrect locks on the other
  resource

### 0.16.2 (2021-09-29)

* Added `MapSetContext` to attributes and relationships

### 0.16.1 (2021-09-02)

* Added higher-arity request parser overloads

### 0.16.0 (2021-09-02)

* Split `LockOther` into `LockOtherForResourceCreation` and `LockOtherForModification` to avoid overload resolution
  problems when only using one of the optional arguments. This also made the API more composable.

### 0.15.3 (2021-06-25)

* Will now remove duplicate sort columns in `sort` query parameters. For example, `sort=a,-a,-b,a,b` will be converted
  to `sort=a,-b` before parsing. This is done because 1) duplicate sort columns will logically never influence search
  results (for well-behaved servers), and 2) duplicate sort columns may cause errors in some databases, e.g. SQL Server.
  This is a non-breaking change unless the server does something weird based on duplicate sort columns.

### 0.15.2 (2021-06-16)

* Now supports checking preconditions on resource creation requests. See new POST operation methods.

### 0.15.1 (2021-06-09)

* Now allows using nullable relationships in `LockOther`. Due to F# overload resolution limitations, if your resource
  modules are recursive, you likely need to define the relationships (whether nullable or not) above where you use them
  in `LockOther`.

### 0.15.0 (2021-05-31)

* Updated for Giraffe 5.0. As a consequence, Felicity now only targets .NET 5.

### 0.14.13 (2021-05-31)

* Added missing method `define.Resource.Polymorphic().ResolveId()`, which is required when
  using `GetLinkageIfNotIncluded` with a polymorphic relationship.

### 0.14.12 (2021-05-31)

* Relationship links are on longer present on resources with collection names but not GET resource operations

### 0.14.11 (2021-04-09)

* Added possibility of supplying response-level meta by using `GetMeta` in `AddJsonApi` in startup code. This lets you
  specify a function `'ctx -> Map<obj, string>`, with the implication that `'ctx` should be mutable and the meta added
  during the request processing.

### 0.14.10 (2021-03-23)

* Fixed some internal errors being returned with the same error `id` for each occurrence

### 0.14.9 (2021-03-09)

* Add `CustomResourceCreationLock` to support custom locking of resource creation operation (like `POST /articles`).
  This complements the existing `CustomLock`, which locks everything except resource creation.

### 0.14.8 (2021-02-25)

* Improved certain error messages

### 0.14.7 (2021-01-28)

* Added `SetOrder` method to all fields to allow specifying the order in which fields should be set during POST
  collection and PATCH resource requests

### 0.14.6 (2021-01-25)

* Added `AsNonNullable` and `AsNonNullableOptional` to nullable attributes to require a non-null value in request
  parsing

### 0.14.5 (2021-01-25)

* Added higher-arity request parser overloads

### 0.14.4 (2020-11-26)

* Now supports taking multiple locks per resource (see documentation)

### 0.14.3 (2020-11-23)

* Updated dependencies

### 0.14.2 (2020-11-05)

* Fixed bug introduced in 0.14.0 where SimpleDateTimeOffset attributes were surrounded by extra double quotes during
  serialization

### 0.14.1 (2020-11-02)

* Resource linkage may now be included by default even if the actual resource is not included

### 0.14.0 (2020-11-02)

* **Breaking:** `SimpleDateTimeOffset` attributes now require an offset (e.g. `Z` or `+02:00`) when parsed. The same
  goes for filter query parameters for these attributes. As a side-effect, `SimpleDateTimeOffset` attributes are now
  serialized/deserialized as `string`, not `DateTimeOffset`. Use `SimpleDateTimeOffsetAllowMissingOffset` to keep the
  old behavior where values without offset assume the server’s current offset.
* **Breaking:** `Query.DateTimeOffset` now requires an offset when parsed. Use `Query.DateTimeOffsetAllowMissingOffset`
  for the old behavior where values without offset use the server’s current offset.
* Update `FSharp.SystemTextJson` to v0.14.8

### 0.13.1 (2020-10-27)

* Added more `LockOther` overloads

### 0.13.0 (2020-10-27)

* **Breaking:** Simplified and improved resource locking
  * `CustomLockOther` is removed
  * `LockOther` now simply delegates to the “parent” resource lock (recursively if needed), using the specified
    parameters to support POST collection operations (parent relationship) and/or all other operations (child-to-parent
    ID lookup)
  * Allow locking resources without collection names (e.g. when locking parent resources that are only accessible
    through relationships)

### 0.12.3 (2020-10-16)

* Treat fields/params as consumed when using the one-off `GetRequired`/`GetOptional`

### 0.12.2 (2020-09-30)

* Fix `DateTime` and `DateTimeOffset` query parsing

### 0.12.1 (2020-09-29)

* Enabled lookup of arbitrary types in relationship `Set`, `SetAll`, and `Add` overloads accepting a resource lookup (
  was previously restricted to the relationship’s related entity type, now allows lookups of e.g. simpler resource
  projections, or simple existence checks for the related IDs)

### 0.12.0 (2020-09-25)

* **Breaking:** Renamed `define.Attribute.Simple` to `SimpleUnsafe` and added variants for well-known types such
  as `SimpleString`, `SimpleDateTimeOffset`, etc.
* Added `DateTime` and `DateTimeOffset` as built-in query parser target types

### 0.11.2 (2020-09-05)

* Added support for external lock mechanisms

### 0.11.1 (2020-09-02)

* Fix 500 error when deserializing `null` to a reference-typed non-nullable attribute (now returns a more helpful error)

### 0.11.0 (2020-08-28)

* Extended resource locking/queueing to support POST requests to create child resources. Pass in the parent relationship
  to `.LockOther()` to enable this.

### 0.10.2 (2020-08-22)

* Added resource locking/queueing to ensure thread safety; see the documentation for details

### 0.10.1 (2020-08-10)

* Added more `AfterModifySelf` overloads to relationships

### 0.10.0 (2020-07-27)

* Updated FSharp.SystemTextJson dependency

### 0.9.3 (2020-07-27)

* Supported `include` query parameter for all relationship `self` URL operations

### 0.9.2 (2020-07-03)

* Added `Error.setSourceParam` and `Error.setSourcePointer`

### 0.9.1 (2020-07-02)

* Fixed `AddConstraint` overload resolution

### 0.9.0 (2020-06-17)

* Breaking: `RequestParserHelper.GetRequired` and `.GetOptional` are renamed to `GetRequiredJob` and `GetOptionalJob`
* Added `RequestParserHelper.GetRequiredAsync` and `.GetOptionalAsync`
* Base URL no longer needs to be specified in startup code
* Can add a relative JSON:API handler root path instead of the full base URL in startup code if the `jsonApi` handler is
  not at the root
* Updated `System.Text.Json` from 4.7.1 to 4.7.2

### 0.8.5 (2020-06-16)

* Fix nullable relationship request parser interpreting null data as missing relationship

### 0.8.4 (2020-06-08)

* More robust workaround workaround
  for [giraffe-fsharp/Giraffe#24](https://github.com/giraffe-fsharp/Giraffe/issues/424)

### 0.8.3 (2020-06-05)

* Added workaround for [giraffe-fsharp/Giraffe#24](https://github.com/giraffe-fsharp/Giraffe/issues/424)

### 0.8.2 (2020-05-21)

* Enabled lookup of arbitrary types in `myRelationship.Related()` parsers (was previously restricted to the
  relationship’s related entity type, now allows lookups of e.g. simpler resource projections, or simple existence
  checks for the related IDs)

### 0.8.1 (2020-04-29)

* Added additional query/header parsing overloads

### 0.8.0 (2020-04-29)

* Removed `Filter.FieldAsNonNullable` to simplify field query parsing; just use `Filter.Field` instead

### 0.7.7 (2020-04-29)

* Fixed nullable attributes in request parser

### 0.7.6 (2020-04-24)

* Improved query param helper overload resolution

### 0.7.4 (2020-04-24)

* Added arity 6 overloads to request parser

### 0.7.3 (2020-04-23)

* Add `.AsNonNullable` to a nullable to-one relationship’s related getter (used in request parsing) to require a
  non-null related resource

### 0.7.2 (2020-04-14)

* Update Giraffe version

### 0.7.1 (2020-04-06)

* Update and lock FSharp.SystemTextJson version

### 0.7.0 (2020-04-03)

* Breaking bugfix: For operations with transformed context (`.ForContext(...)`),
  the `ResourceParserHelper`/`ResourceParser` is now typed to the original context, not the mapped context, so that it
  actually can parse the resource’s fields (which are typed to the original context)

### 0.6.0 (2020-04-03)

* Breaking: Renamed `RequestParser.Parse` to `ParseJob` and added `ParseAsync`
* Breaking: Removed the “related setter” versions of `ToManyRelationship.Remove`. These violated the JSON:API spec; they
  returned errors when the related resources were not found, but the spec requires a success response even if the
  members are already removed from the relationship.
* Added `PostCustom` operation for use-cases where the strict “create entity → persist entity” workflow of the
  normal `Post` operation doesn’t work
* Fixed error for undefined resource-specific operations when the resource is not found

### 0.5.4 (2020-03-24)

* Don't validate fallthrough requests

### 0.5.3 (2020-03-23)

* Fixed serialization when using multiple context types
* Fixed routing when using multiple context types
* Added `Option` variants of `Filter.Field`, `Query.Parsed`, `Header.Parsed`, and `Sort.Parsed`

### 0.5.0 (2020-03-23)

* Huge performance improvements – Felicity overhead reduced by ~90% for very large responses, ~20% for small responses
* Add dependency on Hopac and use `Job` internally (this gave most of the performance improvements)
* Add `Job` overloads as alternatives to `Async`
* Add `string` overload to `JsonApiConfigBuilder.BaseUrl`
* Breaking: Added `Async` suffix to custom operation's `Get`, `Post`, `Patch`, and `Delete`

### 0.4.4 (2020-03-19)

* Added arity 5 overloads to request parser

### 0.4.3 (2020-03-18)

* Added support for sideposting (creating multiple related resources in a single POST request)

### 0.4.2 (2020-03-16)

* Added ability to create custom setters in PATCH operation
* Added higher-arity `Add` overloads to request parser for functions that require more than one value parameter

### 0.4.1 (2020-03-13)

* Added additional context-less `AfterUpdate` overloads to PATCH operation

### 0.4.0 (2020-03-12)

* Updated dependency FSharp.SystemTextJson
* Removed own `Skippable` implementation in favor of the one from FSharp.SystemTextJson (in
  namespace `System.Text.Json.Serialization`)
* Fail on startup if operations are missing required persistence functions
* Added support for optional precondition validation

### 0.3.3 (2020-03-05)

* Added `define.Operation.PostBackRef` to provide access to a pre-create "parent entity" in `AfterCreate` (using a
  relationship back-reference in the POST request)

### 0.3.2 (2020-03-04)

* Added some missing attribute overloads

### 0.3.1 (2020-03-03)

* Allow filtering on the resource ID

### 0.3.0 (2020-03-03)

* **Breaking:** POST, PATCH, and DELETE operations now have a single `ModifyResponse` member instead of separate members
  for 202 Accepted and normal responses
* Allow specifying `Async` constraints and constraint lists

### 0.2.0 (2020-02-28)

* Added entity-returning overloads for PATCH's `BeforeUpdate`, DELETE's `BeforeDelete`, and
  relationships' `BeforeModifySelf`

- Breaking: Made `BeforeModifySelf` use `Func`

### 0.1.3 (2020-02-25)

* Added `SetNonNull` for nullable attributes and relationships that may return null, but not be set to null

### 0.1.2 (2020-02-18)

* Improve some exception messages

### 0.1.1 (2020-02-17)

* Return ETag for HEAD requests

### 0.1.0 (2020-02-17)

* Initial release
