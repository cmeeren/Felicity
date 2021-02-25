Release notes
==============

### 0.14.8 (2021-02-25)

* Improved certain error messages

### 0.14.7 (2021-01-28)

* Added `SetOrder` method to all fields to allow specifying the order in which fields should be set during POST collection and PATCH resource requests

### 0.14.6 (2021-01-25)

* Added `AsNonNullable` and `AsNonNullableOptional` to nullable attributes to require a non-null value in request parsing

### 0.14.5 (2021-01-25)

* Added higher-arity request parser overloads

### 0.14.4 (2020-11-26)

* Now supports taking multiple locks per resource (see documentation)

### 0.14.3 (2020-11-23)

* Updated dependencies

### 0.14.2 (2020-11-05)

* Fixed bug introduced in 0.14.0 where SimpleDateTimeOffset attributes were surrounded by extra double quotes during serialization

### 0.14.1 (2020-11-02)

* Resource linkage may now be included by default even if the actual resource is not included

### 0.14.0 (2020-11-02)

* **Breaking:** `SimpleDateTimeOffset` attributes now require an offset (e.g. `Z` or `+02:00`) when parsed. The same goes for filter query parameters for these attributes. As a side-effect, `SimpleDateTimeOffset` attributes are now serialized/deserialized as `string`, not `DateTimeOffset`. Use `SimpleDateTimeOffsetAllowMissingOffset` to keep the old behavior where values without offset assume the server’s current offset.
* **Breaking:** `Query.DateTimeOffset` now requires an offset when parsed. Use `Query.DateTimeOffsetAllowMissingOffset` for the old behavior where values without offset use the server’s current offset.
* Update `FSharp.SystemTextJson` to v0.14.8

### 0.13.1 (2020-10-27)

* Added more `LockOther` overloads

### 0.13.0 (2020-10-27)

* **Breaking:** Simplified and improved resource locking
  * `CustomLockOther` is removed
  * `LockOther` now simply delegates to the “parent” resource lock (recursively if needed), using the specified parameters to support POST collection operations (parent relationship) and/or all other operations (child-to-parent ID lookup)
  * Allow locking resources without collection names (e.g. when locking parent resources that are only accessible through relationships)

### 0.12.3 (2020-10-16)

* Treat fields/params as consumed when using the one-off `GetRequired`/`GetOptional`

### 0.12.2 (2020-09-30)

* Fix `DateTime` and `DateTimeOffset` query parsing

### 0.12.1 (2020-09-29)

* Enabled lookup of arbitrary types in relationship `Set`, `SetAll`, and `Add` overloads accepting a resource lookup (was previously restricted to the relationship’s related entity type, now allows lookups of e.g. simpler resource projections, or simple existence checks for the related IDs)

### 0.12.0 (2020-09-25)

* **Breaking:** Renamed `define.Attribute.Simple` to `SimpleUnsafe` and added variants for well-known types such as `SimpleString`, `SimpleDateTimeOffset`, etc.
* Added `DateTime` and `DateTimeOffset` as built-in query parser target types

### 0.11.2 (2020-09-05)

* Added support for external lock mechanisms

### 0.11.1 (2020-09-02)

* Fix 500 error when deserializing `null` to a reference-typed non-nullable attribute (now returns a more helpful error)

### 0.11.0 (2020-08-28)

* Extended resource locking/queueing to support POST requests to create child resources. Pass in the parent relationship to `.LockOther()` to enable this.

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
* Can add a relative JSON:API handler root path instead of the full base URL in startup code if the `jsonApi` handler is not at the root
* Updated `System.Text.Json` from 4.7.1 to 4.7.2

### 0.8.5 (2020-06-16)

* Fix nullable relationship request parser interpreting null data as missing relationship

### 0.8.4 (2020-06-08)

* More robust workaround workaround for [giraffe-fsharp/Giraffe#24](https://github.com/giraffe-fsharp/Giraffe/issues/424)

### 0.8.3 (2020-06-05)

* Added workaround for [giraffe-fsharp/Giraffe#24](https://github.com/giraffe-fsharp/Giraffe/issues/424)

### 0.8.2 (2020-05-21)

* Enabled lookup of arbitrary types in `myRelationship.Related()` parsers (was previously restricted to the relationship’s related entity type, now allows lookups of e.g. simpler resource projections, or simple existence checks for the related IDs)

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

* Add `.AsNonNullable` to a nullable to-one relationship’s related getter (used in request parsing) to require a non-null related resource

### 0.7.2 (2020-04-14)

* Update Giraffe version

### 0.7.1 (2020-04-06)

* Update and lock FSharp.SystemTextJson version

### 0.7.0 (2020-04-03)

* Breaking bugfix: For operations with transformed context (`.ForContext(...)`), the `ResourceParserHelper`/`ResourceParser` is now typed to the original context, not the mapped context, so that it actually can parse the resource’s fields (which are typed to the original context)

### 0.6.0 (2020-04-03)

* Breaking: Renamed `RequestParser.Parse` to `ParseJob` and added `ParseAsync`
* Breaking: Removed the “related setter” versions of `ToManyRelationship.Remove`. These violated the JSON:API spec; they returned errors when the related resources were not found, but the spec requires a success response even if the members are already removed from the relationship.
* Added `PostCustom` operation for use-cases where the strict “create entity → persist entity” workflow of the normal `Post` operation doesn’t work
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
* Removed own `Skippable` implementation in favor of the one from FSharp.SystemTextJson (in namespace `System.Text.Json.Serialization`)
* Fail on startup if operations are missing required persistence functions
* Added support for optional precondition validation

### 0.3.3 (2020-03-05)

* Added `define.Operation.PostBackRef` to provide access to a pre-create "parent entity" in `AfterCreate` (using a relationship back-reference in the POST request)

### 0.3.2 (2020-03-04)

* Added some missing attribute overloads

### 0.3.1 (2020-03-03)

* Allow filtering on the resource ID

### 0.3.0 (2020-03-03)

* **Breaking:** POST, PATCH, and DELETE operations now have a single `ModifyResponse` member instead of separate members for 202 Accepted and normal responses
* Allow specifying `Async` constraints and constraint lists

### 0.2.0 (2020-02-28)

* Added entity-returning overloads for PATCH's `BeforeUpdate`, DELETE's `BeforeDelete`, and relationships' `BeforeModifySelf`

- Breaking: Made `BeforeModifySelf` use `Func`

### 0.1.3 (2020-02-25)

* Added `SetNonNull` for nullable attributes and relationships that may return null, but not be set to null

### 0.1.2 (2020-02-18)

* Improve some exception messages

### 0.1.1 (2020-02-17)

* Return ETag for HEAD requests

### 0.1.0 (2020-02-17)

* Initial release
