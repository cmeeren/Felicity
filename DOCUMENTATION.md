Felicity documentation
==============

The aim of this document is to explain how to use Felicity. For a complete, working example of a simple API, check out the [sample API](https://github.com/cmeeren/Felicity/tree/master/src/Felicity.SampleApi) in this repo. For a very brief overview, check out the [Quick Start section](https://github.com/cmeeren/Felicity/blob/master/README.md#quick-start) of the readme.

This documentation assumes working F# knowledge. If you’re new to F#, Scott Wlaschin’s blog [F# for fun and profit](https://fsharpforfunandprofit.com/) is a great place to start (and continue) learning the ins and outs of F# and functional programming. His book [Domain Modeling Made Functional](https://pragprog.com/book/swdddf/domain-modeling-made-functional) is also a great resource for learning F# (and in particular how it can  be used for domain modeling). You can find many more excellent resources at [fsharp.org](https://fsharp.org).

This documentation also assumes some knowledge of ASP.NET Core, [Giraffe](https://github.com/giraffe-fsharp/Giraffe), and of course [JSON:API](https://jsonapi.org/format/).

Suggestions for improvements are welcome. For large changes, please  open an issue. For small changes (e.g. typos), simply submit a PR.

Table of contents
-----------------

TODO

Basics
------

TODO

### Compile-time safety

TODO

### Requirements for your domain logic

TODO

### Convenience overloads

TODO

### Returning errors

TODO

* Logging, suppressing

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

* Setter: ID vs. related, must check for existence self if using ID setter

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
* Query parameters
  * Filter
  * Single vs. List
  * Sort
  * Page
  * Custom
  * Parsed vs. enum
* Headers

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

### Preconditions

TODO

* POST, PATCH, DELETE

### Execution order

TODO

Operation-specific authorization
--------------------------------

TODO

* Context mapping

ETag
----

TODO

* hashes response body
* success responses
* 304 not modified

Preconditions (`If-Match`/`If-Unmodified-Since`)
------------------------------------------------

TODO

Polymorphism
------------

TODO

Current limitations
-------------------

TODO

* Included resources in relationship self requests

FAQ
---

TODO

* Why `Func`