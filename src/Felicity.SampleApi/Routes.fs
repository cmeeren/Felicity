module Routes

// This module contains the API's routing. Nothing too surprising here if you
// know Giraffe.

open Giraffe
open Felicity

open JsonApi


let mainHandler : HttpHandler =
  choose [

    // Redirect API root to specification/documentation
    routex "/?" >=> redirectTo false "/spec"

    // This inserts all JSON:API routes for the specified context type. This requires a
    // corresponding registration using AddJsonApi in ConfigureServices.
    jsonApi<Context>

    // You can add handlers for multiple context types. For example, you might have some
    // resources that are only accessible to admins. You can model that using a separate
    // context type. Just remember to register all necessary context parsers using
    // AddJsonApi in ConfigureServices.
  ]
