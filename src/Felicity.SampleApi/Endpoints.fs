module Endpoints

// This module contains the API's routing. Nothing too surprising here if you
// know Giraffe.

open Giraffe
open Giraffe.EndpointRouting


let endpoints: Endpoint list = [
    // Redirect API root to specification/documentation
    route "/" (redirectTo false "/spec")
]
