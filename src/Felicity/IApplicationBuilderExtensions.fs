namespace Felicity

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.EndpointRouting
open Routing



[<AutoOpen>]
module IApplicationBuilderExtensions =

  type IApplicationBuilder with
    member this.UseJsonApiEndpoints<'ctx>() =
      let (JsonApiEndpoints endpoints) = this.ApplicationServices.GetService<JsonApiEndpoints<'ctx>> ()
      this.UseEndpoints(fun b -> b.MapGiraffeEndpoints(endpoints))
