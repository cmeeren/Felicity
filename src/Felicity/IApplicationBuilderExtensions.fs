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
      let endpoints = this.ApplicationServices.GetService<JsonApiEndpoints<'ctx>> ()

      if isNull (box endpoints) then
        if typeof<'ctx> = typeof<obj> then
          failwith $"Missing IServiceCollection.AddJsonApi call for context type {typeof<'ctx>.FullName}, or missing explicit type parameter in call to IApplicationBuilder.UseJsonApiEndpoints"
        else
          failwith $"Missing IServiceCollection.AddJsonApi call for context type {typeof<'ctx>.FullName}"

      let (JsonApiEndpoints endpoints) = endpoints
      this.UseEndpoints(fun b -> b.MapGiraffeEndpoints(endpoints))
