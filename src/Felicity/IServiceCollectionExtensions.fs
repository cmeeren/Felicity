namespace Felicity

open System
open System.Reflection
open System.Text.Json
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open RoutingOperations


type JsonApiConfigBuilder<'ctx> = internal {
  services: IServiceCollection
  baseUrl: Uri option
  getCtx: (HttpContext -> Async<Result<'ctx, Error list>>) option
  configureSerializerOptions: (JsonSerializerOptions -> unit) option
} with

  static member internal DefaultFor services : JsonApiConfigBuilder<'ctx> = {
    services = services
    baseUrl = None
    getCtx = None
    configureSerializerOptions = None
  }

  member this.BaseUrl(url) : JsonApiConfigBuilder<'ctx> =
    { this with baseUrl = Some url }

  member this.GetCtxAsyncRes(getCtx: HttpContext -> Async<Result<'ctx, Error list>>) : JsonApiConfigBuilder<'ctx> =
    { this with getCtx = Some getCtx }

  member this.GetCtxAsync(getCtx: HttpContext -> Async<'ctx>) : JsonApiConfigBuilder<'ctx> =
    { this with getCtx = Some (getCtx >> Async.map Ok) }

  member this.GetCtxRes(getCtx: HttpContext -> Result<'ctx, Error list>) : JsonApiConfigBuilder<'ctx> =
    { this with getCtx = Some (getCtx >> async.Return) }

  member this.GetCtx(getCtx: HttpContext -> 'ctx) : JsonApiConfigBuilder<'ctx> =
    { this with getCtx = Some (getCtx >> Ok >> async.Return) }

  member this.ConfigureSerializerOptions(configure: JsonSerializerOptions -> unit) : JsonApiConfigBuilder<'ctx> =
    { this with configureSerializerOptions = Some configure }

  member this.Add() =
    let baseUrl = this.baseUrl |> Option.defaultWith (fun () -> failwith "Must specify a base URL")
    let getCtx = this.getCtx |> Option.defaultWith (fun () -> failwith "Must specify a context getter")
    let configureSerializerOptions = this.configureSerializerOptions |> Option.defaultValue ignore
    
    let resourceModules = ResourceModule.all<'ctx>
          
    ResourceModule.validateAll<'ctx> resourceModules
          
    let resourceModuleMap =
      resourceModules
      |> Array.map (fun m -> (ResourceModule.resourceDefinition<'ctx> m).TypeName, m)
      |> Map.ofArray
          
    let getFieldType =
      let lookup =
        resourceModules
        |> Array.collect (fun m ->
            let rDef = ResourceModule.resourceDefinition<'ctx> m
            let nonNullableAttrTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<NonNullableAttribute<_, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> Attribute<'ctx>).Name, pi.PropertyType.GetGenericArguments().[3])
            let nullableAttrTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<NullableAttribute<_, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> Attribute<'ctx>).Name, typedefof<Option<_>>.MakeGenericType(pi.PropertyType.GetGenericArguments().[3]))
            let toOneRelTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<ToOneRelationship<_, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> ToOneRelationship<'ctx>).Name, typeof<InternalDeserializationModelDoNotUse.DToOneRelationship>)
            let toOneNullableRelTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<ToOneNullableRelationship<_, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> ToOneNullableRelationship<'ctx>).Name, typeof<InternalDeserializationModelDoNotUse.DToOneNullableRelationship>)
            let toManyRelTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<ToManyRelationship<_, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> ToManyRelationship<'ctx>).Name, typeof<InternalDeserializationModelDoNotUse.DToManyRelationship>)
            let constraintsFieldByName =
              let hasConstrainedFields =
                ResourceModule.constrainedFields<'ctx> m
                |> Array.exists (fun f -> not f.BoxedGetConstraints.IsEmpty)
              if hasConstrainedFields then [| "constraints", typeof<obj> |] else [||]
            Array.concat [
              nonNullableAttrTypesByName
              nullableAttrTypesByName
              toOneRelTypesByName
              toOneNullableRelTypesByName
              toManyRelTypesByName
              constraintsFieldByName
            ]
            |> Array.map (fun (n, t) -> (rDef.TypeName, n), t)
        )
        |> dict
      fun typeName (fieldName: FieldName) ->
        match lookup.TryGetValue ((typeName, fieldName)) with
        | false, _ -> None
        | true, t -> Some t
          
    let getFieldSerializationOrder =
      let lookup =
        resourceModules
        |> Array.map (fun m ->
            let rDef = ResourceModule.resourceDefinition<'ctx> m
            let fields = ResourceModule.fields<'ctx> m |> Array.map (fun x -> x.Name)
            rDef.TypeName, Array.append fields [|"constraints"|]
        )
        |> dict
      fun typeName ->
        match lookup.TryGetValue typeName with
        | true, fs -> fs
        | false, _ -> [||]
          
    let modulesByCollectionName =
      resourceModules
      |> Array.groupBy ResourceModule.collectionName<'ctx>
      |> Array.choose (function
          | (None, _) -> None
          | Some collName, ms -> Some (collName, ms)
      )
          
    let collections =
      (Map.empty, modulesByCollectionName)
      ||> Array.fold (fun map (collName: CollectionName, resourceModules: Type []) ->
            let collOperations = collectionOperations<'ctx> resourceModuleMap baseUrl collName resourceModules
            map.Add(collName, collOperations)
      )
    
    this.services
      .AddSingleton<JsonApiHandler<'ctx>>(JsonApiHandler (Routing.jsonApiHandler getCtx collections))
      .AddSingleton<Serializer>(Serializer(getFieldType, getFieldSerializationOrder, configureSerializerOptions))
    
  



[<AutoOpen>]
module IServiceCollectionExtensions =

  type IServiceCollection with
    member this.AddJsonApi() = JsonApiConfigBuilder<'ctx>.DefaultFor this
