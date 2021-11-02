namespace Felicity

open System
open System.Reflection
open System.Text.Json
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Hopac
open Giraffe
open RoutingOperations
open Routing



type JsonApiConfigBuilder<'ctx> = internal {
  services: IServiceCollection
  baseUrl: string option
  relativeJsonApiRoot: string option
  getCtx: (HttpContext -> Job<Result<'ctx, Error list>>) option
  getMeta: 'ctx -> Map<string, obj>
  configureSerializerOptions: (JsonSerializerOptions -> unit) option
} with

  static member internal DefaultFor services : JsonApiConfigBuilder<'ctx> = {
    services = services
    baseUrl = None
    relativeJsonApiRoot = None
    getCtx = None
    getMeta = fun _ -> Map.empty
    configureSerializerOptions = None
  }

  /// Explicitly sets the base URL to be used in JSON:API responses. This is
  /// optional; if not supplied, this will be inferred from the actual request
  /// URL. (See also RelativeJsonApiRoot which you need to use then if your
  /// jsonApi handler is not at the root level.)
  ///
  /// This may not be combined with RelativeJsonApiRoot.
  member this.BaseUrl(url: Uri) : JsonApiConfigBuilder<'ctx> =
    if this.relativeJsonApiRoot.IsSome then failwith "BaseUrl and RelativeJsonApiRoot can not be mixed."
    { this with baseUrl = Some (url.ToString().TrimEnd('/')) }

  /// Explicitly sets the base URL to be used in JSON:API responses. This is
  /// optional; if not supplied, this will be inferred from the actual request
  /// URL. (See also RelativeJsonApiRoot which you need to use then if your
  /// jsonApi handler is not at the root level.)
  ///
  /// This may not be combined with RelativeJsonApiRoot.
  member this.BaseUrl(url: string) : JsonApiConfigBuilder<'ctx> =
    if this.relativeJsonApiRoot.IsSome then failwith "BaseUrl and RelativeJsonApiRoot can not be mixed."
    { this with baseUrl = Some (url.TrimEnd('/')) }

  /// Sets the relative root path for the JSON:API routes. This must match the
  /// placement of the jsonApi HttpHandler in your Giraffe routing. For example,
  /// if the jsonApi handler is placed in a subroute 'foo', e.g. clients call
  /// 'GET /foo/articles' to query the /articles collection, then you must pass
  /// 'foo' as a parameter here.
  ///
  /// This may not be combined with BaseUrl.
  ///
  /// Leading/trailing slashes don't matter.
  member this.RelativeJsonApiRoot(path: string) : JsonApiConfigBuilder<'ctx> =
    if this.baseUrl.IsSome then failwith "BaseUrl and RelativeJsonApiRoot can not be mixed."
    { this with relativeJsonApiRoot = Some (path.Trim('/')) }

  member this.GetCtxJobRes(getCtx: HttpContext -> Job<Result<'ctx, Error list>>) : JsonApiConfigBuilder<'ctx> =
    { this with getCtx = Some getCtx }

  member this.GetCtxAsyncRes(getCtx: HttpContext -> Async<Result<'ctx, Error list>>) : JsonApiConfigBuilder<'ctx> =
    this.GetCtxJobRes(Job.liftAsync getCtx)

  member this.GetCtxJob(getCtx: HttpContext -> Job<'ctx>) : JsonApiConfigBuilder<'ctx> =
    this.GetCtxJobRes(getCtx >> Job.map Ok)

  member this.GetCtxAsync(getCtx: HttpContext -> Async<'ctx>) : JsonApiConfigBuilder<'ctx> =
    this.GetCtxJob(Job.liftAsync getCtx)

  member this.GetCtxRes(getCtx: HttpContext -> Result<'ctx, Error list>) : JsonApiConfigBuilder<'ctx> =
    this.GetCtxJobRes(Job.lift getCtx)

  member this.GetCtx(getCtx: HttpContext -> 'ctx) : JsonApiConfigBuilder<'ctx> =
    this.GetCtxJobRes(JobResult.lift getCtx)

  member this.GetMeta(getMeta: 'ctx -> Map<string, obj>) : JsonApiConfigBuilder<'ctx> =
    { this with getMeta = getMeta }

  member this.ConfigureSerializerOptions(configure: JsonSerializerOptions -> unit) : JsonApiConfigBuilder<'ctx> =
    { this with configureSerializerOptions = Some configure }

  member this.Add() =
    let getBaseUrl =
      match this.baseUrl with
      | None ->
          fun (ctx: HttpContext) ->
            let url = Uri(ctx.GetRequestUrl())
            let baseUrl = url.Scheme + Uri.SchemeDelimiter + url.Authority
            match this.relativeJsonApiRoot with None -> baseUrl | Some r -> baseUrl + "/" + r
      | Some url ->
          fun _ -> url
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
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<NonNullableAttribute<_, _, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> Attribute<'ctx>).Name, pi.PropertyType.GetGenericArguments().[4])
            let nullableAttrTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<NullableAttribute<_, _, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> Attribute<'ctx>).Name, typedefof<Option<_>>.MakeGenericType(pi.PropertyType.GetGenericArguments().[4]))
            let toOneRelTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<ToOneRelationship<_, _, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> ToOneRelationship<'ctx>).Name, typeof<InternalDeserializationModelDoNotUse.DToOneRelationship>)
            let toOneNullableRelTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<ToOneNullableRelationship<_, _, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> ToOneNullableRelationship<'ctx>).Name, typeof<InternalDeserializationModelDoNotUse.DToOneNullableRelationship>)
            let toManyRelTypesByName =
              m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
              |> Array.filter (fun pi -> pi.PropertyType.IsGenericType && pi.PropertyType.GetGenericTypeDefinition() = typedefof<ToManyRelationship<_, _, _, _, _>>)
              |> Array.map (fun pi -> (pi.GetValue(null) :?> ToManyRelationship<'ctx>).Name, typeof<InternalDeserializationModelDoNotUse.DToManyRelationship>)
            let constraintsFieldByName =
              let hasConstrainedFields =
                ResourceModule.constrainedFields<'ctx> m
                |> Array.exists (fun f -> f.HasConstraints)
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
            let collOperations = collectionOperations<'ctx> resourceModuleMap getBaseUrl collName resourceModules
            map.Add(collName, collOperations)
      )

    let relativeRootWithLeadingSlash =
      match this.relativeJsonApiRoot, this.baseUrl with
      | None, None -> ""
      | Some _, Some _ -> failwith "Framework bug: Both relative root and base URL specified"
      | Some root, None -> "/" + root
      | None, Some url ->
          let relativeRoot = Uri(url).PathAndQuery.Trim('/')
          if relativeRoot = "" then "" else "/" + relativeRoot

    this.services
      .AddSingleton<JsonApiEndpoints<'ctx>>(JsonApiEndpoints (jsonApiEndpoints relativeRootWithLeadingSlash getCtx collections))
      .AddSingleton<Serializer<'ctx>>(Serializer<'ctx>(getFieldType, getFieldSerializationOrder, configureSerializerOptions))
      .AddSingleton<Serializer<ErrorSerializerCtx>>(Serializer<ErrorSerializerCtx>(getFieldType, getFieldSerializationOrder, configureSerializerOptions))
      .AddSingleton<SemaphoreQueueFactory<'ctx>>(SemaphoreQueueFactory<'ctx>())
      .AddSingleton<MetaGetter<'ctx>>(MetaGetter<'ctx>(this.getMeta))
    
  



[<AutoOpen>]
module IServiceCollectionExtensions =

  type IServiceCollection with
    member this.AddJsonApi() = JsonApiConfigBuilder<'ctx>.DefaultFor this
