namespace Felicity

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Giraffe


module internal FieldTracker =


  // Note: Similar code exists in ResourceBuilder
  let private shouldUseField (req: Request) typeName fieldName =
    match req.Fieldsets.TryGetValue typeName with
    | false, _ -> true
    | true, fields -> fields.Contains fieldName

  // Note: Similar code exists in ResourceBuilder
  let private shouldIncludeRelationship (req: Request) (currentIncludePath: RelationshipName list) relName =
    req.Includes |> List.exists (fun path ->
      path.Length > currentIncludePath.Length && path[currentIncludePath.Length] = relName
    )

  let trackFieldUsage<'ctx>
    (resourceModuleMap: Map<ResourceTypeName, Type>)
    primaryResourceTypes
    (ctx: 'ctx)
    (req: Request)
    (httpCtx: HttpContext)
    (relationshipOperationResourceTypeNameAndFieldName: (ResourceTypeName * FieldName) option)
    (relNameIfRelationshipSelf: FieldName option)
    (consumedFieldNamesWithType: (ResourceTypeName * Set<ConsumedFieldName>) option)
    (report: 'ctx -> FieldUseInfo list -> Task<HttpHandler>)
    =
    let fieldUsage = ConcurrentDictionary<struct (ResourceTypeName * FieldName), FieldUsage>()

    let addFieldUsage typeName fieldName usageType =
      fieldUsage.AddOrUpdate(
        struct(typeName, fieldName),
        usageType,
        fun _ oldUsageType ->
          match oldUsageType, usageType with
          | FieldUsage.Explicit, _ | _, FieldUsage.Explicit -> FieldUsage.Explicit
          | FieldUsage.Implicit, _ | _, FieldUsage.Implicit -> FieldUsage.Implicit
          | FieldUsage.Excluded, FieldUsage.Excluded -> FieldUsage.Excluded
      )
      |> ignore

    let rec trackUsageForResourceType (relNameIfRelationshipSelfAndNotRecursiveCall: FieldName option) (currentIncludePath: RelationshipName list) (typeName: ResourceTypeName) =

      let resourceModule = resourceModuleMap[typeName]

      let constrainedFields = ResourceModule.constrainedFields<'ctx> resourceModule

      // Regarding 'relNameIfRelationshipSelfAndNotRecursiveCall': For self URLs, the primary resources are resource
      // identifiers (i.e., no fields), and the included resources are the same as those for the parent resource
      // document but can only start using the current relationship, so only consider that relationship.
      //
      // Furthermore, only do this on the initial call; when recursing (i.e., though includes), everything works
      // normally.

      let attrNames =
        if relNameIfRelationshipSelfAndNotRecursiveCall.IsSome then [||]
        else
          ResourceModule.attributes<'ctx> resourceModule
          |> Array.filter (fun a -> a.BoxedGetSerialized.IsSome)  // Only gettable attributes
          |> Array.map (fun a -> a.Name)
          |> Array.append (if constrainedFields |> Array.forall (fun f -> not f.HasConstraints) then [||] else [| "constraints" |])

      let relNamesAndAllowedTypes =
        Array.concat [
          ResourceModule.toOneRels<'ctx> resourceModule |> Array.map (fun r -> r.Name, r.AllowedTypes)
          ResourceModule.toOneNullableRels<'ctx> resourceModule |> Array.map (fun r -> r.Name, r.AllowedTypes)
          ResourceModule.toManyRels<'ctx> resourceModule |> Array.map (fun r -> r.Name, r.AllowedTypes)
        ]
        |> Array.filter (fun (relName, _) -> relNameIfRelationshipSelfAndNotRecursiveCall.IsNone || relNameIfRelationshipSelfAndNotRecursiveCall = Some relName)

      for attrName in attrNames do
        let usageType =
          match req.Fieldsets.TryGetValue typeName with
          | true, fields when fields.Contains attrName -> FieldUsage.Explicit
          | true, _ -> FieldUsage.Excluded
          | false, _ -> FieldUsage.Implicit

        addFieldUsage typeName attrName usageType

      for relName, allowedTypes in relNamesAndAllowedTypes do
        let shouldInclude = shouldIncludeRelationship req currentIncludePath relName
        let usageType =
          match req.Fieldsets.TryGetValue typeName with
          | true, fields when fields.Contains relName -> FieldUsage.Explicit
          | _, _ when shouldInclude -> FieldUsage.Explicit
          | true, _ -> FieldUsage.Excluded
          | false, _ -> FieldUsage.Implicit

        addFieldUsage typeName relName usageType

        if shouldInclude then
          match allowedTypes with
          | None -> logFieldTrackerPolymorphicRelTraversalWarning httpCtx typeName relName
          | Some ts -> ts |> Seq.iter (trackUsageForResourceType None (currentIncludePath @ [relName]))

    task {
      match relationshipOperationResourceTypeNameAndFieldName with
      | None -> ()
      | Some (usedTypeName, fieldName) ->
          addFieldUsage usedTypeName fieldName FieldUsage.Explicit

      match consumedFieldNamesWithType with
      | None -> ()
      | Some (typeName, fieldNames) ->
          for fn in fieldNames do addFieldUsage typeName fn FieldUsage.Explicit

      primaryResourceTypes |> List.iter (trackUsageForResourceType relNameIfRelationshipSelf [])

      return!
        fieldUsage
        |> Seq.map (fun kvp ->
            let struct (typeName, fieldName) = kvp.Key
            {
              TypeName = typeName
              FieldName = fieldName
              Usage = kvp.Value
            }
        )
        |> Seq.toList
        |> report ctx
    }
