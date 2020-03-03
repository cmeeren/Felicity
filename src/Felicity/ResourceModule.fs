module internal Felicity.ResourceModule

open System
open System.Collections.Concurrent
open System.Reflection



let private allDict = ConcurrentDictionary<Type, Type []>()
let all<'ctx> =
  allDict.GetOrAdd(
    typeof<'ctx>,
    fun _ ->
      AppDomain.CurrentDomain.GetAssemblies()
      |> Array.collect (fun a -> try a.GetTypes() with _ -> [||])
      |> Array.filter (fun t ->
          t.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
          |> Array.exists (fun pi -> typeof<ResourceDefinition<'ctx>>.IsAssignableFrom pi.PropertyType)
      )
  )
  


let private resourceDefinitionDict = ConcurrentDictionary<Type * Type, obj>()
let resourceDefinition<'ctx> (m: Type) =
  resourceDefinitionDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<ResourceDefinition<'ctx>>)
      |> function
          | [||] -> failwithf "Resource module '%s' does not contain a resource definition" m.Name
          | [|x|] -> x
          | xs -> failwithf "Resource module '%s' contains %i public resource definitions; only one is allowed" m.Name xs.Length
      |> box<ResourceDefinition<'ctx>>
  )
  |> unbox<ResourceDefinition<'ctx>>


let private fieldsDict = ConcurrentDictionary<Type * Type, obj>()
let fields<'ctx> (m: Type) =
  fieldsDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<Field<'ctx>>)
      |> box<Field<'ctx> []>
  )
  |> unbox<Field<'ctx> []>


let private constrainedFieldsDict = ConcurrentDictionary<Type * Type, obj>()
let constrainedFields<'ctx> (m: Type) =
  constrainedFieldsDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<ConstrainedField<'ctx>>)
      |> box<ConstrainedField<'ctx> []>
  )
  |> unbox<ConstrainedField<'ctx> []>


let private attributesDict = ConcurrentDictionary<Type * Type, obj>()
let attributes<'ctx> (m: Type) =
  attributesDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<Attribute<'ctx>>)
      |> box<Attribute<'ctx> []>
  )
  |> unbox<Attribute<'ctx> []>


let private toOneRelsDict = ConcurrentDictionary<Type * Type, obj>()
let toOneRels<'ctx> (m: Type) =
  toOneRelsDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<ToOneRelationship<'ctx>>)
      |> box<ToOneRelationship<'ctx> []>
  )
  |> unbox<ToOneRelationship<'ctx> []>


let private toOneNullableRelsDict = ConcurrentDictionary<Type * Type, obj>()
let toOneNullableRels<'ctx> (m: Type) =
  toOneNullableRelsDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<ToOneNullableRelationship<'ctx>>)
      |> box<ToOneNullableRelationship<'ctx> []>
  )
  |> unbox<ToOneNullableRelationship<'ctx> []>


let private toManyRelsDict = ConcurrentDictionary<Type * Type, obj>()
let toManyRels<'ctx> (m: Type) =
  toManyRelsDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<ToManyRelationship<'ctx>>)
      |> box<ToManyRelationship<'ctx> []>
  )
  |> unbox<ToManyRelationship<'ctx> []>


let private customOpsDict = ConcurrentDictionary<Type * Type, obj>()
let customOps<'ctx> (m: Type) =
  customOpsDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun pi -> pi.GetValue(null) |> tryUnbox<CustomOperation<'ctx>>)
      |> box<CustomOperation<'ctx> []>
  )
  |> unbox<CustomOperation<'ctx> []>


let collectionName<'ctx> (resourceModule: Type) =
  (resourceDefinition<'ctx> resourceModule).CollectionName


let hasCollectionName<'ctx> = collectionName<'ctx> >> Option.isSome


let resourceLookup<'ctx> collName (msInColl: Type []) =
  let nonPolymorphicOperations =
    msInColl
    |> Array.choose (fun m ->
        m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<ResourceLookup<'ctx>>)
        |> Array.tryHead
        |> Option.map (fun op ->
            let rDef = resourceDefinition<'ctx> m
            { new ResSpecificResourceLookup<'ctx> with
                member _.GetByIdBoxed ctx rawId =
                  op.GetByIdBoxed rDef ctx rawId
                  |> AsyncResult.map (Option.map (fun e -> rDef, e))
            }
        )
    )

  let polymorphicOperations =
    msInColl
    |> Array.choose (fun m ->
        m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<PolymorphicResourceLookup<'ctx>>)
        |> Array.tryHead
        |> Option.map (fun op ->
            let rDef = resourceDefinition<'ctx> m
            { new ResSpecificResourceLookup<'ctx> with
                member _.GetByIdBoxed ctx rawId =
                  op.GetByIdBoxed rDef ctx rawId
            }
        )
    )

  Array.concat [nonPolymorphicOperations; polymorphicOperations]
  |> function
      | [||] -> None
      | [|x|] -> Some x
      | xs -> failwithf "%i public resource lookup operations specified for collection name %s; only one is allowed" xs.Length collName


let hasResourceLookup<'ctx> (collName, msInColl) =
  resourceLookup<'ctx> collName msInColl |> Option.isSome


let postOperation<'ctx> (m: Type) =
  m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
  |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<PostOperation<'ctx>>)
  |> function
      | [||] -> None
      | [|x|] -> Some x
      | xs -> failwithf "Resource module '%s' contains %i public POST collection operations; only one is allowed" m.Name xs.Length


let preconditions<'ctx> (m: Type) =
  m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
  |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<Preconditions<'ctx>>)
  |> function
      | [||] -> None
      | [|x|] -> Some x
      | xs -> failwithf "Resource module '%s' contains %i public precondition definitions; only one is allowed" m.Name xs.Length


let private getResourceOperationDict = ConcurrentDictionary<Type * Type, obj>()
let getResourceOperation<'ctx> (m: Type) =
  getResourceOperationDict.GetOrAdd(
    (typeof<'ctx>, m),
    fun _ ->
      m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
      |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<GetResourceOperation<'ctx>>)
      |> function
          | [||] -> None
          | [|x|] -> Some x
          | xs -> failwithf "Resource module '%s' contains %i public GET resource operations; only one is allowed" m.Name xs.Length
      |> box<GetResourceOperation<'ctx> option>
  )
  |> unbox<GetResourceOperation<'ctx> option>


let hasGetResourceOperation<'ctx> =
  getResourceOperation<'ctx> >> Option.isSome


let private ensureValidAndUniqueFieldNames<'ctx> (m: Type) =
  fields<'ctx> m
  |> Array.map (fun x -> x.Name)
  |> Array.countBy id
  |> Array.iter (fun (name, count) ->
      if name = "type" then
        failwithf "Resource module '%s' contains a field named 'type', which is reserved by JSON:API" m.Name
      elif name = "id" then
        failwithf "Resource module '%s' contains a field named 'id', which is reserved by JSON:API" m.Name
      elif not <| MemberName.isValid name then
        failwithf "Resource module '%s' contains a field named '%s', which is not a valid JSON:API member name" m.Name name
      elif count > 1 then
        failwithf "Resource module '%s' contains %i fields named '%s'; names must be unique" m.Name count name
  )


let private ensureCollectionNameIfNeeded<'ctx> (m: Type) =
  if not (hasCollectionName<'ctx> m) then
    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
    |> Array.iter (fun x ->
        match x.GetValue(null) with
        | :? GetCollectionOperation<'ctx>
        | :? PolymorphicGetCollectionOperation<'ctx> ->
            failwithf "Resource module '%s' has no collection name, but contains a public GET collection operation, which requires a collection name" m.Name
        | :? PostOperation<'ctx> ->
            failwithf "Resource module '%s' has no collection name, but contains a public POST collection operation, which requires a collection name" m.Name
        | :? GetResourceOperation<'ctx> ->
            failwithf "Resource module '%s' has no collection name, but contains a public GET resource operation, which requires a collection name" m.Name
        | :? PatchOperation<'ctx> ->
            failwithf "Resource module '%s' has no collection name, but contains a public PATCH resource operation, which requires a collection name" m.Name
        | :? DeleteOperation<'ctx> ->
            failwithf "Resource module '%s' has no collection name, but contains a public DELETE resource operation, which requires a collection name" m.Name
        | :? CustomOperation<'ctx> ->
            failwithf "Resource module '%s' has no collection name, but contains a public custom operation, which requires a collection name" m.Name
        // getRelated/getSelf is not included since it is always present when the
        // relationship is gettable, and it may be possible to get related resources as
        // includes in a compound document even though the parent resource does not have a
        // self link.
        | :? RelationshipHandlers<'ctx> as op when op.PostSelf.IsSome || op.PatchSelf.IsSome || op.DeleteSelf.IsSome ->
            failwithf "Resource module '%s' has no collection name, but contains a public relationship with a POST/PATCH/DELETE operation, which requires a collection name" m.Name
        | _ -> ()
    )


let private ensureHasGetResourceOpIfNeeded<'ctx> (m: Type) =
  if not (hasGetResourceOperation<'ctx> m) then
    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
    |> Array.iter (fun x ->
        // A GET resource operation is required for a self link, which is required for
        // the operations below
        match x.GetValue(null) with
        | :? PatchOperation<'ctx> ->
            failwithf "Resource module '%s' has no GET resource operation, but contains a public PATCH resource operation, which requires a GET resource operation" m.Name
        | :? DeleteOperation<'ctx> ->
            failwithf "Resource module '%s' has no GET resource operation, but contains a public DELETE resource operation, which requires a GET resource operation" m.Name
        | :? CustomOperation<'ctx> ->
            failwithf "Resource module '%s' has no GET resource operation, but contains a public custom operation, which requires a GET resource operation" m.Name
        // getRelated/getSelf is not included since it is always present when the
        // relationship is gettable, and it may be possible to get related resources as
        // includes in a compound document even though the parent resource does not have a
        // self link.
        | :? RelationshipHandlers<'ctx> as op when op.PostSelf.IsSome || op.PatchSelf.IsSome || op.DeleteSelf.IsSome ->
            failwithf "Resource module '%s' has no GET resource operation, but contains a public relationship with a POST/PATCH/DELETE operation, which requires a GET resource operation" m.Name
        | _ -> ()
    )


let private ensureHasLookupIfNeeded<'ctx> ms =
  ms
  |> Array.groupBy collectionName<'ctx>
  |> Array.choose (fun (cn, m) -> cn |> Option.map (fun cn -> cn, m))
  |> Array.filter (not << hasResourceLookup<'ctx>)
  |> Array.iter (fun (collName, msInColl) ->
      msInColl
      |> Array.iter (fun m ->
        m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.iter (fun x ->
            match x.GetValue(null) with
            | :? GetResourceOperation<'ctx> ->
                failwithf "Collection '%s' has no lookup operation, but contains public GET resource operations, which requires a lookup operation" collName
            | :? PatchOperation<'ctx> ->
                failwithf "Collection '%s' has no lookup operation, but contains public PATCH resource operations, which requires a lookup operation" collName
            | :? DeleteOperation<'ctx> ->
                failwithf "Collection '%s' has no lookup operation, but contains public DELETE resource operations, which requires a lookup operation" collName
            | :? CustomOperation<'ctx> ->
                failwithf "Collection '%s' has no lookup operation, but contains public custom operations, which requires a lookup operation" collName
            // getRelated/getSelf is not included since it is always present when the
            // relationship is gettable, and it may be possible to get related resources
            // as includes in a compound document without the parent resource having a
            // lookup operation.
            //
            // Also, PatchSelf is not included since it can be used when creating
            // resources, which does not require a lookup operation.
            | :? RelationshipHandlers<'ctx> as op when op.PostSelf.IsSome || op.DeleteSelf.IsSome ->
                failwithf "Resource module '%s' has no lookup operation, but contains public relationships with a POST/DELETE operation, which requires a lookup operation" m.Name
            | _ -> ()
        )
      )
  )


let private ensureNoRelLinkNameCollisions<'ctx> m =
  let ops = customOps<'ctx> m |> Array.map (fun op -> op.Name)
  let rels =
    [ toOneRels<'ctx> m |> Array.map (fun r -> r.Name)
      toOneNullableRels<'ctx> m |> Array.map (fun r -> r.Name)
      toManyRels<'ctx> m |> Array.map (fun r -> r.Name) ]
    |> Array.concat

  ops
  |> Array.countBy id
  |> Array.iter (fun (name, count) ->
      if count > 1 then
        failwithf "Resource module '%s' contains %i links named '%s'; names must be unique" m.Name count name
      elif name = "relationships" then
        // This might actually be possible, though not recommended since it would be
        // confusing. Block it fow now, add test for it later if it should be supported.
        failwithf "Resource module '%s' contains a link named 'relationships'; this is not allowed since it conflicts with standard JSON:API URLs" m.Name
  )

  rels
  |> Array.countBy id
  |> Array.iter (fun (name, count) ->
      if count > 1 then
        failwithf "Resource module '%s' contains %i relationships named '%s'; names must be unique" m.Name count name
      elif name = "relationships" then
        // This might actually be possible, though not recommended since it would be
        // confusing. Block it fow now, add test for it later if it should be supported.
        failwithf "Resource module '%s' contains a relationship named 'relationships'; this is not allowed since it conflicts with standard JSON:API URLs" m.Name
  )

  for opName in ops do
  for relName in rels do
    if opName = relName then
      failwithf "Resource module '%s' contains both a relationship and a link named '%s'; names must be unique among relationships and links" m.Name opName


let private ensureNoConstraintsAttrIfAnyFieldHasConstraints<'ctx> m =
  let fieldWithConstraints =
    constrainedFields<'ctx> m
    |> Array.tryPick (fun f -> if f.HasConstraints then Some f.Name else None)
  let hasConstraintsField = fields<'ctx> m |> Array.exists(fun f -> f.Name = "constraints")
  match fieldWithConstraints, hasConstraintsField with
  | Some fieldName, true -> failwithf "Resource module '%s' contains both a field '%s' with constraints and a separate field called 'constraints'; either rename the 'constraints' field or remove all constraints from all fields" m.Name fieldName
  | _ -> ()



let private ensurePolymorphicModuleHasOnlyPolymorphicOperations<'ctx> m =
  let resDef = resourceDefinition<'ctx> m

  if resDef.TypeName = Constants.polymorphicTypeName then
    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
    |> Array.iter (fun x ->
        match x.GetValue(null) with
        | :? Field<'ctx> ->
            failwithf "Polymorphic resource module '%s' may only contain polymorphic GET collection and lookup operations, but contained a field" m.Name
        | :? GetResourceOperation<'ctx> ->
            failwithf "Polymorphic resource module '%s' may only contain polymorphic GET collection and lookup operations, but contained a GET resource operation" m.Name
        | :? PatchOperation<'ctx> ->
            failwithf "Polymorphic resource module '%s' may only contain polymorphic GET collection and lookup operations, but contained a PATCH operation" m.Name
        | :? DeleteOperation<'ctx> ->
            failwithf "Polymorphic resource module '%s' may only contain polymorphic GET collection and lookup operations, but contained a DELETE operation" m.Name
        | :? CustomOperation<'ctx> ->
            failwithf "Polymorphic resource module '%s' may only contain polymorphic GET collection and lookup operations, but contained a custom link" m.Name
        | _ -> ()
    )


let private ensureTypeNameIsValid<'ctx> m =
  let resDef = resourceDefinition<'ctx> m
  if resDef.TypeName <> Constants.polymorphicTypeName && not (MemberName.isValid resDef.TypeName) then
    failwithf "Resource module '%s' uses type name '%s', which is not a valid JSON:API member name" m.Name resDef.TypeName


let private ensureNoDuplicateTypeNames<'ctx> ms =
  ms
  |> Array.map (fun m -> (resourceDefinition<'ctx> m).TypeName)
  |> Array.countBy id
  |> Array.iter (fun (name, count) ->
    if count > 1 && name <> Constants.polymorphicTypeName then
      failwithf "Resource type name '%s' exists in %i resource modules; type names must be unique" name count
  )




let validateAll<'ctx> ms =
  ms |> Array.iter ensureValidAndUniqueFieldNames<'ctx>
  ms |> Array.iter ensureCollectionNameIfNeeded<'ctx>
  ms |> Array.iter ensureHasGetResourceOpIfNeeded<'ctx>
  ms |> Array.iter ensureNoRelLinkNameCollisions<'ctx>
  ms |> Array.iter ensureNoConstraintsAttrIfAnyFieldHasConstraints<'ctx>
  ms |> Array.iter ensurePolymorphicModuleHasOnlyPolymorphicOperations<'ctx>
  ms |> Array.iter ensureTypeNameIsValid<'ctx>
  ms |> ensureNoDuplicateTypeNames<'ctx>
  ms |> ensureHasLookupIfNeeded<'ctx>
