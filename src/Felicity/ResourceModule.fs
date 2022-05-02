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
          | [||] -> failwith $"Resource module '%s{m.Name}' does not contain a resource definition"
          | [|x|] -> x
          | xs -> failwith $"Resource module '%s{m.Name}' contains %i{xs.Length} public resource definitions; only one is allowed"
      |> box<ResourceDefinition<'ctx>>
  )
  |> unbox<ResourceDefinition<'ctx>>


let private lockSpecDict = ConcurrentDictionary<Type * string, obj>()
let lockSpec<'ctx> (collName: CollectionName) =
  lockSpecDict.GetOrAdd(
    (typeof<'ctx>, collName),
    fun _ ->
      all<'ctx>
      |> Array.collect (fun m -> m.GetProperties(BindingFlags.Public ||| BindingFlags.Static))
      |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<ResourceDefinitionLockSpec<'ctx>>)
      |> Array.filter (fun x -> x.CollName = Some collName)
      |> Array.choose (fun x -> x.LockSpecs |> Option.map (fun xs -> xs, x.TotalTimeout))
      |> function
          | [||] -> None
          | [|x|] -> Some x
          | xs -> failwith $"Collection name '%s{collName}' contains %i{xs.Length} resources with lock definitions; only one lock definition per collection is allowed"
      |> box<(LockSpecification<'ctx> list * TimeSpan option) option>
  )
  |> unbox<(LockSpecification<'ctx> list * TimeSpan option) option>


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
                  |> TaskResult.map (Option.map (fun e -> rDef, e))
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
      | xs -> failwith $"%i{xs.Length} public resource lookup operations specified for collection name %s{collName}; only one is allowed"


let hasResourceLookup<'ctx> (collName, msInColl) =
  resourceLookup<'ctx> collName msInColl |> Option.isSome


let postOperation<'ctx> (m: Type) =
  m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
  |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<PostOperation<'ctx>>)
  |> function
      | [||] -> None
      | [|x|] -> Some x
      | xs -> failwith $"Resource module '%s{m.Name}' contains %i{xs.Length} public POST collection operations; only one is allowed"


let preconditions<'ctx> (m: Type) =
  m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
  |> Array.choose (fun x -> x.GetValue(null) |> tryUnbox<Preconditions<'ctx>>)
  |> function
      | [||] -> None
      | [|x|] -> Some x
      | xs -> failwith $"Resource module '%s{m.Name}' contains %i{xs.Length} public precondition definitions; only one is allowed"


let hasPreconditions<'ctx> m =
  preconditions<'ctx> m |> Option.isSome


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
          | xs -> failwith $"Resource module '%s{m.Name}' contains %i{xs.Length} public GET resource operations; only one is allowed"
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
        failwith $"Resource module '%s{m.Name}' contains a field named 'type', which is reserved by JSON:API"
      elif name = "id" then
        failwith $"Resource module '%s{m.Name}' contains a field named 'id', which is reserved by JSON:API"
      elif not <| MemberName.isValid name then
        failwith $"Resource module '%s{m.Name}' contains a field named '%s{name}', which is not a valid JSON:API member name"
      elif count > 1 then
        failwith $"Resource module '%s{m.Name}' contains %i{count} fields named '%s{name}'; names must be unique"
  )


let private ensureCollectionNameIfNeeded<'ctx> (m: Type) =
  if not (hasCollectionName<'ctx> m) then
    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
    |> Array.iter (fun x ->
        match x.GetValue(null) with
        | :? GetCollectionOperation<'ctx>
        | :? PolymorphicGetCollectionOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no collection name, but contains a public GET collection operation, which requires a collection name"
        | :? PostOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no collection name, but contains a public POST collection operation, which requires a collection name"
        | :? GetResourceOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no collection name, but contains a public GET resource operation, which requires a collection name"
        | :? PatchOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no collection name, but contains a public PATCH resource operation, which requires a collection name"
        | :? DeleteOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no collection name, but contains a public DELETE resource operation, which requires a collection name"
        | :? CustomOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no collection name, but contains a public custom operation, which requires a collection name"
        // getRelated/getSelf is not included since it is always present when the
        // relationship is gettable, and it may be possible to get related resources as
        // includes in a compound document even though the parent resource does not have a
        // self link.
        | :? RelationshipHandlers<'ctx> as op when op.PostSelf.IsSome || op.PatchSelf.IsSome || op.DeleteSelf.IsSome ->
            failwith $"Resource module '%s{m.Name}' has no collection name, but contains a public relationship with a POST/PATCH/DELETE operation, which requires a collection name"
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
            failwith $"Resource module '%s{m.Name}' has no GET resource operation, but contains a public PATCH resource operation, which requires a GET resource operation"
        | :? DeleteOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no GET resource operation, but contains a public DELETE resource operation, which requires a GET resource operation"
        | :? CustomOperation<'ctx> ->
            failwith $"Resource module '%s{m.Name}' has no GET resource operation, but contains a public custom operation, which requires a GET resource operation"
        // getRelated/getSelf is not included since it is always present when the
        // relationship is gettable, and it may be possible to get related resources as
        // includes in a compound document even though the parent resource does not have a
        // self link.
        | :? RelationshipHandlers<'ctx> as op when op.PostSelf.IsSome || op.PatchSelf.IsSome || op.DeleteSelf.IsSome ->
            failwith $"Resource module '%s{m.Name}' has no GET resource operation, but contains a public relationship with a POST/PATCH/DELETE operation, which requires a GET resource operation"
        | _ -> ()
    )


let private ensureHasPersistFunction<'ctx> (m: Type) =
  m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
  |> Array.iter (fun x ->
      match x.GetValue(null) with
      | :? PostOperation<'ctx> as op when not op.HasPersist ->
          failwith $"Resource module '%s{m.Name}' has a POST operation with no AfterCreate, which is required"
      | :? PatchOperation<'ctx> as op when not op.HasPersist ->
          failwith $"Resource module '%s{m.Name}' has a PATCH operation with no AfterUpdate, which is required"
      | :? RelationshipHandlers<'ctx> as op when op.IsSettableWithoutPersist ->
          failwith $"Resource module '%s{m.Name}' has a settable relationship '%s{op.Name}' with no AfterModifySelf, which is required"
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
                failwith $"Collection '%s{collName}' has no lookup operation, but contains public GET resource operations, which requires a lookup operation"
            | :? PatchOperation<'ctx> ->
                failwith $"Collection '%s{collName}' has no lookup operation, but contains public PATCH resource operations, which requires a lookup operation"
            | :? DeleteOperation<'ctx> ->
                failwith $"Collection '%s{collName}' has no lookup operation, but contains public DELETE resource operations, which requires a lookup operation"
            | :? CustomOperation<'ctx> ->
                failwith $"Collection '%s{collName}' has no lookup operation, but contains public custom operations, which requires a lookup operation"
            // getRelated/getSelf is not included since it is always present when the
            // relationship is gettable, and it may be possible to get related resources
            // as includes in a compound document without the parent resource having a
            // lookup operation.
            //
            // Also, PatchSelf is not included since it can be used when creating
            // resources, which does not require a lookup operation.
            | :? RelationshipHandlers<'ctx> as op when op.PostSelf.IsSome || op.DeleteSelf.IsSome ->
                failwith $"Resource module '%s{m.Name}' has no lookup operation, but contains public relationships with a POST/DELETE operation, which requires a lookup operation"
            | _ -> ()
        )
      )
  )


let private ensureHasModifyingOperationsIfPreconditionsDefined<'ctx> (m: Type) =
  if hasPreconditions<'ctx> m then
    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
    |> Array.exists (fun x ->
        match x.GetValue(null) with
        | :? PatchOperation<'ctx> -> true
        | :? DeleteOperation<'ctx> -> true
        | :? CustomOperation<'ctx> as op -> op.HasModifyingOperations
        | :? RelationshipHandlers<'ctx> as op -> op.IsSettable
        | _ -> false
    )
    |> function
        | true -> ()
        | false -> failwith $"Resource module '%s{m.Name}' contains a precondition definition that is unused because the module contains no resource setters or other modifying operations"


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
        failwith $"Resource module '%s{m.Name}' contains %i{count} links named '%s{name}'; names must be unique"
      elif name = "relationships" then
        // This might actually be possible, though not recommended since it would be
        // confusing. Block it fow now, add test for it later if it should be supported.
        failwith $"Resource module '%s{m.Name}' contains a link named 'relationships'; this is not allowed since it conflicts with standard JSON:API URLs"
  )

  rels
  |> Array.countBy id
  |> Array.iter (fun (name, count) ->
      if count > 1 then
        failwith $"Resource module '%s{m.Name}' contains %i{count} relationships named '%s{name}'; names must be unique"
      elif name = "relationships" then
        // This might actually be possible, though not recommended since it would be
        // confusing. Block it fow now, add test for it later if it should be supported.
        failwith $"Resource module '%s{m.Name}' contains a relationship named 'relationships'; this is not allowed since it conflicts with standard JSON:API URLs"
  )

  for opName in ops do
  for relName in rels do
    if opName = relName then
      failwith $"Resource module '%s{m.Name}' contains both a relationship and a link named '%s{opName}'; names must be unique among relationships and links"


let private ensureNoConstraintsAttrIfAnyFieldHasConstraints<'ctx> m =
  let fieldWithConstraints =
    constrainedFields<'ctx> m
    |> Array.tryPick (fun f -> if f.HasConstraints then Some f.Name else None)
  let hasConstraintsField = fields<'ctx> m |> Array.exists(fun f -> f.Name = "constraints")
  match fieldWithConstraints, hasConstraintsField with
  | Some fieldName, true -> failwith $"Resource module '%s{m.Name}' contains both a field '%s{fieldName}' with constraints and a separate field called 'constraints'; either rename the 'constraints' field or remove all constraints from all fields"
  | _ -> ()



let private ensurePolymorphicModuleHasOnlyPolymorphicOperations<'ctx> m =
  let resDef = resourceDefinition<'ctx> m

  if resDef.TypeName = Constants.polymorphicTypeName then
    m.GetProperties(BindingFlags.Public ||| BindingFlags.Static)
    |> Array.iter (fun x ->
        match x.GetValue(null) with
        | :? Field<'ctx> ->
            failwith $"Polymorphic resource module '%s{m.Name}' may only contain polymorphic GET collection and lookup operations, but contained a field"
        | :? GetResourceOperation<'ctx> ->
            failwith $"Polymorphic resource module '%s{m.Name}' may only contain polymorphic GET collection and lookup operations, but contained a GET resource operation"
        | :? PatchOperation<'ctx> ->
            failwith $"Polymorphic resource module '%s{m.Name}' may only contain polymorphic GET collection and lookup operations, but contained a PATCH operation"
        | :? DeleteOperation<'ctx> ->
            failwith $"Polymorphic resource module '%s{m.Name}' may only contain polymorphic GET collection and lookup operations, but contained a DELETE operation"
        | :? CustomOperation<'ctx> ->
            failwith $"Polymorphic resource module '%s{m.Name}' may only contain polymorphic GET collection and lookup operations, but contained a custom link"
        | _ -> ()
    )


let private ensureTypeNameIsValid<'ctx> m =
  let resDef = resourceDefinition<'ctx> m
  if resDef.TypeName <> Constants.polymorphicTypeName && not (MemberName.isValid resDef.TypeName) then
    failwith $"Resource module '%s{m.Name}' uses type name '%s{resDef.TypeName}', which is not a valid JSON:API member name"


let private ensureNoDuplicateTypeNames<'ctx> ms =
  ms
  |> Array.map (fun m -> (resourceDefinition<'ctx> m).TypeName)
  |> Array.countBy id
  |> Array.iter (fun (name, count) ->
    if count > 1 && name <> Constants.polymorphicTypeName then
      failwith $"Resource type name '%s{name}' exists in %i{count} resource modules; type names must be unique"
  )


let private ensureAtMostOneLockSpecPerCollection<'ctx> ms =
  ms
  |> Array.choose (fun m -> (resourceDefinition<'ctx> m).CollectionName)
  |> Array.distinct
  |> Array.iter (lockSpec<'ctx> >> ignore)


let validateAll<'ctx> ms =
  ms |> Array.iter ensureValidAndUniqueFieldNames<'ctx>
  ms |> Array.iter ensureCollectionNameIfNeeded<'ctx>
  ms |> Array.iter ensureHasGetResourceOpIfNeeded<'ctx>
  ms |> Array.iter ensureNoRelLinkNameCollisions<'ctx>
  ms |> Array.iter ensureNoConstraintsAttrIfAnyFieldHasConstraints<'ctx>
  ms |> Array.iter ensurePolymorphicModuleHasOnlyPolymorphicOperations<'ctx>
  ms |> Array.iter ensureTypeNameIsValid<'ctx>
  ms |> Array.iter ensureHasModifyingOperationsIfPreconditionsDefined<'ctx>
  ms |> Array.iter ensureHasPersistFunction<'ctx>
  ms |> ensureNoDuplicateTypeNames<'ctx>
  ms |> ensureHasLookupIfNeeded<'ctx>
  ms |> ensureAtMostOneLockSpecPerCollection<'ctx>
