namespace Felicity



type Define<'ctx, 'entity, 'id>() =
    member _.Attribute =
        AttributeHelper<'ctx, 'ctx, 'entity>(fun ctx _ -> Ok ctx |> Task.result)

    member _.Id = IdHelper<'ctx, 'entity, 'id>()

    member _.Resource(name, id) =
        ResourceDefinition<'ctx, 'entity, 'id>.Create (name, id)

    member _.PolymorphicResource(id) =
        ResourceDefinition<'ctx, 'entity, 'id>.Create (Constants.polymorphicTypeName, id)

    member _.Relationship =
        RelationshipHelper<'ctx, 'ctx, 'entity>(fun ctx _ -> Ok ctx |> Task.result)

    member _.Operation = OperationHelper<'ctx, 'ctx, 'entity, 'id>(Ok >> Task.result)
    member _.Preconditions = Preconditions<'ctx, 'entity>.Create ()
