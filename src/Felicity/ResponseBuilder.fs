namespace Felicity

type internal ResponseBuilder<'ctx> =
  abstract Write: 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) -> Async<ResourceDocument>
  abstract WriteList: 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) list -> Async<ResourceCollectionDocument>
  abstract WriteOpt: 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) option -> Async<ResourceDocument>
