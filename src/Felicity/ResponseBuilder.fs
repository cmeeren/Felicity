namespace Felicity

open Hopac

type internal ResponseBuilder<'ctx> =
  abstract Write: 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) -> Job<ResourceDocument>
  abstract WriteList: 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) list -> Job<ResourceCollectionDocument>
  abstract WriteOpt: 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) option -> Job<ResourceDocument>
