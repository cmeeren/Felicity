namespace Felicity

open Microsoft.AspNetCore.Http
open Hopac

type internal ResponseBuilder<'ctx> =
  abstract Write: HttpContext -> 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) -> Job<ResourceDocument>
  abstract WriteList: HttpContext -> 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) list -> Job<ResourceCollectionDocument>
  abstract WriteOpt: HttpContext -> 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) option -> Job<ResourceDocument>
