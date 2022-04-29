﻿namespace Felicity

open System.Threading.Tasks
open Microsoft.AspNetCore.Http

type internal ResponseBuilder<'ctx> =
  abstract Write: HttpContext -> 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) -> Task<ResourceDocument>
  abstract WriteList: HttpContext -> 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) list -> Task<ResourceCollectionDocument>
  abstract WriteOpt: HttpContext -> 'ctx -> Request -> (ResourceDefinition<'ctx> * 'entity) option -> Task<ResourceDocument>
