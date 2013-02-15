open Cohttp
open Cohttp_lwt_unix

type t
type out = (Response.t * Body.t) Lwt.t

val create: Database.t -> Request.t -> Body.t -> t

val get: t -> out
val get_indices: t -> out
val get_revisions: t -> out
val put: t -> out
val delete: t -> out
val update: t -> out
val authenticate: t -> out
val register: t -> out
val unregister: t -> out

val default: t -> out
