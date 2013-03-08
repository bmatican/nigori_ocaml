open Cohttp
open Cohttp_lwt_unix

module type H = sig
  type elt
  type t
  type out = (Response.t * Body.t) Lwt.t

  val init_db : unit -> elt

  val create: elt -> Request.t -> Body.t -> t

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
end

module Make (DB : Database.DB) : H with type elt = DB.t
