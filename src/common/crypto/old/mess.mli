module type Interface = sig
  exception UnimplementedMethodException
  exception MalformatedMessageException
  type t
  val url_suffix : unit -> string
  val create : (string, string) Hashtbl.t -> t
  val to_json : t -> string
end

(* Default implementation to include. *)
module Base : Interface

(* Messages that the server understands. *)
module Register : Interface
