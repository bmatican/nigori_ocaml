module type Interface = sig
  exception UnimplementedMethodException
  exception MalformatedMessageException
  type t
  val url_suffix : unit -> string
  val create : (string, string) Hashtbl.t -> t
  val to_json : t -> string
end

module Base : Interface = struct
  exception UnimplementedMethodException
  exception MalformatedMessageException
  type t = (string, string) Hashtbl.t
  let create data = data
  let url_suffix () = raise UnimplementedMethodException
  let to_json = raise UnimplementedMethodException
end

module Register : Interface = struct
  include Base
  (* let create data = data *)
  let url_suffix () = "register"
  let to_json message = raise UnimplementedMethodException
end
