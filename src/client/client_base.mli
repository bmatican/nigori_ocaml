module Client : sig
  type t
  val create :
    ?address:string ->
    ?port:int ->
    ?keys:(Primitives.DSA.key * Primitives.DSA.key) option ->
    username:string ->
    password:string ->
    servername:string ->
    t

  val authenticate : t -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t
  val register : t -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t
  val unregister : t -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t

  val put : t -> string -> string -> string -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t

  val delete : t -> string -> ?revision:string option -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t

  val get : t -> string -> ?revision:string option -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t
  val decode_get : t -> string -> string

  val get_indices : t -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t
  val decode_get_indices : t -> string -> string

  val get_revisions : t -> string -> ?decode:(string -> string) -> ?print:bool -> string -> unit Lwt.t
  val decode_get_revisions : t -> string -> string
end
