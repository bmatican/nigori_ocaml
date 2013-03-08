module type DB = sig
  type t

  val create : unit -> t

  val get_user : t -> User.hash -> User.t option
  val have_user : t -> User.hash -> bool
  val add_user : t -> User.public_key -> User.hash -> bool
  val delete_user : t -> User.t -> bool
  val get_public_key : t -> User.hash -> User.public_key option

  val check_and_add_nonce : t -> Nonce.t -> User.hash -> bool
  val clear_old_nonces : t -> unit

  val get_indices : t -> User.t -> string list option
  val get_record : t -> User.t -> string -> ?revision:string option -> unit -> Messages_t.revision_value list option
  val get_revisions : t -> User.t -> string -> string list option

  val put_record : t -> User.t -> string -> string -> string -> bool
  val delete_record : t -> User.t -> string -> ?revision:string option -> unit -> bool
end
