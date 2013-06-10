module type DB = sig
  type t

  val create : ?name:string -> unit -> t

  val get_user : database:t -> pub_hash:User.hash -> User.t option
  val have_user : database:t -> pub_hash:User.hash -> bool
  val add_user : database:t -> pub_key:User.public_key -> pub_hash:User.hash -> bool
  val delete_user : database:t -> user:User.t -> bool
  val get_public_key : database:t -> pub_hash:User.hash -> User.public_key option

  val check_and_add_nonce : database:t -> nonce:Nonce.t -> pub_hash:User.hash -> bool
  val clear_old_nonces : database:t -> unit

  val get_indices : database:t -> user:User.t -> string list option
  val get_record : database:t -> user:User.t -> key:string -> ?revision:string option -> unit -> Messages_t.revision_value list option
  val get_revisions : database:t -> user:User.t -> key:string -> string list option

  val put_record : database:t -> user:User.t -> key:string -> revision:string -> data:string -> bool
  val delete_record : database:t -> user:User.t -> key:string -> ?revision:string option -> unit -> bool
end
