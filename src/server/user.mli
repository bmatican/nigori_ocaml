type t
type public_key
type hash

val create : public_key -> hash -> t
val make_key : string -> public_key
val make_hash : string -> hash

val get_name : t -> string
val get_key : t -> public_key
val get_hash : t -> hash
val get_registration_date : t -> float
