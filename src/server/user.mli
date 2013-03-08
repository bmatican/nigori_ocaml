type t
type public_key = Primitives.DSA.key
type hash

val create : public_key -> hash -> t

val string_to_key: string -> public_key
val key_to_string: public_key -> string

val string_to_hash: string -> hash
val hash_to_string: hash -> string

val get_name : t -> string
val get_key : t -> public_key
val get_hash : t -> hash
val get_registration_date : t -> float

val to_string : t -> string
val from_string : string -> t
