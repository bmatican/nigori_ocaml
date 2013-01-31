type t
type key = string
type value = string
val create : key -> value -> t
val get_key : t -> key
val get_value : t -> value
val encode_key : key -> string
val encode_value : value -> string
val to_string : t-> string
