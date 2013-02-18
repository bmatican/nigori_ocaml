exception InvalidHashLength
exception InvalidEncodingLength

val to_hex : string -> string

val bin2int : string -> int
val int2bin : int -> string

val four_octet_encode : int -> string
val four_octet_decode : string -> int

val xor : string list -> string
val concat : string list -> string

val encode_length : string list -> string
val decode_length : string -> string list

val hash_key : string -> string
