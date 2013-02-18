type t
exception InvalidLengthException

val create_exact : int -> int -> t
val create_timed : int -> t
val create_random : unit -> t

val get_time : t -> int
val get_nt : t -> string

val get_random : t -> int
val get_nr : t -> string

val from_string : string -> t
val to_string : t -> string

val is_recent : t -> bool
