type t
type time = int
type random = int
val create_exact : time -> random -> t
val create_timed : time -> t
val create_random : unit -> t
