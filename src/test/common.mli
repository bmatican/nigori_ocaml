val local_assert_equal : ?msg:string -> string -> string -> unit

val assert_true : bool -> string -> unit
val assert_false : bool -> string -> unit

val test_function : 'a list -> ('a -> string * string) -> unit

val make_fixtures : string -> (string * OUnit.test_fun) list -> OUnit.test

val start_testing : OUnit.test -> OUnit.test_result list
