open Common

(* Test cases for utils. *)
let valid_utils = [
  ["97"; "\000\000\000a"];
]

(* Test function for utils. *)
let test_utils () =
  let f test = 
    let nr = int_of_string (List.nth test 0) in
    let expected = List.nth test 1 in
    let result = Utils.four_octet_encode nr in
    (expected, result) in
  test_function valid_utils f

let test_fixtures = 
  let name = "Utils" in
  let tests = [
    ("utils", test_utils);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
