open OUnit
open List

module C = Primitives

(* Custom local_assert_equal to print problems. *)
let local_assert_equal a b = 
  assert_equal 
    ~printer:(fun v -> v)
    a b 

(* General test function. Takes list of test cases and test function. *)
let test_function tests tester =
  let f test_case =
    let p = tester test_case in
    let expected = fst p in
    let result = snd p in
    local_assert_equal expected result in
  List.iter f tests

(* Test cases for utils. *)
let valid_utils = [
  ["97"; "\000\000\000a"];
]

(* Test function for utils. *)
let test_utils () =
  let f test = 
    let nr = int_of_string (List.nth test 0) in
    let expected = List.nth test 1 in
    let result = C.Utils.four_octet_encode nr in
    (expected, result) in
  test_function valid_utils f

(* Test cases for sha1. *)
let valid_sha1 = [
  ["foobar"; "8843d7f92416211de9ebb963ff4ce28125932878"];
]

(* Test function for sha1. *)
let test_sha1 () =
  let f test = 
    let m = List.nth test 0 in
    let expected = List.nth test 1 in
    let result = C.SHA1.(to_string (apply m)) in
    (expected, result) in
  test_function valid_sha1 f

(* Test cases for hmac. *)
let valid_hmac = [
  ["foo"; "bar"; "46b4ec586117154dacd49d664e5d63fdc88efb51"];
]

(* Test function for hmac. *)
let test_hmac () =
  let f test = 
    let key  = List.nth test 0 in
    let message = List.nth test 1 in
    let expected = List.nth test 2 in
    let result = C.HMAC.(to_string (apply key message)) in
    (expected, result) in
  test_function valid_hmac f

(* Test cases for hmac. *)
let valid_pbkdf2 = [
  ["password"; "salt"; "1"; "20"; "0c60c80f961f0e71f3a9b524af6012062fe037a6"];
]

(* Test function for hmac. *)
let test_pbkdf2 () =
  let f test = 
    let password = List.nth test 0 in
    let salt = List.nth test 1 in
    let iterations = int_of_string (List.nth test 2) in
    let length = int_of_string (List.nth test 3) in
    let expected = List.nth test 4 in
    let result = C.PBKDF2.(to_string (apply password salt iterations length)) in
    (expected, result) in
  test_function valid_pbkdf2 f


let test_fixtures = "Crypto" >::: 
  [
    "utils" >:: test_utils;
    "sha1" >:: test_sha1;
    "hmac" >:: test_hmac;
    "pbkdf2" >:: test_pbkdf2;
  ]

let _ = run_test_tt ~verbose:true test_fixtures
