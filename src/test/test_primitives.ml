open OUnit
open Common

module P = Primitives

(* Test cases for sha1. *)
let valid_sha1 = [
  ["foobar"; "8843d7f92416211de9ebb963ff4ce28125932878"];
]

(* Test function for sha1. *)
let test_sha1 () =
  let f test = 
    let m = List.nth test 0 in
    let expected = List.nth test 1 in
    let result = P.SHA1.(to_string (apply m)) in
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
    let result = P.HMAC.(to_string (apply key message)) in
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
    let result = P.PBKDF2.(to_string (apply password salt iterations length)) in
    (expected, result) in
  test_function valid_pbkdf2 f

(* Test fixtures combined. *)
let test_fixtures = 
  let name = "Primitives" in
  let tests = [
    ("sha1", test_sha1);
    ("hmac", test_hmac);
    ("pbkdf2", test_pbkdf2);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
