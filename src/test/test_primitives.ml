open OUnit
open Common
open Cryptokit

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
    let result = Utils.to_hex P.SHA1.(to_string (apply m)) in
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
    let result = Utils.to_hex P.HMAC.(to_string (apply key message)) in
    (expected, result) in
  test_function valid_hmac f

(* Test cases for pbkdf2. *)
let valid_pbkdf2 = [
  ["password"; "salt"; "1"; "20"; "0c60c80f961f0e71f3a9b524af6012062fe037a6"];
]

(* Test function for pbkdf2. *)
let test_pbkdf2 () =
  let f test =
    let password = List.nth test 0 in
    let salt = List.nth test 1 in
    let iterations = int_of_string (List.nth test 2) in
    let length = int_of_string (List.nth test 3) in
    let expected = List.nth test 4 in
    let result = Utils.to_hex P.PBKDF2.(to_string (apply password salt iterations length)) in
    (expected, result) in
  test_function valid_pbkdf2 f

(* Test cases for aes256. *)
let valid_aes256 = [
  ["1234567890123456"; "1234567890123456"; "testing";];
  [""; "1111111111111111"; "random";];
]

(* Test function for aes256. *)
let test_aes256 () =
  let f test =
    let base_iv = List.nth test 0 in
    let iv =
      if String.length base_iv == 0
      then P.InitializationVector.create_random ()
      else P.InitializationVector.create base_iv in
    let key = List.nth test 1 in
    let text = List.nth test 2 in
    let cipher = P.AES256.(to_string (encrypt iv key text)) in
    let result = P.AES256.decrypt iv key cipher in
    (text, result) in
  test_function valid_aes256 f

let valid_dsa = [
  ["test1";];
  ["test2";];
]

(* Test function for DSA. *)
let test_dsa () =
  let keys = P.DSA.nigori_new_key () in
  let pub_key = fst keys in
  let priv_key = snd keys in
  let f test =
    let message = List.nth test 0 in
    let signature = P.DSA.nigori_sign message priv_key in
    let check = P.DSA.nigori_verify message signature pub_key in
    ((string_of_bool true), (string_of_bool check)) in
  test_function valid_dsa f

(* Test DSA serialization *)
let test_dsa_serialization () =
  let pub_key, priv_key = P.DSA.nigori_new_key () in
  let f key =
    let ser = P.DSA.serialize_key key in
    let next = P.DSA.serialize_key (P.DSA.deserialize_key (P.DSA.serialize_key key)) in
    (ser, next)
  in
  test_function [pub_key; priv_key;] f

(* Test fixtures combined. *)
let test_fixtures =
  let name = "Primitives" in
  let tests = [
    ("sha1", test_sha1);
    ("hmac", test_hmac);
    ("pbkdf2", test_pbkdf2);
    ("aes256", test_aes256);
    ("dsa", test_dsa);
    ("dsa_serialization", test_dsa_serialization);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
