open OUnit
open Common
open Cryptokit

module P = Primitives
module D = Derivations.UnassistedKeys

let create_manager () =
  let username = "username" in
  let password = "password" in
  let servername = "servername" in
  D.create ~username ~password ~servername

let valid_enc = [
  ["plaintext"];
]

(* Fake test just to run enc. *)
let test_enc () =
  let manager = create_manager () in
  let f test =
    let plaintext = List.nth test 0 in
    let expected = plaintext in
    let _ = D.enc_value ~manager ~plaintext in
    let _ = D.enc_index ~manager ~plaintext in
    let _ = D.enc_revision ~manager ~plaintext in
    (expected, plaintext) in
  test_function valid_enc f

let generic_enc_dec encoder decoder = 
  let manager = create_manager () in
  let valid_data = [["plaintext";];] in
  let f test =
    let plaintext = List.nth test 0 in
    let expected = plaintext in
    let result = decoder ~manager ~cypher:(encoder ~manager ~plaintext) in
    (expected, result) in
  test_function valid_data f

let test_enc_dec_index () =
  generic_enc_dec D.enc_index D.dec_index
let test_enc_dec_revision () =
  generic_enc_dec D.enc_revision D.dec_revision
let test_enc_dec_value () =
  generic_enc_dec D.enc_value D.dec_value

(* Test fixtures combined. *)
let test_fixtures =
  let name = "Derivations" in
  let tests = [
    ("enc", test_enc);
    ("enc_dec_index", test_enc_dec_index);
    ("enc_dec_revision", test_enc_dec_revision);
    ("enc_dec_value", test_enc_dec_value);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
