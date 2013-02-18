open Common

let test_string () =
  for i=1 to 10 do
    (* let nonce = Nonce.create_random () in *)
    let nonce = Nonce.create_exact (1 lsl 28) (1 lsl 28) in
    let random = Nonce.get_random nonce in
    let time = Nonce.get_time nonce in
    let other_nonce = Nonce.from_string (Nonce.to_string nonce) in
    let other_random = Nonce.get_random other_nonce in
    let other_time = Nonce.get_time nonce in
    assert_true (time == other_time) "time mismatch";
    assert_true (random == other_random) "random mismatch"
  done

(* Test fixtures combined. *)
let test_fixtures = 
  let name = "Nonce" in
  let tests = [
    ("string", test_string);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
