open Common

(* Test cases for length encoding. *)
let valid_encode_length = [
  ["abc"; "\000\000\000\003abc"];
]

(* Test function for length encoding. *)
let test_encode_length () =
  let f test = 
    let element = List.nth test 0 in
    let expected = List.nth test 1 in
    let result = Utils.encode_length [element] in
    (expected, result) in
  test_function valid_encode_length f

(* Test cases for length decoding. *)
let valid_decode_length = [
  ["\000\000\000\003abc\000\000\000\004defg"; "abc"; "defg"];
]

(* Test function for length encoding. *)
let test_decode_length () =
  let f test = 
    let encoded = List.hd test in
    let expected = List.tl test in
    let result = Utils.decode_length encoded in
    (
      (String.concat "" expected),
      (String.concat "" result)
    ) in
  test_function valid_decode_length f

(* Test for encode-decode passes. *)
let test_encode_decode_length () =
  let rec ints_up_to nr = 
    if nr <= 0
    then []
    else nr :: ints_up_to (nr - 1) in
  let tests = (ints_up_to 255) in
  let f test =
    let c = char_of_int test in
    let testing = String.make 3 c in
    let to_test = [testing;testing] in
    let encoded = Utils.encode_length to_test in
    let decoded = Utils.decode_length encoded in
    (
      (String.concat "" to_test), 
      (String.concat "" decoded) 
    ) in
  test_function tests f

(* Testing bin2int and int2bin functions. *)
let test_conversion () =
  Random.self_init ();
  for i = 0 to 100 do
    let test = Random.bits () in
    let str = Utils.int2bin test in
    let nr = Utils.bin2int str in
    assert_true (test == nr) "Conversion failed"
  done

let test_fixtures = 
  let name = "Utils" in
  let tests = [
    ("encode_length", test_encode_length);
    ("decode_length", test_decode_length);
    ("encode_decode_length", test_encode_decode_length);
    ("conversion", test_conversion);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
