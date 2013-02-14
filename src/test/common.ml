open OUnit

(* Custom local_assert_equal to print problems. *)
let local_assert_equal ?msg a b = 
  match msg with
  | Some s ->
    assert_equal 
      ~msg:s
      ~printer:(fun v -> v)
      a b 
  | None ->
    assert_equal 
      ~printer:(fun v -> v)
      a b 

(* Assert true and assert false *)
let assert_true exp msg =
  local_assert_equal ~msg (string_of_bool exp) (string_of_bool true)
let assert_false exp msg =
  local_assert_equal ~msg (string_of_bool exp) (string_of_bool false)

(* General test function. Takes list of test cases and test function. *)
let test_function tests tester =
  let f test_case =
    let p = tester test_case in
    let expected = fst p in
    let result = snd p in
    local_assert_equal expected result in
  List.iter f tests

let make_fixtures test_name test_pairs = 
  let f acc el =
    let name = fst el in
    let func = snd el in
    (name >:: func) :: acc in
  let tests = List.fold_left f [] test_pairs in
  test_name >::: tests

let start_testing test_fixtures = run_test_tt ~verbose:true test_fixtures
