open OUnit

open Common
open Messages_t
open Primitives

let pub_key = fst (DSA.nigori_new_key ())
let pub_hash = User.string_to_hash (DSA.hash_key pub_key)
let other_pub = fst (DSA.nigori_new_key ())
let other_hash = User.string_to_hash (DSA.hash_key other_pub)

let test_users () =
  let open Hash_database.DB in
  let database = create () in
  assert_false (have_user ~database ~pub_hash) "Users present in empty ~database";

  let ret = add_user ~database ~pub_key ~pub_hash in
  assert_true ret "Cannot add user";
  assert_true (have_user ~database ~pub_hash) "User not added";

  let ret = add_user ~database ~pub_key ~pub_hash in
  assert_false ret "Adding already existing user";

  let some_key = get_public_key ~database ~pub_hash in
  match some_key with
  | None -> assert_string "Cannot get pub_key of user"
  | Some key -> begin
    assert_true (pub_key == key) "Public keys do not match"
  end;

  let fake_key = get_public_key ~database ~pub_hash:other_hash in
  match fake_key with
  | None -> ()
  | Some key -> begin
    assert_string "Got public key of non-existing user"
  end;

  let some_user = get_user ~database ~pub_hash in
  match some_user with
  | None -> assert_string "Cannot get user that exists"
  | Some user -> begin
    assert_true (User.get_key user == pub_key) "User pub_key is different";
    assert_true (User.get_hash user == pub_hash) "User ~pub_hash is different";

    let ret = delete_user ~database ~user in
    assert_true ret "Cannot delete user";
    assert_false (have_user ~database ~pub_hash) "User did not get deleted";

    let ret = delete_user ~database ~user in
    assert_false ret "Delete worked on non-existing user"
  end;

  let fake_user = get_user ~database ~pub_hash:other_hash in
  match fake_user with
  | None -> ()
  | Some user -> assert_string "Got a user that shouldn't exit"

let test_records () =
  let open Hash_database.DB in
  let database = create () in
  let other_user = User.create other_pub other_hash in

  let ret = add_user ~database ~pub_key ~pub_hash in
  assert_true ret "Could not add user";

  let some_user = get_user ~database ~pub_hash in
  match some_user with
  | None -> assert_string "Could not get user"
  | Some user -> begin
    let key1 = "key1" in
    let key2 = "key2" in
    let rev1 = "rev1" in
    let rev2 = "rev2" in
    let data1 = "data1" in
    let data2 = "data2" in

    let other_key = "other_key" in
    let other_rev = "other_rev" in

    let ret = (put_record ~database ~user ~key:key1 ~revision:rev1 ~data:data1) in
    assert_true ret "Cannot add values to ~database";

    let ret = (put_record ~database ~user ~key:key1 ~revision:rev2 ~data:data2) in
    assert_true ret "Cannot add values to ~database for same key diff rev";

    let ret =  (put_record ~database ~user ~key:key2 ~revision:rev1 ~data:data1) in
    assert_true ret "Cannot add values to ~database for diff key";

    let ret = (put_record ~database ~user ~key:key2 ~revision:rev2 ~data:data2) in
    assert_true ret "Cannot add values to ~database for diff key diff rev";

    let ret = (put_record ~database ~user ~key:key1 ~revision:rev1 ~data:data1) in
    assert_true ret "Adding same data for same key same rev";

    let ret = (put_record ~database ~user ~key:key1 ~revision:rev1 ~data:data2) in
    assert_false ret "Adding same data for same key same rev";

    let fake_indices = get_indices ~database ~user:other_user in
    match fake_indices with
    | None -> ()
    | Some indices -> begin
      assert_string "Got indices for invalid user"
    end;

    let some_indices = get_indices ~database ~user in
    match some_indices with
    | None -> assert_string "Could not get indices for valid user"
    | Some indices -> begin
      assert_true (indices == [key1; key2;]) "Invalid indices for user"
    end;

    let make_rv rev data =
      {
        revval_revision = rev1;
        revval_value = data1;
      }
    in

    let rv11 = make_rv rev1 data1 in
    let rv12 = make_rv rev1 data2 in
    let indices1 = [rv11; rv12;] in
    let rv21 = make_rv rev2 data1 in
    let rv22 = make_rv rev2 data2 in

    let indices2 = [rv21; rv22;] in
    let f = (fun el ->
      let some_record = get_record ~database ~user ~key:key1 () in
      match some_record with
      | None -> assert_string "Could not get record for valid user"
      | Some record -> begin
        assert_true (record == el) "Invalid record"
      end
    ) in
    List.iter f [indices1; indices2;];

    let other_record = get_record ~database ~user:other_user ~key:key1 () in
    match other_record with
    | None -> ()
    | Some record -> begin
      assert_string "Got records for invalid user"
    end;

    let f = (fun el ->
      let input = fst el in
      let rv = snd el in
      let key = List.nth input 0 in
      let rev = List.nth input 1 in
      let some_rv = get_record ~database ~user ~key ~revision:(Some (rev)) () in
      match some_rv with
      | None -> assert_string "Could not get data for valid user, key and rev"
      | Some test_rv -> begin
        assert_true (test_rv == [rv] ) "Invalid data"
      end
    ) in
    let test = [
      ([key1;rev1], rv11);
      ([key1;rev2], rv12);
      ([key2;rev1], rv21);
      ([key2;rev2], rv22);
    ] in
    List.iter f test;

    let other_data = get_record ~database ~user:other_user ~key:key1 ~revision:(Some (rev1)) () in
    match other_data with
    | None -> ()
    | Some data -> begin
      assert_string "Got data for invalid user"
    end;

    let other_data = get_record ~database ~user ~key:other_key ~revision:(Some (rev1)) () in
    match other_data with
    | None -> ()
    | Some data -> begin
      assert_string "Got data for invalid key"
    end;

    let other_data = get_record ~database ~user ~key:key1 ~revision:(Some (other_rev)) () in
    match other_data with
    | None -> ()
    | Some data -> begin
      assert_string "Got data for invalid rev"
    end;

    let input = [
      (key1, [rev1;rev2;]);
      (key2, [rev1;rev2;]);
    ] in
    let f = (fun el ->
      let key = fst el in
      let revs = snd el in
      let some_revisions = get_revisions ~database ~user ~key in
      match some_revisions with
      | None -> assert_string "Could not get revisions for valid user and key"
      | Some revisions -> begin
        assert_true (revs == revisions) "Revisions did not match"
      end
    ) in
    List.iter f input;

    let input = [key1;key2;] in
    let f = (fun key ->
      let ret = delete_record ~database ~user ~key ~revision:None () in
      assert_true ret "Could not delete record"
    ) in
    List.iter f input;

    let f = (fun key ->
      let ret = delete_record ~database ~user ~key ~revision:None () in
      assert_false ret "Deleted record again"
    ) in
    List.iter f input
  end

(* Test fixtures combined. *)
let test_fixtures =
  let name = "Database" in
  let tests = [
    ("users", test_users);
    ("records", test_records);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
