open OUnit

open Common
open Messages_t

module DB = Sql_database.DB
module D = Derivations.UnassistedKeys
module P = Primitives

exception InvalidBatchNumbering
exception InternalError

let fill_string prefix number total =
  let number_str = string_of_int number in
  let diff = total - (String.length prefix) - (String.length number_str) in
  if diff < 0
  then
    raise InvalidBatchNumbering
  else
    let zeroes = String.make diff '0' in
    prefix ^ zeroes ^ number_str

let random_prefix size seed =
  String.make size '0'
  (*
  let s = String.create size in
  let char_seed = char_of_int (seed mod 256) in
  for i = 0 to size / 16 do
    s.[i * 16] <- char_seed
  done;
  s
  *)

let db_name number value =
  Printf.sprintf "nigori.db.n%s.v%s"
    (fill_string "" number 6)
    (fill_string "" value 6)

let test_data () =
  let min_value = 4 * 1024 - 1in
  let min_number = 1 in

  let iter_value = 4 in
  let iter_number = 4 in

  let mult_value = 2 in
  let mult_number = 10 in

  let username = "username" in
  let password = "password" in
  let servername = "servername" in
  let manager = D.create ~username ~password ~servername in

  let test_key = "-----test-key-000000" in
  let prefix_rev = "-----test-rev-" in
  let prefix_val = "-----test-val-" in
  let min_size = 20 in

  let secret_key = D.enc_index ~manager ~plaintext:test_key in

  let user_key = fst (P.DSA.nigori_new_key ()) in
  let user_hash = User.string_to_hash (P.DSA.hash_key user_key) in

  for i = 0 to iter_value do
    for j = 0 to iter_number do
      let value =
        min_value *
        (int_of_float
          ((float_of_int mult_value) ** (float_of_int i))) in
      let number =
        min_number *
          (int_of_float ((float_of_int mult_number) ** (float_of_int j))) in
      let name = db_name number value in

      let database = DB.create ~name () in
      let _ = DB.add_user
        ~database
        ~pub_key:user_key
        ~pub_hash:user_hash in
      let user = match DB.get_user ~database ~pub_hash:user_hash with
      | None -> raise InternalError
      | Some u -> u in

      for k = 1 to number do
        let prefix = (random_prefix (value - min_size) k) in

        let rev_str =  fill_string prefix_rev k min_size in
        let val_str =  prefix ^ (fill_string prefix_val k min_size) in

        let secret_rev = D.enc_revision ~manager ~plaintext:rev_str in
        let secret_val = D.enc_value ~manager ~plaintext:val_str in
        if k == 1 then
          Printf.printf "%d -> %d ::: %d ->%d\n"
            (String.length rev_str)
            (String.length secret_rev)
            (String.length val_str)
            (String.length secret_val);

        let res = DB.put_record
          ~database ~user
          ~key:secret_key
          ~revision:secret_rev
          ~data:secret_val in
        assert_true res "Could not put into DB";
        ()
      done;
      let revs = match DB.get_revisions ~database ~user ~key:secret_key with
        | None -> raise InternalError
        | Some r -> r in
      Printf.printf "Database %s -> %d\n"
        name
        (List.length revs)

      (*
      ;
      match DB.get_record ~database ~user ~key:secret_key () with
      | None -> raise InternalError
      | Some record -> begin
        let f r = begin
          Printf.printf "%s :::::::: %s\n" 
          (Utils.to_hex r.revval_revision)
          (Utils.to_hex r.revval_value)
        end in
        List.iter f record
      end
      *)
    done;
  done;

  assert_true true "Bla"
  (* let ret = add_user ~database ~pub_key ~pub_hash in *)

(* Test fixtures combined. *)
let test_fixtures =
  let name = "Batch" in
  let tests = [
    ("data", test_data);
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
