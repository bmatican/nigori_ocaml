open OUnit

open Common
open Messages_t

module DB = Sql_database.DB
module D = Derivations.UnassistedKeys
module P = Primitives

exception InvalidBatchNumbering
exception InternalError

let user_key = fst (P.DSA.nigori_new_key ())
let user_hash = User.string_to_hash (P.DSA.hash_key user_key)

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
  let min_value = 100 in
  let min_number = 999 in

  let iter_value = 0 in
  let iter_number = 3 in

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
    done;
  done;
  assert_true true "Bla"

let rec helper_call_iter size max_iter mult cur_iter f =
  let res = (size, (f size)) in
  if cur_iter >= max_iter
  then
    [res]
  else
    res :: (helper_call_iter (size * mult) max_iter mult (cur_iter + 1) f)

let call_iter size max_iter mult f =
  helper_call_iter size max_iter mult 0 f

let rec collect f times =
  let res = f () in
  if times <= 0
  then
    [res]
  else
    res :: (collect f (times - 1))

let rec sum l = match l with
  | [] -> 0.
  | x :: l2 -> x +. (sum l2)

let mean l = (sum l) /. (float_of_int (List.length l))

let std_dev l =
  let m = mean l in
  let f x = (x -. m) ** 2.0 in
  let newl = List.map f l in
  sqrt ((sum newl) /. (float_of_int (List.length l)))

let test_aes () =
  let init_size = 1 * 1024 in
  let mult_iter = 2 in
  let iter_number = 10 in
  let iv = P.InitializationVector.create_random () in
  let key = String.create 32 in
  let f size = begin
    let g () = begin
      let start = Unix.gettimeofday () in
      let plaintext = String.create size in
      let _ = P.AES256.encrypt iv key plaintext in
      let finish = Unix.gettimeofday () in
      (finish -. start)
    end in
    let l = collect g 50 in
    ((mean l), (std_dev l))
  end in
  let times = call_iter init_size iter_number mult_iter f in
  let printer el = begin
    let size = (fst el) / 1024 in
    let data = snd el in
    let m = 1000.0 *. (fst data) in
    let std = 1000.0 *. (snd data) in
    Printf.printf "%d %f %f\n" size m std
  end in
  List.map printer times;

  assert_true true "Bla"

let test_dsa () =
  assert_true true "Bla"

let test_db () =
  let db1 = "nigori.db.n000001.v000000" in
  let db2 = "nigori.db.n000010.v000000" in
  let db3 = "nigori.db.n000100.v000000" in
  let db4 = "nigori.db.n001000.v000000" in
  let dbs = [db1;db2;db3;db4;] in
  let test_key = "-----test-key-000000" in
  let f name = begin
    let database = DB.create ~name () in
    let user = match (DB.get_user ~database ~pub_hash:user_hash) with
    | None -> raise InternalError
    | Some (u) -> u in
    let g () = begin
      let start = Unix.gettimeofday () in
      let _ = DB.get_record ~database ~user ~key:test_key () in
      let finish = Unix.gettimeofday () in
      (finish -. start)
    end in
    let l = collect g 50 in
    ((mean l), (std_dev l))
  end in
  let times = List.map f dbs in
  let printer el = begin
    let m = 1000.0 *. (fst el) in
    let std = 1000.0 *. (snd el) in
    Printf.printf "%f %f\n" m std
  end in
  List.map printer times;
  assert_true true "Bla"

(* Test fixtures combined. *)
let test_fixtures =
  let name = "Batch" in
  let tests = [
    ("data", test_data);
    ("aes", test_aes);
    ("dsa", test_dsa);
    (* ("db", test_db); *)
  ] in
  make_fixtures name tests

let _ = start_testing test_fixtures
