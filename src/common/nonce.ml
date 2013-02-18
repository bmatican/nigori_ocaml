type t = {
  since_epoch : int;
  random : int;
}
exception InvalidLengthException

let offset_past = 60 * 60 * 24 * 2
let offset_future = 60 * 60

let random_size = 4 (* INT SIZE *)
let int_size = 4

let _ = Random.self_init ()
let random_int () = 
  Random.bits ()
  (*
  let rand = Primitives.Random.generate random_size in
  (Utils.bin2int rand)
  *)

let now () = 
  int_of_float (Unix.gettimeofday ())

let create_exact since_epoch random = 
  {
    since_epoch = since_epoch;
    random = random;
  }

let create_timed since_epoch = 
  let random = random_int () in
  create_exact since_epoch random

let create_random () = 
  let random = random_int () in
  let since_epoch = now () in
  create_exact since_epoch random

let get_time nonce = nonce.since_epoch
let get_nt nonce = Utils.int2bin nonce.since_epoch

let get_random nonce = nonce.random
let get_nr nonce = Utils.int2bin nonce.random

let from_string str =
  if String.length str != random_size + int_size
  then raise InvalidLengthException
  else
    let random = Utils.bin2int (String.sub str 0 random_size) in
    let since_epoch = Utils.bin2int (String.sub str random_size int_size) in
    create_exact since_epoch random

let to_string nonce =
  let random = Utils.int2bin nonce.random in
  let since_epoch = Utils.int2bin nonce.since_epoch in
  random ^ since_epoch

let is_recent nonce =
  let current = now () in
  let diff = current - nonce.since_epoch in
  (diff < offset_past) && (diff > -offset_future)
