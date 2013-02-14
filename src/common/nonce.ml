type t = {
  since_epoch : int;
  random : int;
}
exception InvalidLengthException

let offset_past = 60 * 60 * 24 * 2
let offset_future = 60 * 60

let random_size = 4 (* INT SIZE *)
let int_size = 4

let random_int () = 
  let rand = Primitives.Random.generate random_size in
  (Utils.four_octet_decode rand)

let now () = 
  (int_of_float (Unix.gettimeofday ())) / 1000

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

let from_string str =
  if String.length str != random_size + int_size
  then raise InvalidLengthException
  else
    let random = int_of_string (String.sub str 0 random_size) in
    let since_epoch = int_of_string (String.sub str random_size int_size) in
    create_exact since_epoch random

let to_string nonce =
  let random = nonce.random in
  let since_epoch = nonce.since_epoch in
  (Utils.four_octet_encode random) ^ (Utils.four_octet_encode since_epoch)

let is_recent nonce =
  let current = now () in
  let diff = current - nonce.since_epoch in
  (diff < offset_past) && (diff > -offset_future)
