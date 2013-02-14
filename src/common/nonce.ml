type time = int
type random = int
type t = time * random

let random_size = 4 (* INT SIZE *)
let random_int () = 
  let rand = Primitives.Random.generate random_size in
  (Utils.four_octet_decode rand)

let create_exact time rand = (time, rand)

let create_timed time = 
  let rand = random_int () in
  create_exact time rand

let create_random () = 
  let rand = random_int () in
  let time = int_of_float (Unix.gettimeofday ()) in
  create_exact time rand

(*TODO*)
let is_recent nonce = false
