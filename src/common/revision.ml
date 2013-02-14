type key = string
type value = string
type t = key * value

let create key value = (key, value)

let get_key rev = fst rev
let get_value rev = snd rev

let encode_key k = Cohttp.Base64.encode k
let encode_value v = Cohttp.Base64.encode v

let to_string rev = 
  let k = get_key rev in
  let v = get_value rev in
  k ^ ":" ^ v
