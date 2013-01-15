open Cryptokit

let hexa_string message = Cryptokit.(transform_string (Hexa.encode ()) message)

module Nigori_sha1 = struct
  type t = string
  let apply message = Cryptokit.(hash_string (Hash.sha1 ()) message)
  let to_string message = hexa_string message
end

module Nigori_pbkdf2 = struct
  type t = string
  (* type prf = Nigori_sha1.apply *)
  type prf = string (* need to make this work *)
  let apply prf password salt count dk_length = password
  let to_string message = hexa_string message
end

module Nigori_hmac = struct
  type t = string
  let apply key message = Cryptokit.(hash_string (MAC.hmac_sha256 key) message)
  let to_string message = hexa_string message
end
