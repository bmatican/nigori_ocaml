open Cryptokit
open Printf
open String
open List

let hexa_string message = Cryptokit.(transform_string (Hexa.encode ()) message)

(* TODO: figure out if I need to do things this way... *)
module Nigori_octet_encoding = struct
  exception InvalidListLength
  let rec take l n = match l with
    [] -> if n >= 0 then [] else raise InvalidListLength
    | h :: t -> h :: take t (n - 1)

  let convert_to_hex nr = Printf.sprintf "%x" nr

  let rec split_by_octets nr pieces = match nr with
    0 -> pieces
    | _ -> let octet_size = (1 lsl 4) in
      let mask = octet_size - 1 in
      split_by_octets 
        (nr lsr octet_size) 
        ((convert_to_hex (nr land mask)) :: pieces)

  let four_octet_encode nr = 
    let pieces = ["00";"00";"00";"00"] in
    String.concat "" (take (split_by_octets nr pieces) 4)
end

module Nigori_sha1 = struct
  type t = string
  exception InvalidHashLength

  let apply message = Cryptokit.(hash_string (Hash.sha1 ()) message)

  let to_string message = hexa_string message

  let xor a b = 
    if String.length a != String.length b
    then raise InvalidHashLength
    else
      let result = String.copy a in
      let iterator i c = 
        let xor_int = (int_of_char result.[i]) lxor (int_of_char c) in
        let xor_char = char_of_int xor_int in
        String.set result i xor_char in
      String.iteri iterator b;
      result

  let xor_list l = List.fold_left xor "" l
end

module Nigori_pbkdf2 = struct
  type t = string
  type u = Nigori_sha1.t
  (* type prf = Nigori_sha1.apply *)
  type prf = string (* need to make this work *)
  type password = string
  type salt = string
  exception DerivedKeyTooLong

  let h_len = 20 (* sha1 output *)

  let rec f_helper password salt count iteration current result =
    if current <= 0
    then result
    else 
      let element = 
        if iteration == current 
        then salt (* need to add the iteration *)
        else List.hd result in 
      let next = (Nigori_sha1.apply element) :: result in 
      f_helper password salt count iteration current next

  let f password salt count iteration = 
    let result = f_helper password salt count iteration iteration [] in 
    Nigori_sha1.xor_list result

  let apply prf password salt count dk_length = 
    if dk_length > h_len * (1 lsl 32 - 1)
    then raise DerivedKeyTooLong
    else
      let l = int_of_float (ceil ( (float dk_length) /. (float h_len))) in
      let r = dk_length - (l - 1) * h_len in
      "" (* TODO: continue from here *)

  let to_string message = hexa_string message
end

module Nigori_hmac = struct
  type t = string
  let apply key message = Cryptokit.(hash_string (MAC.hmac_sha256 key) message)
  let to_string message = hexa_string message
end
