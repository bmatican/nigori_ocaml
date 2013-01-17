open Printf
open String
open List
open Cryptokit

let to_hex message = Cryptokit.(transform_string (Hexa.encode ()) message)

module InitializationVector = struct
  type t = string
  exception InvalidVectorSize
  let valid_vector_size = 16

  let create value = 
    if String.length value != valid_vector_size
    then raise InvalidVectorSize
    else value
end

module AES256 = struct
  type t = string
  type key = string
  type text = string
  exception InvalidKeyLength (* Accepts 16, 24, 32 bytes *)

  let aes_block_size = 16

  module R = Cryptokit.Random
  let random_seed = R.(string secure_rng 55)
  let random_gen size = R.string (R.pseudo_rng random_seed) size

  let get_random_iv () = random_gen aes_block_size

  let apply iv key text =
    try 
      let transform = Cryptokit.(
        Cipher.aes
          ~iv:iv
          ~mode:Cipher.CBC
          ~pad:Padding._8000
          key
          Cipher.Encrypt
      ) in
      Cryptokit.(transform_string transform text)
    with 
      exn -> raise InvalidKeyLength

  let to_string message = to_hex message
end

module Utils = struct
  exception InvalidListLength
  let take_first l n = 
    let rec take_first_helper l n = 
      if n <= 0 then []
      else match l with
        [] -> raise InvalidListLength
        | h :: t -> h :: take_first_helper t (n - 1) in
    List.rev (take_first_helper l n)

  let rec pad_with_zeros l n = 
    if n <= 0
    then l
    else pad_with_zeros (0 :: l) (n - 1)

  let rec split_by_octets nr pieces = 
    if nr <= 0
    then pieces
    else 
      let octet_size = 8 in
      let size = 1 lsl octet_size in
      let mask = size - 1 in
      split_by_octets (nr lsr octet_size) ((nr land mask) :: pieces)

  let four_octet_encode nr = 
    let size = 4 in
    let octets = split_by_octets nr [] in
    let diff = size - (List.length octets) in
    let first_four =
      if diff > 0 
      then pad_with_zeros octets diff
      else take_first octets size in
    let result = String.create size in
    let set_result index nr =
      String.set result index (char_of_int nr) in
    List.iteri set_result first_four;
    result

  let concat a b = a ^ b

  let concat_list l = List.fold_left concat "" l

  exception InvalidHashLength

  let xor a b = 
    if String.length a == 0
    then b
    else
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

module SHA1 = struct
  type t = string
  type message = string

  let apply message = Cryptokit.(hash_string (Hash.sha1 ()) message)

  let to_string message = to_hex message
end

module HMAC = struct
  type t = string
  type key = string
  type message = string
  let apply key message = Cryptokit.(hash_string (MAC.hmac_sha1 key) message)
  let to_string message = to_hex message
end

module PBKDF2 = struct
  type t = string
  type hash = SHA1.t
  type password = string
  type salt = string
  type count = int
  type dk_length = int
  exception DerivedKeyTooLong

  let hash_length = 20 (* sha1 output *)

  let rec u_compute password salt count iterations current_count result =
    if current_count > count
    then result
    else 
      let u_partial = (* U_i from RFC2898 *)
        if current_count == 1 (* TODO: is this correct? *)
        then 
          Utils.concat 
            salt 
            (Utils.four_octet_encode iterations)
        else List.hd result in 
      let message = HMAC.apply password u_partial in
      let next = message :: result in 
      u_compute password salt count iterations (current_count + 1) next

  let f password salt count iterations = 
    let result = u_compute password salt count iterations 1 [] in 
    Utils.xor_list result

  let rec t_compute password salt count current_step steps last_length result = 
    let f_result = f password salt count current_step in
    if current_step >= steps
    then
      (String.sub f_result ((steps - 1) * hash_length) last_length) :: result
    else
      let next = f_result :: result in
        t_compute password salt count (current_step + 1) steps last_length next

  let apply password salt count dk_length = 
    if dk_length > hash_length * (1 lsl 32 - 1)
    then raise DerivedKeyTooLong
    else
      let l = int_of_float (ceil ((float dk_length) /. (float hash_length))) in
      let r = dk_length - (l - 1) * hash_length in
      let pieces = t_compute password salt count 1 l r [] in
      Utils.concat_list pieces

  let to_string message = to_hex message
end

module Enc = struct
  type t = string
  type key = string
  type plaintext = string

  let create : InitializationVector.t -> AES256.t -> HMAC.t -> t = 
    fun iv cipher hmac -> Utils.concat_list [iv; cipher; hmac]

  let enc key1 key2 plaintext = 
    let iv = AES256.get_random_iv () in
    let cipher = AES256.apply iv key1 plaintext in
    let hmac = HMAC.apply key2 cipher in
    create iv cipher hmac

  let enc_det key1 key2 key3 plaintext = 
    let temp = HMAC.apply key3 plaintext in
    let half = (String.length temp) / 2 in
    let f1 = String.sub temp 0 half in
    let f2 = String.sub temp half half in
    let g = Utils.xor f1 f2 in
    let cipher = AES256.apply g key1 plaintext in
    let hmac = HMAC.apply key2 cipher in
    create g cipher hmac

  let to_string message = to_hex message
end

(* TODO: put this in when we have it *)
module DSA = struct
  type t = string
  type public_key = string
  type secret_key = string
  let pub_key secret_key = ""
  let sign secret_key message = message
  let verify public_key message signature = true
end
