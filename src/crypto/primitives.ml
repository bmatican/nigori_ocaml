module Random = struct
  module R = Cryptokit.Random
  let random_seed = R.(string secure_rng 55) (* Maximum size for pseudo_rng *)
  let random_gen size = R.string (R.pseudo_rng random_seed) size

  let generate size = random_gen size
end

module InitializationVector = struct
  type t = string
  exception InvalidVectorSize
  let valid_vector_size = 16 (* Taken from cryptokit. *)

  let create value = 
    if String.length value != valid_vector_size
    then raise InvalidVectorSize
    else value

  let create_random () = 
    create (Random.generate valid_vector_size)

  let to_string message = message
end

module AES256 = struct
  type t = string
  type key = string
  exception InvalidKeyLength (* Accepts 16, 24, 32 bytes *)

  let encrypt iv key text =
    try 
      let transform = Cryptokit.(
        Cipher.aes
          ~iv:iv
          ~mode:Cipher.CBC
          ~pad:Padding.length (* As seen in the Nigori python version. *)
          key
          Cipher.Encrypt
      ) in
      Cryptokit.(transform_string transform text)
    with 
      exn -> raise InvalidKeyLength

  let decrypt iv key text =
    try 
      let transform = Cryptokit.(
        Cipher.aes
          ~iv:iv
          ~mode:Cipher.CBC
          ~pad:Padding.length (* As seen in the Nigori python version. *)
          key
          Cipher.Decrypt
      ) in
      Cryptokit.(transform_string transform text)
    with 
      exn -> raise InvalidKeyLength

  let to_string message = message
end

module SHA1 = struct
  type t = string
  type message = string

  let apply message = Cryptokit.(hash_string (Hash.sha1 ()) message)

  let to_string message = message
end

module HMAC = struct
  type t = string
  type key = string
  type message = string
  let apply key message = Cryptokit.(hash_string (MAC.hmac_sha1 key) message)
  let to_string message = message
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
          Utils.concat [
            salt;
            (Utils.four_octet_encode iterations);
          ]
        else List.hd result in 
      let message = HMAC.apply password u_partial in
      let next = message :: result in 
      u_compute password salt count iterations (current_count + 1) next

  let f password salt count iterations = 
    let result = u_compute password salt count iterations 1 [] in 
    Utils.xor result

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
      Utils.concat pieces

  let to_string message = message
end

module Enc = struct
  type t = string
  type key = string
  type plaintext = string

  let create : InitializationVector.t -> AES256.t -> HMAC.t -> t = 
    fun iv cipher hmac -> Utils.concat [iv; cipher; hmac]

  let enc key1 key2 plaintext = 
    let iv = InitializationVector.create_random () in
    let cipher = AES256.encrypt iv key1 plaintext in
    let hmac = HMAC.apply key2 cipher in
    create iv cipher hmac

  let enc_det key1 key2 key3 plaintext = 
    let temp = HMAC.apply key3 plaintext in
    let half = (String.length temp) / 2 in
    let f1 = String.sub temp 0 half in
    let f2 = String.sub temp half half in
    let g = Utils.xor [f1; f2] in
    let cipher = AES256.encrypt g key1 plaintext in
    let hmac = HMAC.apply key2 cipher in
    create g cipher hmac

  let to_string message = message
end

(* TODO: put this in when we have it *)
module DSA = struct
  type t = string
  type public_key = string
  type secret_key = string
  let pub_key secret_key = ""
  let sign secret_key message = message
  let verify public_key message signature = 
    signature == (sign public_key message)
end
