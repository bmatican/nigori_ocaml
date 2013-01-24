module Random : sig
  val generate : int -> string
end

module InitializationVector : sig
  type t
  exception InvalidVectorSize
  val create : string -> t
  val create_random : unit -> t
  val to_string : t -> string
end

module AES256 : sig
  type t
  type key = string
  exception InvalidKeyLength (* Accepts 16, 24, 32 bytes *)
  val encrypt : InitializationVector.t -> key -> string -> t
  (* TODO: Should only decrypt type t, but we can't cast otherwise... *)
  val decrypt : InitializationVector.t -> key -> string -> string
  val to_string : t -> string
end

module SHA1 : sig 
  type t
  type message = string
  val apply : message -> t
  val to_string : t -> string
end

module HMAC : sig
  type t
  type key = string
  type message = string
  val apply : key -> message -> t
  val to_string : t -> string
end

module PBKDF2 : sig
  type t
  type password = string
  type count = int
  type dk_length = int
  type salt = string
  exception DerivedKeyTooLong

  val f : password -> salt -> count -> dk_length -> string
  val apply : password -> salt -> count -> dk_length -> t
  val to_string : t -> string
end

module Enc : sig
  type t
  type key = string
  type plaintext = string
  val enc : key -> key -> plaintext -> t
  val enc_det: key -> key -> key -> plaintext -> t
  val to_string : t -> string
end

module DSA : sig
  type t
  type public_key = string
  type secret_key = string
  val pub_key : secret_key -> public_key
  val sign : secret_key -> string -> t
  val verify : public_key -> string -> t -> bool
end
