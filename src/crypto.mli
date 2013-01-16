module Nigori_octet_encoding : sig
  val four_octet_encode : int -> string
end

module Nigori_sha1 : sig 
  type t
  exception InvalidHashLength
  val apply : string -> t
  val to_string : t -> string
  val xor : t -> t -> t
  val xor_list : t list -> t
end

module Nigori_pbkdf2 : sig
  type t
  type u
  type prf
  type password
  type salt
  exception DerivedKeyTooLong

  val f : password -> salt -> int -> int -> u
  val apply : prf -> password -> salt -> int -> int -> t
  val to_string : t -> string
end

module Nigori_hmac : sig
  type t
  val apply : string -> string -> t
  val to_string : t -> string
end
