module Utils : sig
  val four_octet_encode : int -> string
  val concat : string -> string -> string
  val concat_list : string list -> string
end

module SHA1 : sig 
  type t
  exception InvalidHashLength
  val apply : string -> t
  val to_string : t -> string
  val xor : t -> t -> t
  val xor_list : t list -> t
end

module HMAC : sig
  type t
  val apply : string -> string -> t
  val to_string : t -> string
end

module PBKDF2 : sig
  type t
  type hash
  type password = string
  type salt = string
  exception DerivedKeyTooLong

  val f : password -> salt -> int -> int -> hash
  val apply : password -> salt -> int -> int -> t
  val to_string : t -> string
end
