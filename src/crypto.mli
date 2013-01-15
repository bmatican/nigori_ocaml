module Nigori_sha1 : sig 
  type t
  val apply : string -> t
  val to_string : t -> string
end

module Nigori_pbkdf2 : sig
  type t
  type prf
  val apply : prf -> string -> string -> int -> int -> t
  val to_string : t -> string
end

module Nigori_hmac : sig
  type t
  val apply : string -> string -> t
  val to_string : t -> string
end
