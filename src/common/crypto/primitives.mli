module Random : sig
  val generate : int -> string
end

module InitializationVector : sig
  type t
  exception InvalidVectorSize
  val valid_size : int
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
  val valid_size : int
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

module DSA : sig
  open Cryptokit

  type group_info =
      { size : int;
  p : Gmp.Z.t;
  q : Gmp.Z.t;
  base : Gmp.Z.t}

  (* This data-type is used to hold information about the group that the
    signatures will be computed in.  [size] is the size of the prime [p] in
    bits, [p] and [q] are the primes used in DSA, and [base] is a generator of
    the unique cyclic group of order [q] in $(\mathbb{Z}/\mathbb{Z}_p)^*$ *)

  type key = group_info * Gmp.Z.t 

  (* Data-type used for holding both the [group_info] and either a public
             or private key *)

  val sign: ?rng : Random.rng -> string -> key -> string * string

  (* [sign ?rng msg key] returns the signature (r,s) of the message
      [msg] computed using private key [key].  If [rng] is not
      specified [sign] uses the GMP default PRNG to generate the
      one time secret $k$. *)

  val verify: string -> string * string -> key -> bool

  (* [verify msg signature key] verifies if [signature] is a valid
      signature on message [msg] created with the private counterpart
      of public key [key].  [verify] returns true if the signature is
      valid, and false otherwise. *)

  val new_key: ?rng: Random.rng -> ?grp_info: group_info -> ?num_bits: int -> string -> key * key

  (* [new_key ?rng ?grp_info ?num_bits ?xseed] returns the generated keypair
      [(public_key,private_key)].  If [rng] is not specified, [new_key]
      will use the GMP default PRNG to generate the long-term secret
      $x$ as well as the public group information ([q],[p],[base]) if
      [grp_info] is not supplied.  [num_bits] is the length of the prime
      [p] in bits.  If it is not supplied, it will default to the FIPS
      186-2 specified value of 1024.  [xseed] is an additional
      seed value for the PRNG if additional seeding is not
      desired pass the empty string as [xseed]. *)

  (* TODO: NIGORI SPECIFIC *)
  val make_group : int -> string -> string -> string -> group_info
  val serialize_key: key -> string
  val deserialize_key: string -> key
  val hash_key : key -> string

  val nigori_sign: string -> key -> string * string
  val nigori_verify: string -> string * string -> key -> bool
  val print_key: key -> unit
  val nigori_new_key: unit -> key * key
end
