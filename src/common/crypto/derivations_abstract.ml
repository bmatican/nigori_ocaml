module type Keys = sig
  type t

  val create: username:string -> password:string -> servername:string -> t

  type k_user
  type k_enc
  type k_mac
  type k_iv

  val generate_k_user : t -> k_user
  val to_string_k_user: k_user -> string

  val generate_k_enc : t -> k_enc
  val to_string_k_enc: k_enc -> string

  val generate_k_mac : t -> k_mac
  val to_string_k_mac: k_mac -> string

  val generate_k_iv : t -> k_iv
  val to_string_k_iv: k_iv -> string

  val enc_index: t -> string -> string
  val enc_revision: t -> string -> string
  val enc_value: t -> string -> string
end
