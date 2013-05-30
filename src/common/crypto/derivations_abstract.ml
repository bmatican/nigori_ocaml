module type Keys = sig
  exception InvalidMessageLength
  exception InvalidHmac
  type t

  val create: username:string -> password:string -> servername:string -> t
  val get_username: manager:t -> string
  val get_password: manager:t -> string
  val get_servername: manager:t -> string

  val enc_index: manager:t -> plaintext:string -> string
  val dec_index: manager:t -> cypher:string -> string

  val enc_revision: manager:t -> plaintext:string -> string
  val dec_revision: manager:t -> cypher:string -> string

  val enc_value: manager:t -> plaintext:string -> string
  val dec_value: manager:t -> cypher:string -> string
end
