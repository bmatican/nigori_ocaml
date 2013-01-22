module type Keys = sig
  type username
  type password
  type servername
  type salt

  type t = {
    username : username;
    password : password;
    servername : servername;
    salt : salt;
  }

  val data : t option ref
  val create_salt : username -> password -> servername -> unit

  type k_user
  type k_enc
  type k_mac
  type k_iv
  exception SaltNotCreated

  val get_k_user : unit -> k_user
  val get_k_enc : unit -> k_enc
  val get_k_mac : unit -> k_mac
  val get_k_iv : unit -> k_iv
end

module UnassistedKeys : Keys
