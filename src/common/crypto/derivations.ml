module P = Primitives.PBKDF2 

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

module UnassistedKeys : Keys = struct
  type username = string
  type password = string
  type servername = string
  type salt = string

  type t = {
    username : username;
    password : password;
    servername : servername;
    salt : salt;
  }

  let data = ref None
  let create_salt username password servername =
    let s_user = "" in
    data := Some {
      username = username;
      password = password;
      servername = servername;
      salt = s_user;
    } 

  type k_user = P.t
  type k_enc = P.t
  type k_mac = P.t
  type k_iv = P.t
  exception SaltNotCreated

  let get_k_user () = match !data with
    None -> raise SaltNotCreated
    | Some d -> P.apply d.password d.salt Constants.n_user Constants.b_k_dsa
  let get_k_enc () = match !data with
    None -> raise SaltNotCreated
    | Some d -> P.apply d.password d.salt Constants.n_enc Constants.b_k_enc
  let get_k_mac () = match !data with
    None -> raise SaltNotCreated
    | Some d -> P.apply d.password d.salt Constants.n_mac Constants.b_k_mac
  let get_k_iv () = match !data with
    None -> raise SaltNotCreated
    | Some d -> P.apply d.password d.salt Constants.n_iv Constants.b_k_iv
end

(*
module AssistedKeys = struct
  type k_user = string
  type k_enc = string
  type k_mac = string
  type k_iv = string
  let get_k_user () = unit -> k_user
  let get_k_enc () = unit -> k_enc
  let get_k_mac () = unit -> k_mac
  let get_k_iv () = unit -> k_iv
end
*)
