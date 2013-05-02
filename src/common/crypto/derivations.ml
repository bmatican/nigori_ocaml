module P = Primitives.PBKDF2
module Enc = Primitives.Enc
module C = Constants

module UnassistedKeys : Derivations_abstract.Keys = struct
  type t = {
    username : string;
    password : string;
    servername : string;
    salt : string;
  }

  let create ~username ~password ~servername =
    let user_and_server = Utils.encode_length [username; servername;] in
    let s_user = P.apply user_and_server C.user_salt C.n_salt C.b_s_user in
    {
      username = username;
      password = password;
      servername = servername;
      salt = P.to_string s_user;
    }

  type k_user = P.t
  type k_enc = P.t
  type k_mac = P.t
  type k_iv = P.t

  let generate_k_user keys = P.apply keys.password keys.salt C.n_user C.b_k_dsa
  let to_string_k_user k_user = P.to_string k_user

  let generate_k_enc keys = P.apply keys.password keys.salt C.n_enc C.b_k_enc
  let to_string_k_enc k_enc = P.to_string k_enc

  let generate_k_mac keys = P.apply keys.password keys.salt C.n_mac C.b_k_mac
  let to_string_k_mac k_mac = P.to_string k_mac

  let generate_k_iv keys = P.apply keys.password keys.salt C.n_iv C.b_k_iv
  let to_string_k_iv k_iv = P.to_string k_iv

  let enc_index keys plaintext =
    Enc.(to_string (enc_det
      (to_string_k_enc (generate_k_enc keys))
      (to_string_k_mac (generate_k_mac keys))
      (to_string_k_iv (generate_k_iv keys))
      plaintext
    ))

  let enc_revision keys plaintext =
    Enc.(to_string (enc_det
      (to_string_k_enc (generate_k_enc keys))
      (to_string_k_mac (generate_k_mac keys))
      (to_string_k_iv (generate_k_iv keys))
      plaintext
    ))

  let enc_value keys plaintext =
    Enc.(to_string (enc
      (to_string_k_enc (generate_k_enc keys))
      (to_string_k_mac (generate_k_mac keys))
      plaintext
    ))
end
