open Primitives

module P = PBKDF2
module C = Constants

module UnassistedKeys : Derivations_abstract.Keys = struct
  exception InvalidMessageLength
  exception InvalidHmac
  type t = {
    username : string;
    password : string;
    servername : string;
    salt : string;
    kuser : P.t;
    kenc : P.t;
    kmac : P.t;
    kiv : P.t;
  }

  let get_username ~manager = manager.username
  let get_password ~manager = manager.password
  let get_servername ~manager = manager.servername

  let create ~username ~password ~servername =
    let user_and_server = Utils.encode_length [username; servername;] in
    let s_user =
      P.(to_string (apply
        user_and_server
        C.user_salt
        C.n_salt
        C.b_s_user
      )) in
    {
      username = username;
      password = password;
      servername = servername;
      salt = s_user;
      kuser = P.apply password s_user C.n_user C.b_k_dsa;
      kenc = P.apply password s_user C.n_enc C.b_k_enc;
      kmac = P.apply password s_user C.n_mac C.b_k_mac;
      kiv = P.apply password s_user C.n_iv C.b_k_iv;
    }

  let make_packet iv cypher hmac =
    Utils.concat [
      (InitializationVector.to_string iv);
      (AES256.to_string cypher);
      (HMAC.to_string hmac);
    ]

  let xor_hmac input =
    let iv_size = InitializationVector.valid_size in
    let rec split_str str =
      let len = String.length str in
      if len <= iv_size
      then
        let zeroes_len = iv_size - len in
        let zeroes = String.make zeroes_len (char_of_int 0) in
        [str ^ zeroes]
      else
        let piece = String.sub str 0 iv_size in
        let rest = String.sub str iv_size (len - iv_size) in
        piece :: (split_str rest)
    in
    let pieces = split_str input in
    Utils.xor pieces

  let make_iv_from_plaintext ~manager ~plaintext =
    let kiv = P.to_string manager.kiv in
    let hmac = HMAC.apply kiv plaintext in
    let xor_iv = xor_hmac (HMAC.to_string hmac) in
    InitializationVector.create xor_iv

  let encrypt ~manager ~plaintext ?(random_iv=false) () =
    let iv =
      if random_iv
      then
        InitializationVector.create_random ()
      else
        make_iv_from_plaintext ~manager ~plaintext
    in
    let kenc = P.to_string manager.kenc in
    let kmac = P.to_string manager.kmac in
    let cypher = AES256.encrypt iv kenc plaintext in
    let hmac = HMAC.apply kmac (AES256.to_string cypher) in
    make_packet iv cypher hmac

  let decrypt ~manager ~cypher =
    let len = String.length cypher in
    let iv_size = InitializationVector.valid_size in
    let hmac_size = HMAC.valid_size in
    let min_size = iv_size + hmac_size in
    if len <= min_size
    then
      raise InvalidMessageLength
    else
      let kmac = P.to_string manager.kmac in
      let kenc = P.to_string manager.kenc in
      let cypher_size = len - min_size in
      let iv = InitializationVector.create (String.sub cypher 0 iv_size) in
      let data = String.sub cypher iv_size cypher_size in
      let hmac_str = String.sub cypher (iv_size + cypher_size) hmac_size in
      let hmac = HMAC.apply kmac data in
      if String.compare hmac_str (HMAC.to_string hmac) == 0
      then
        AES256.decrypt iv kenc data
      else
        raise InvalidHmac

  let enc_index ~manager ~plaintext =
    encrypt ~manager ~plaintext ()
  let dec_index ~manager ~cypher =
    decrypt ~manager ~cypher

  let enc_revision ~manager ~plaintext =
    encrypt ~manager ~plaintext ()
  let dec_revision ~manager ~cypher =
    decrypt ~manager ~cypher

  let enc_value ~manager ~plaintext =
    encrypt ~manager ~plaintext ~random_iv:true ()
  let dec_value ~manager ~cypher =
    decrypt ~manager ~cypher
end
