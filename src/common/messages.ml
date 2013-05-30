open Cohttp

open Primitives
open Messages_j
open Messages_t
module D = Derivations.UnassistedKeys

let request_get = "get"
let request_get_indices = "get-indices"
let request_get_revisions = "get-revisions"
let request_put = "put"
let request_delete = "delete"
let request_update = "update"
let request_authenticate = "authenticate"
let request_register = "register"
let request_unregister = "unregister"

let to_bytes str = str

let enc str = Base64.encode str
let enc_list fn lst =
  let f l k = (fn k) :: l in
  List.rev (List.fold_left f [] lst)

let dec str = Base64.decode str
let dec_list fn lst =
  let f l k = (fn k) :: l in
  List.rev (List.fold_left f [] lst)

module Factory = struct
  type t = {
    keys : DSA.key * DSA.key;
    manager : D.t;
  }

  let create keys manager =
    {
      keys = keys;
      manager = manager;
    }

  let to_sign_raw nonce servername command payload =
    let payload_str = Utils.encode_length payload in
    let nt = Nonce.get_nt nonce in
    let nr = Nonce.get_nr nonce in
    Utils.encode_length [to_bytes servername; nt; nr; to_bytes command; payload_str;]

  let to_sign_request auth_request command payload =
    let nonce = Nonce.from_string auth_request.auth_request_nonce in
    let servername = auth_request.auth_request_servername in
    to_sign_raw nonce servername command payload


  let make_auth_request_internal factory command payload =
    let keys = factory.keys in
    let servername = D.get_servername factory.manager in
    let pub_key = fst keys in
    let priv_key = snd keys in
    let nonce = Nonce.create_random () in
    let message = to_sign_raw nonce servername command payload in
    let r, s = DSA.sign message priv_key in
    {
      auth_request_public_key = DSA.hash_key pub_key;
      auth_request_sig = Utils.encode_length [r;s;];
      auth_request_nonce = Nonce.to_string nonce;
      auth_request_servername = servername;
    }

  (* AUTHENTICATE REQUEST *)
  let payload_auth_request () = []

  let to_sign_auth_request request =
    to_sign_request
      request
      request_authenticate
      (payload_auth_request ())

  let make_auth_request factory =
    let payload = payload_auth_request () in
    make_auth_request_internal factory request_authenticate payload

  let encode_auth_request request =
    {
      auth_request_public_key = enc request.auth_request_public_key;
      auth_request_sig = enc request.auth_request_sig;
      auth_request_nonce = enc request.auth_request_nonce;
      auth_request_servername = request.auth_request_servername;
    }

  let decode_auth_request request =
    {
      auth_request_public_key = dec request.auth_request_public_key;
      auth_request_sig = dec request.auth_request_sig;
      auth_request_nonce = dec request.auth_request_nonce;
      auth_request_servername = request.auth_request_servername;
    }

  (* REVISION VALUE *)
  let make_revision_value revision value =
    {
      revval_revision = revision;
      revval_value = value;
    }

  let encode_revision_value rv =
    {
      revval_revision = enc rv.revval_revision;
      revval_value = enc rv.revval_value;
    }

  let decode_revision_value factory rv =
    let manager = factory.manager in
    {
      revval_revision = D.dec_revision ~manager ~cypher:(dec rv.revval_revision);
      revval_value = D.dec_value ~manager ~cypher:(dec rv.revval_value);
    }

  (* GET REQUEST *)
  let payload_get_request key ?(revision=None) () =
    match revision with
    | None -> [key;]
    | Some rev -> [key; rev;]

  let to_sign_get_request request =
    let auth = request.get_request_auth in
    to_sign_request
      auth
      request_get
      (payload_get_request
        request.get_request_key
        ~revision:request.get_request_revision ())

  let make_get_request factory key ?(revision=None) () =
    let manager = factory.manager in
    let key = D.enc_index ~manager ~plaintext:key in
    let revision = match revision with
    | None -> None
    | Some rev -> Some (D.enc_revision ~manager ~plaintext:rev) in
    let payload = payload_get_request key ~revision () in
    {
      get_request_auth = make_auth_request_internal factory request_get payload;
      get_request_key = key;
      get_request_revision = revision;
    }

  let encode_get_request request =
    let rev =
      match request.get_request_revision with
      | None -> None
      | Some r -> Some (enc r)
    in
    {
      get_request_auth = encode_auth_request request.get_request_auth;
      get_request_key = enc request.get_request_key;
      get_request_revision = rev;
    }

  let decode_get_request request =
    let rev =
      match request.get_request_revision with
      | None -> None
      | Some r -> Some (dec r)
    in
    {
      get_request_auth = decode_auth_request request.get_request_auth;
      get_request_key = dec request.get_request_key;
      get_request_revision = rev;
    }

  (* GET RESPONSE *)
  let make_get_response revisions ?(key=None) () =
    {
      get_response_revisions = revisions;
      get_response_key = key;
    }

  let encode_get_response response =
    let revs = enc_list encode_revision_value response.get_response_revisions in
    let key =
      match response.get_response_key with
      | None -> None
      | Some k -> Some (enc k)
    in
    {
      get_response_revisions = revs;
      get_response_key = key;
    }

  let decode_get_response factory response =
    let revs = dec_list (decode_revision_value factory) response.get_response_revisions in
    let key =
      match response.get_response_key with
      | None -> None
      | Some k -> Some (D.dec_index ~manager:factory.manager ~cypher:(dec k))
    in
    {
      get_response_revisions = revs;
      get_response_key = key;
    }

  (* GET INDICES REQUEST *)
  let payload_get_indices_request () = []

  let to_sign_get_indices_request request =
    let auth = request.get_indices_request_auth in
    to_sign_request
      auth
      request_get_indices
      (payload_get_indices_request ())

  let make_get_indices_request factory =
    let payload = payload_get_indices_request () in
    {
      get_indices_request_auth =
        make_auth_request_internal factory request_get_indices payload;
    }

  let encode_get_indices_request request =
    {
      get_indices_request_auth = encode_auth_request request.get_indices_request_auth;
    }

  let decode_get_indices_request request =
    {
      get_indices_request_auth = decode_auth_request request.get_indices_request_auth;
    }

  (* GET INDICES RESPONSE *)
  let make_get_indices_response indices =
    {
      get_indices_response_indices = indices;
    }

  let encode_get_indices_response response =
    {
      get_indices_response_indices = enc_list enc response.get_indices_response_indices;
    }

  let decode_get_indices_response factory response =
    let unfold = fun x -> begin
      D.dec_index ~manager:factory.manager ~cypher:(dec x)
    end in
    {
      get_indices_response_indices = dec_list unfold response.get_indices_response_indices;
    }

  (* GET REVISIONS REQUEST *)
  let payload_get_revisions_request key () = [key;]

  let to_sign_get_revisions_request request =
    let auth = request.get_revisions_request_auth in
    to_sign_request
      auth
      request_get_revisions
      (payload_get_revisions_request
        request.get_revisions_request_key ())

  let make_get_revisions_request factory key =
    let key = D.enc_index ~manager:factory.manager ~plaintext:key in
    let payload = payload_get_revisions_request key () in
    {
      get_revisions_request_auth =
        make_auth_request_internal factory request_get_revisions payload;
      get_revisions_request_key = key;
    }

  let encode_get_revisions_request request =
    {
      get_revisions_request_auth = encode_auth_request request.get_revisions_request_auth;
      get_revisions_request_key = enc request.get_revisions_request_key;
    }

  let decode_get_revisions_request request =
    {
      get_revisions_request_auth = decode_auth_request request.get_revisions_request_auth;
      get_revisions_request_key = dec request.get_revisions_request_key;
    }

  (* GET REVISIONS RESPONSE *)
  let make_get_revisions_response revisions ?(key=None) () =
      {
        get_revisions_response_revisions = revisions;
        get_revisions_response_key = key;
      }

  let encode_get_revisions_response response =
    let revs = enc_list enc response.get_revisions_response_revisions in
    let key =
      match response.get_revisions_response_key with
      | None -> None
      | Some k -> Some (enc k)
    in
    {
      get_revisions_response_revisions = revs;
      get_revisions_response_key = key;
    }

  let decode_get_revisions_response factory response =
    let manager = factory.manager in
    let unfold = fun x -> begin
      D.dec_revision ~manager ~cypher:(dec x)
    end in
    let revs = dec_list unfold response.get_revisions_response_revisions in
    let key =
      match response.get_revisions_response_key with
      | None -> None
      | Some k -> Some (D.dec_index ~manager ~cypher:(dec k))
    in
    {
      get_revisions_response_revisions = revs;
      get_revisions_response_key = key;
    }

  (* PUT REQUEST *)
  let payload_put_request key revision value () = [key; revision; value;]

  let to_sign_put_request request =
    let auth = request.put_request_auth in
    to_sign_request
      auth
      request_put
      (payload_put_request
        request.put_request_key
        request.put_request_revision
        request.put_request_value ())

  let make_put_request factory key revision value =
    let manager = factory.manager in
    let key = D.enc_index ~manager ~plaintext:key in
    let revision = D.enc_revision ~manager ~plaintext:revision in
    let value = D.enc_value ~manager ~plaintext:value in
    let payload = payload_put_request key revision value () in
    {
      put_request_auth =
        make_auth_request_internal factory request_put payload;
      put_request_key = key;
      put_request_revision = revision;
      put_request_value = value;
    }

  let encode_put_request request =
    {
      put_request_auth = encode_auth_request request.put_request_auth;
      put_request_key = enc request.put_request_key;
      put_request_revision = enc request.put_request_revision;
      put_request_value = enc request.put_request_value;
    }

  let decode_put_request request =
    {
      put_request_auth = decode_auth_request request.put_request_auth;
      put_request_key = dec request.put_request_key;
      put_request_revision = dec request.put_request_revision;
      put_request_value = dec request.put_request_value;
    }

  (* DELETE REQUEST *)
  let payload_delete_request key ?(revision=None) () =
    match revision with
    | None -> [key;]
    | Some rev -> [key; rev;]

  let to_sign_delete_request request =
    let auth = request.delete_request_auth in
    to_sign_request
      auth
      request_delete
      (payload_delete_request
        request.delete_request_key
        ~revision:request.delete_request_revision ())

  let make_delete_request factory key ?(revision=None) () =
    let manager = factory.manager in
    let key = D.enc_index ~manager ~plaintext:key in
    let revision = match revision with
    | None -> None
    | Some rev -> Some (D.enc_revision ~manager ~plaintext:rev) in
    let payload = payload_delete_request key ~revision () in
    {
      delete_request_auth =
        make_auth_request_internal factory request_delete payload;
      delete_request_key = key;
      delete_request_revision = revision;
    }

  let encode_delete_request request =
    let rev =
      match request.delete_request_revision with
      | None -> None
      | Some r -> Some (enc r)
    in
    {
      delete_request_auth = encode_auth_request request.delete_request_auth;
      delete_request_key = enc request.delete_request_key;
      delete_request_revision = rev;
    }

  let decode_delete_request request =
    let rev =
      match request.delete_request_revision with
      | None -> None
      | Some r -> Some (dec r)
    in
    {
      delete_request_auth = decode_auth_request request.delete_request_auth;
      delete_request_key = dec request.delete_request_key;
      delete_request_revision = rev;
    }

  (* REGISTER REQUEST *)
  let make_register_request factory ?(token="") () =
    let pub_key = fst factory.keys in
    {
      register_request_public_key = DSA.serialize_key pub_key;
      register_request_token = token;
    }

  let encode_register_request request =
    {
      register_request_public_key = enc request.register_request_public_key;
      register_request_token = enc request.register_request_token;
    }

  let decode_register_request request =
    {
      register_request_public_key = dec request.register_request_public_key;
      register_request_token = dec request.register_request_token;
    }

  (* UNREGISTER REQUEST *)
  let payload_unregister_request () = []

  let to_sign_unregister_request request =
    let auth = request.unregister_request_auth in
    to_sign_request
      auth
      request_unregister
      (payload_unregister_request ())

  let make_unregister_request factory =
    let payload = payload_unregister_request () in
    {
      unregister_request_auth =
        make_auth_request_internal factory request_unregister payload;
    }

  let encode_unregister_request request =
    {
      unregister_request_auth = encode_auth_request request.unregister_request_auth;
    }

  let decode_unregister_request request =
    {
      unregister_request_auth = decode_auth_request request.unregister_request_auth;
    }
end
