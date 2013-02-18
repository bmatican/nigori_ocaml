open Lwt
open Cohttp
open Cohttp_lwt_unix
open Messages_t
open Messages_j

type t = {
  database : Database.t;
  request : Request.t;
  body : Body.t;
}
type out = (Response.t * Body.t) Lwt.t

module Generic = struct
  let respond ?msg status = 
    match msg with
    | None -> Server.respond_string ~status ~body:(Code.string_of_status status) ()
    | Some m -> Server.respond_string ~status ~body:m ()

  let method_not_allowed ?msg () = 
    respond ?msg `Method_not_allowed

  let not_found ?msg () = 
    respond ?msg`Not_found

  let unauthorized ?msg () = 
    respond ?msg `Unauthorized

  let internal ?msg () =
    respond ?msg `Internal_server_error

  let ok ?msg () =
    respond ?msg `OK
end

module Validation = struct
  let is_post req fn =
    if Request.meth req != `POST
    then
      Generic.method_not_allowed ()
    else
      fn ()

  let authenticate_user db auth_request fn = 
    let signature = auth_request.auth_request_sig in
    try
      let signature = Cohttp.Base64.decode signature in
      Printf.eprintf "Got signature %s\n" signature;
      let split_signature = Utils.decode_length signature in
      if List.length split_signature != 2
      then begin
        let msg = "Invalid signature composition" in
        Generic.unauthorized ~msg ()
      end
      else begin
        let key_hash = auth_request.auth_request_public_key in
        (* apparently this is the hash... *)
        let hash = User.make_hash key_hash in
        let some_user = Database.get_user db hash in
        match some_user with
        | None -> begin
          let msg = "No such user" in
          Generic.unauthorized ~msg () 
        end
        | Some user -> begin
          let pub_key = User.get_key user in
          let nonce_data = Cohttp.Base64.decode (auth_request.auth_request_nonce) in
          let nonce = Nonce.from_string (nonce_data) in
          let ret = Database.check_and_add_nonce db nonce pub_key in
          if not ret
          then begin
            let msg = "Invalid nonce" in
            Generic.unauthorized ~msg ()
          end
          else begin
            let nt = Nonce.get_nt nonce in
            let nr = Nonce.get_nr nonce in
            let dsa_r = List.nth split_signature 0 in
            let dsa_s = List.nth split_signature 1 in
            let server_name = auth_request.auth_request_server_name in
            (* TODO *)
            fn user
          end
        end
      end
    with
    | Utils.InvalidEncodingLength -> begin
      let msg = "Invalid signature encoding" in
      Generic.unauthorized ~msg ()
    end
end

let default h = begin
  Printf.eprintf "Defaulted here do to path=%s\n" (Request.path h.request);
  Generic.not_found ()
end

(* Nigori specific handling. *)
let create db req body = begin
  {
    database = db;
    request = req;
    body = body;
  }
end

let get h = begin
  lwt body = Body.string_of_body h.body in
  let request = get_request_of_string body in
  let auth_request = request.get_request_auth in
  Validation.authenticate_user h.database auth_request (fun user -> begin
    let key = request.get_request_key in
    let revision = request.get_request_revision in
    let some_revisions = Database.get_record h.database user key ~revision () in
    match some_revisions with
    | None -> Generic.not_found ()
    | Some revisions -> begin
      let response = {
        get_response_revisions = revisions;
        get_response_key = Some (key);
      } in
      let msg = string_of_get_response response in
      Generic.ok ~msg ()
    end
  end)
end

let get_indices h = begin
  lwt body = Body.string_of_body h.body in
  let request = get_indices_request_of_string body in
  let auth_request = request.get_indices_request_auth in
  Validation.authenticate_user h.database auth_request (fun user -> begin
    let some_indices = Database.get_indices h.database user in
    match some_indices with
    | None -> Generic.not_found ()
    | Some indices -> begin
      let response = {
        get_indices_response_indices = indices;
      } in
      let msg = string_of_get_indices_response response in
      Generic.ok ~msg ()
    end
  end)
end

let get_revisions h = begin
  lwt body = Body.string_of_body h.body in
  let request = get_revisions_request_of_string body in
  let auth_request = request.get_revisions_request_auth in
  Validation.authenticate_user h.database auth_request (fun user ->begin
    let key = request.get_revisions_request_key in
    let some_revs = Database.get_revisions h.database user key in
    match some_revs with
    | None -> Generic.internal ()
    | Some revs -> begin
      let response = {
        get_revisions_response_revisions = revs;
        get_revisions_response_key = Some (key);
      } in
      let msg = string_of_get_revisions_response response in
      Generic.ok ~msg ()
    end
  end)
end

let put h = begin
  lwt body = Body.string_of_body h.body in
  let request = put_request_of_string body in
  let auth_request = request.put_request_auth in
  Validation.authenticate_user h.database auth_request (fun user -> begin
    let key = request.put_request_key in
    let revision = request.put_request_revision in
    let value = request.put_request_value in
    let ret = Database.put_record h.database user key revision value in
    if ret
    then Generic.ok ()
    else Generic.internal ()
  end)
end

let delete h = begin
  lwt body = Body.string_of_body h.body in
  let request = delete_request_of_string body in
  let auth_request = request.delete_request_auth in
  Validation.authenticate_user h.database auth_request (fun user -> begin
    let key = request.delete_request_key in
    let revision = request.delete_request_revision in
    let ret = Database.delete_record h.database user key ~revision () in
    if ret
    then Generic.ok ()
    else Generic.not_found ()
  end)
end

let update h = begin
  Generic.not_found ()
end

let authenticate h = begin
  Validation.is_post h.request (fun () -> begin
    lwt body = Body.string_of_body h.body in
    Server.respond_string ~status:`OK ~body:body ()
  end)
end

let register h = begin
  lwt body = Body.string_of_body h.body in
  let request = register_request_of_string body in
  let pub_key = request.register_request_public_key in
  let hash = Utils.hash_key pub_key in
  let ret = Database.add_user 
      h.database 
      (User.make_key pub_key) 
      (User.make_hash hash)
  in
  if ret
  then
    Server.respond_string ~status:`OK ~body:"User registered" ()
  else
    Server.respond_error ~status:`Conflict ~body:"User is already registered" ()
end

let unregister h = begin
  lwt body = Body.string_of_body h.body in
  let request = unregister_request_of_string body in
  let auth_request = request.unregister_request_auth in
  Validation.authenticate_user h.database auth_request (fun user -> begin
    let ret = Database.delete_user h.database user in
    if ret
    then
      Generic.ok ~msg:"User unregistered" ()
    else
      Generic.internal ~msg:"User could not be unregistered" ()
  end)
end
