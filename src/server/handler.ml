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
    let pub_key = auth_request.auth_request_public_key in
    let signature = auth_request.auth_request_sig in
    let nonce = auth_request.auth_request_nonce in
    let server_name = auth_request.auth_request_server_name in
    let hash = User.make_hash (Utils.hash_key pub_key) in
    let some_user = Database.get_user db hash in
    match some_user with
    | None -> Generic.unauthorized ()
    | Some user -> fn user
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
  Generic.not_found ()
end

let get_indices h = begin
  Generic.not_found ()
end

let get_revisions h = begin
  let db = h.database in
  lwt body = Body.string_of_body h.body in
  let request = get_revisions_request_of_string body in
  let auth_request = request.get_revisions_request_auth in
  (* TODO *)
  Validation.authenticate_user db auth_request (fun user ->begin
    let key = request.get_revisions_request_key in
    let some_revs = Database.get_revisions db user key in
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
  let db = h.database in
  lwt body = Body.string_of_body h.body in
  let request = put_request_of_string body in
  let auth_request = request.put_request_auth in
  Validation.authenticate_user db auth_request (fun user -> begin
    let key = request.put_request_key in
    let revision = request.put_request_revision in
    let value = request.put_request_value in
    let ret = Database.put_record db user key revision value in
    if ret
    then Generic.ok ()
    (* TODO *)
    else Generic.internal ()
  end)
end

let delete h = begin
  Generic.not_found ()
end

let update h = begin
  Generic.not_found ()
end

let authenticate h = begin
  lwt body = Body.string_of_body h.body in
  let f () = Server.respond_string ~status:`OK ~body:body () in
  Validation.is_post h.request f
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
  let db = h.database in
  lwt body = Body.string_of_body h.body in
  let request = unregister_request_of_string body in
  let auth_request = request.unregister_request_auth in
  Validation.authenticate_user db auth_request (fun user -> begin
    let ret = Database.delete_user h.database user in
    if ret
    then
      Generic.ok ~msg:"User unregistered" ()
    else
      Generic.internal ~msg:"User could not be unregistered" ()
  end)
end
