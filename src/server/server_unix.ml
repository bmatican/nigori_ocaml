open Lwt
open Cohttp
open Cohttp_lwt_unix

let address = "127.0.0.1"
let port = 8081

let method_not_allowed () = 
  let status = `Method_not_allowed in
  Server.respond_error ~status ~body:(Code.string_of_status status) ()

let not_found () = 
  let status = `Not_found in
  Server.respond_error ~status ~body:(Code.string_of_status status) ()

let validate_post req fn =
  if Request.meth req != `POST
  then
    method_not_allowed ()
  else
    fn ()

let make_server () =
  let callback conn_id ?body req =
    lwt lwt_body = Body.string_of_body body in
    match Request.path req with
      | "/get" -> not_found ()
      | "/get-indices" -> not_found ()
      | "/get-revisions" -> not_found ()
      | "/put" -> not_found ()
      | "/delete" -> not_found ()
      | "/update" -> not_found ()
      | "/authenticate" -> begin
        let f () = Server.respond_string ~status:`OK ~body:lwt_body () in
        validate_post req f
      end
      | "/register" -> not_found ()
      | "/unregister" -> not_found ()
      | _ -> not_found ()
    (*
    let endpoint = Endpoint.from (Request.path req) in
    Printf.eprintf "Got endpoint: %s\n" (Request.path req);
    match endpoint with
    | None -> not_found ()
    | Some path -> begin match path with
      | `get -> not_found()
      | `get_indices -> not_found ()
      | `get_revisions -> not_found ()
      | `put -> not_found ()
      | `delete -> not_found ()
      | `update -> not_found ()
      | `authenticate -> begin
          let f () = Server.respond_string ~status:`OK ~body:lwt_body () in
          validate_post req f
      end
      | `register -> not_found ()
      | `unregister -> not_found ()
      | _ -> not_found ()
    end
    *)
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let config = { Server.callback; conn_closed } in
  server ~address ~port config
    
let main () = Lwt_unix.run (make_server ()) 
