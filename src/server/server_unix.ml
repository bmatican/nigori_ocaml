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
    | "" | "/" -> 
      Server.respond_string ~status:`OK ~body:"helloworld" ()
    | "/post" -> 
        Printf.eprintf "Got a POST request\n";
        let f () = Server.respond_string ~status:`OK ~body:lwt_body () in
        validate_post req f
    | _ -> not_found ()
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let config = { Server.callback; conn_closed } in
  server ~address ~port config
    
let main () = Lwt_unix.run (make_server ()) 
