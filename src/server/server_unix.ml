open Lwt
open Cohttp
open Cohttp_lwt_unix

open Messages

let address = Config.host_address
let port = Config.host_port
let db = Database.create ()

let make_server () =
  let callback conn_id ?body req =
    let endpoint = Request.path req in
    let request_str = String.sub endpoint 1 ((String.length endpoint) - 1) in
    Printf.eprintf "Hit endpoint: %s\n" endpoint;
    Printf.eprintf "Sizes %d %d\n" (String.length endpoint) (String.length
    request_str);
    let handler = Handler.create db req body in
    (* TODO: understand why OCaml does not do match with variables... *)
    if request_str = request_get then Handler.get handler
    else if request_str = request_get_indices then Handler.get_indices handler
    else if request_str = request_get_revisions then Handler.get_revisions handler
    else if request_str = request_put then Handler.put handler
    else if request_str = request_delete then Handler.delete handler
    else if request_str = request_update then Handler.update handler
    else if request_str = request_authenticate then Handler.authenticate handler
    else if request_str = request_register then Handler.register handler
    else if request_str = request_unregister then Handler.unregister handler
    else Handler.default handler
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let config = { Server.callback; conn_closed } in
  server ~address ~port config
    
let main () = Lwt_unix.run (make_server ()) 
