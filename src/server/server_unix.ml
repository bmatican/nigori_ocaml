open Lwt
open Cohttp
open Cohttp_lwt_unix

open Messages

module CustomHandler = Handler.Make (Hash_database.DB)

let address = Config.host_address
let port = Config.host_port
let db = CustomHandler.init_db ()

let make_server () =
  let callback conn_id ?body req =
    let endpoint = Request.path req in
    let request_str = String.sub endpoint 1 ((String.length endpoint) - 1) in
    Printf.eprintf "Hit endpoint: %s\n" endpoint;
    let handler = CustomHandler.create db req body in
    (* TODO: understand why OCaml does not do match with variables... *)
    if request_str = request_get then CustomHandler.get handler
    else if request_str = request_get_indices then CustomHandler.get_indices handler
    else if request_str = request_get_revisions then CustomHandler.get_revisions handler
    else if request_str = request_put then CustomHandler.put handler
    else if request_str = request_delete then CustomHandler.delete handler
    else if request_str = request_update then CustomHandler.update handler
    else if request_str = request_authenticate then CustomHandler.authenticate handler
    else if request_str = request_register then CustomHandler.register handler
    else if request_str = request_unregister then CustomHandler.unregister handler
    else CustomHandler.default handler
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let config = { Server.callback; conn_closed } in
  server ~address ~port config
    
let main () = Lwt_unix.run (make_server ()) 
