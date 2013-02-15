open Lwt
open Cohttp
open Cohttp_lwt_unix

let address = Config.host_address
let port = Config.host_port
let db = Database.create ()

let make_server () =
  let callback conn_id ?body req =
    let endpoint = Request.path req in
    Printf.eprintf "Hit endpoint: %s\n" endpoint;
    let handler = Handler.create db req body in
    match endpoint with
      | "/get" -> Handler.get handler
      | "/get-indices" -> Handler.get_indices handler
      | "/get-revisions" -> Handler.get_revisions handler
      | "/put" -> Handler.put handler
      | "/delete" -> Handler.delete handler
      | "/update" -> Handler.update handler
      | "/authenticate" -> Handler.authenticate handler
      | "/register" -> Handler.register handler
      | "/unregister" -> Handler.unregister handler
      | _ -> Handler.default handler
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let config = { Server.callback; conn_closed } in
  server ~address ~port config
    
let main () = Lwt_unix.run (make_server ()) 
