open Lwt
open Cohttp
open Cohttp_lwt_unix

open Primitives
open Messages_j
open Messages_t
open Messages

let address = Config.host_address
let port = Config.host_port

let url endpoint = 
  Uri.of_string (Printf.sprintf "http://%s:%d/%s" address port endpoint)

let print_response t = match_lwt t with
  | None -> 
      Printf.printf "Got no response back!\n"; exit 1
  | Some x -> 
      (* let response = fst x in *)
      let body = snd x in
      lwt body = Body.string_of_body body in
      Printf.printf "Got response back: %s\n" body;
      exit 0

let post message endpoint = 
  Printf.printf "Posting message %s\nPointing at endpoint %s\n" message endpoint;
  print_response (Client.post ?body:(Body.body_of_string message) (url endpoint))

let keys = DSA.nigori_new_key ()
let pub_key, priv_key = keys
let server_name = "server_name"
let index = "index"

let authenticate () =
  let request = 
    encode_auth_request 
      (make_auth_request keys server_name) 
  in
  let message = string_of_authenticate_request request in
  post message

let register () =
  let request = 
    encode_register_request 
      (make_register_request pub_key ()) 
  in
  let message = string_of_register_request request in
  post message

let unregister () =
  let request = 
    encode_unregister_request 
      (make_unregister_request keys server_name)
  in
  let message = string_of_unregister_request request in
  post message

let get_revisions () =
  let request = encode_get_revisions_request
    (make_get_revisions_request keys server_name index)
  in
  let message = string_of_get_revisions_request request in
  post message

let fake_put rev = 
  let request = encode_put_request
    (make_put_request keys server_name index rev (rev ^ "_value"))
  in
  let message = string_of_put_request request in
  post message

let put1 () = 
  fake_put "rev1"

let put2 () =
  fake_put "rev2"

let choice () = 
  if Array.length Sys.argv <= 1
  then begin
    Printf.printf "Must supply an argument\n";
    exit 1
  end
  else begin
    let endpoint = Sys.argv.(1) in
    match endpoint with
    | "authenticate" -> (authenticate ()) endpoint
    | "register" -> (register ()) endpoint
    | "unregister" -> (unregister ()) endpoint
    | "get_revisions" -> (get_revisions ()) "get-revisions"
    | "put1" -> (put1 ()) "put"
    | "put2" -> (put2 ()) "put"
    | x -> begin
      Printf.printf "Invalid command %s\n" x;
      exit 1
    end
  end

let main () = Lwt_unix.run(choice ())
