open Lwt
open Cohttp_lwt_unix

open Messages_j
open Messages_t

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
  print_response (Client.post ?body:(Body.body_of_string message) (url endpoint))

let pub_key = "public_key"

let make_auth_request () = 
  {
    auth_request_public_key = pub_key;
    auth_request_sig = "sig";
    auth_request_nonce = "nonce";
    auth_request_server_name = "server_name";
  }

let authenticate () =
  let request = make_auth_request () in
  let message = string_of_authenticate_request request in
  post message

let register () =
  let request = {
    register_request_public_key = pub_key;
    register_request_token = "some_token";
  } in
  let message = string_of_register_request request in
  post message

let unregister () =
  let auth_request = make_auth_request () in
  let request = {
    unregister_request_auth = auth_request;
  } in
  let message = string_of_unregister_request request in
  post message

let get_revisions () =
  let auth_request = make_auth_request () in
  let request = {
    get_revisions_request_auth = auth_request;
    get_revisions_request_key = "key1";
  } in
  let message = string_of_get_revisions_request request in
  post message

let fake_put rev = 
  let auth_request = make_auth_request () in
  let request = {
    put_request_auth = auth_request;
    put_request_key = "key1";
    put_request_revision = rev;
    put_request_value = "value";
  } in
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
