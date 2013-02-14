open Lwt
open Cohttp_lwt_unix

open Messages_j
open Messages_t

let address = "127.0.0.1"
let port = 8081
let url = Uri.of_string (Printf.sprintf "http://%s:%d/authenticate" address port)

let print_response t = match_lwt t with
  | None -> 
      Printf.printf "Got no response back!\n"; exit 1
  | Some x -> 
      (* let response = fst x in *)
      let body = snd x in
      lwt body = Body.string_of_body body in
      let auth = authenticate_request_of_string body in
      Printf.printf "Got response back: %s\n" (auth.auth_request_public_key);
      return ()

let post () =
  let auth = {
    auth_request_public_key = "public_key";
    auth_request_sig = "sig";
    auth_request_nonce = "nonce";
    auth_request_server_name = "server_name";
  } in
  let message = string_of_authenticate_request auth in
  print_response (Client.post ?body:(Body.body_of_string message) url)

let main () = Lwt_unix.run(post ())
