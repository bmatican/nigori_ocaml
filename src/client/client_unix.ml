open Lwt
open Cohttp_lwt_unix

open Messages_j
open Messages_t

let address = "127.0.0.1"
let port = 8081
let url = Uri.of_string (Printf.sprintf "http://%s:%d/post" address port)

let print_response t = match_lwt t with
  | None -> 
      Printf.printf "Got no response back!\n"; exit 1
  | Some x -> 
      let response = fst x in
      let body = snd x in
      lwt body = Body.string_of_body body in
      let rev_val = revision_value_of_string body in
      Printf.printf "Got response back: %s\n" (rev_val.revval_revision);
      return ()

let post () =
  let rev_val = {
    revval_revision = "revisioooooon";
    revval_value = "valuuuuuue";
  } in
  let message = string_of_revision_value rev_val in
  print_response (Client.post ?body:(Body.body_of_string message) url)

let main () = Lwt_unix.run(post ())
