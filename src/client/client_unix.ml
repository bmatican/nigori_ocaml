open Lwt
open Cohttp
open Cohttp_lwt_unix

open Primitives
open Messages_j
open Messages_t
open Messages
open Messages.Factory

let address = Config.host_address
let port = Config.host_port

let url endpoint =
  Uri.of_string (Printf.sprintf "http://%s:%d/%s" address port endpoint)

let print_response ~decode t = match_lwt t with
  | None ->
      Printf.printf "Got no response back!\n"; exit 1
  | Some x ->
      let response = fst x in
      let body = snd x in
      lwt body = Body.string_of_body body in
      let printing =
        if Response.status response == `OK
        then decode body
        else body
      in
      Printf.printf "Got response back: %s\n" printing;
      exit 0

let post message ?(decode=(fun x -> x)) endpoint =
  print_response ~decode (Client.post ?body:(Body.body_of_string message) (url endpoint))

let keys = DSA.nigori_new_key ()
let username = "username"
let password = "password"
let servername = "servername"
let manager = Derivations.UnassistedKeys.create ~username ~password ~servername
let factory = Factory.create keys manager

let authenticate =
  let request =
    encode_auth_request
      (make_auth_request factory)
  in
  let message = string_of_authenticate_request request in
  post message

let register =
  let request =
    encode_register_request
      (make_register_request factory ())
  in
  let message = string_of_register_request request in
  post message

let unregister =
  let request =
    encode_unregister_request
      (make_unregister_request factory)
  in
  let message = string_of_unregister_request request in
  post message

let decode_get_indices message =
  let response = get_indices_response_of_string message in
  let decoded = decode_get_indices_response factory response in
  string_of_get_indices_response decoded

let get_indices =
  let request = encode_get_indices_request
    (make_get_indices_request factory)
  in
  let message = string_of_get_indices_request request in
  post message

let decode_get_revisions message =
  let response = get_revisions_response_of_string message in
  let decoded = decode_get_revisions_response factory response in
  string_of_get_revisions_response decoded

let get_revisions index =
  let request = encode_get_revisions_request
    (make_get_revisions_request factory index)
  in
  let message = string_of_get_revisions_request request in
  post message

let put index rev value =
  let request = encode_put_request
    (make_put_request factory index rev value)
  in
  let message = string_of_put_request request in
  post message

let delete key ?(revision=None) =
  let request = encode_delete_request
    (make_delete_request factory key ~revision ())
  in
  let message = string_of_delete_request request in
  post message

let decode_get message =
  let response = get_response_of_string message in
  let decoded = decode_get_response factory response in
  string_of_get_response decoded

let get key ?(revision=None) =
  let request = encode_get_request
    (make_get_request factory key ~revision ())
  in
  let message = string_of_get_request request in
  post message

let choice () =
  if Array.length Sys.argv <= 1
  then begin
    Printf.printf "Must supply an argument\n";
    exit 1
  end
  else begin
    let endpoint = Sys.argv.(1) in
    match endpoint with
    | "authenticate" -> begin
      authenticate endpoint
    end
    | "register" -> begin
      register endpoint
    end
    | "unregister" -> begin
      unregister endpoint
    end
    | "get-indices" -> begin
      get_indices
        ~decode:decode_get_indices
        endpoint
    end
    | "get-revisions" -> begin
      if Array.length Sys.argv != 3
      then begin
        Printf.eprintf "usage: %s index\n" endpoint;
        exit 1
      end
      else begin
        let index = Sys.argv.(2) in
        get_revisions index ~decode:decode_get_revisions endpoint
      end
    end
    | "delete" -> begin
      if Array.length Sys.argv < 3
      then begin
        Printf.eprintf "usage: %s key [rev]\n" endpoint;
        exit 1
      end
      else begin
        let key = Sys.argv.(2) in
        let revision =
          if Array.length Sys.argv == 3
          then None
          else Some (Sys.argv.(3))
        in
        delete key ~revision endpoint
      end
    end
    | "get" -> begin
      if Array.length Sys.argv < 3
      then begin
        Printf.eprintf "usage: %s key [rev]\n" endpoint;
        exit 1
      end
      else begin
        let key = Sys.argv.(2) in
        let revision =
          if Array.length Sys.argv == 3
          then None
          else Some (Sys.argv.(3))
        in
        get key ~revision ~decode:decode_get endpoint
      end
    end
    | "put" -> begin
      if Array.length Sys.argv != 5
      then begin
        Printf.eprintf "usage: %s index rev value\n" endpoint;
        exit 1
      end
      else begin
        let index = Sys.argv.(2) in
        let rev = Sys.argv.(3) in
        let value = Sys.argv.(4) in
        put index rev value endpoint
      end
    end
    | x -> begin
      Printf.printf "Invalid command %s\n" x;
      exit 1
    end
  end

let main () = Lwt_unix.run(choice ())
