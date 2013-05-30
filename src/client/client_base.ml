open Lwt
open Cohttp
open Cohttp_lwt_unix
module Cohttp_client = Client

open Primitives
open Messages_j
open Messages_t
open Messages
open Messages.Factory

module Client = struct
  type t = {
    address : string;
    port : int;
    factory : Factory.t;
  }

  let create ?(address=Config.host_address) ?(port=Config.host_port) ?(keys=None) ~username ~password ~servername =
    let keys = match keys with
    | None -> DSA.nigori_new_key ()
    | Some k -> k in
    let manager =
      Derivations.UnassistedKeys.create ~username ~password ~servername in
    let factory = Factory.create keys manager in
    {
      address = address;
      port = port;
      factory = factory;
    }

  let url client endpoint =
    Uri.of_string
      (Printf.sprintf "http://%s:%d/%s"
        client.address
        client.port
        endpoint
      )

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

  let post client message ?(decode=(fun x -> x)) endpoint =
    print_response
      ~decode
      (Cohttp_client.post
        ?body:(Body.body_of_string message)
        (url client endpoint)
      )

  let authenticate client =
    let request =
      encode_auth_request
        (make_auth_request client.factory)
    in
    let message = string_of_authenticate_request request in
    post client message

  let register client =
    let request =
      encode_register_request
        (make_register_request client.factory ())
    in
    let message = string_of_register_request request in
    post client message

  let unregister client =
    let request =
      encode_unregister_request
        (make_unregister_request client.factory)
    in
    let message = string_of_unregister_request request in
    post client message

  let decode_get_indices client message =
    let response = get_indices_response_of_string message in
    let decoded = decode_get_indices_response client.factory response in
    string_of_get_indices_response decoded

  let get_indices client =
    let request = encode_get_indices_request
      (make_get_indices_request client.factory)
    in
    let message = string_of_get_indices_request request in
    post client message

  let decode_get_revisions client message =
    let response = get_revisions_response_of_string message in
    let decoded = decode_get_revisions_response client.factory response in
    string_of_get_revisions_response decoded

  let get_revisions client index =
    let request = encode_get_revisions_request
      (make_get_revisions_request client.factory index)
    in
    let message = string_of_get_revisions_request request in
    post client message

  let put client index rev value =
    let request = encode_put_request
      (make_put_request client.factory index rev value)
    in
    let message = string_of_put_request request in
    post client message

  let delete client key ?(revision=None) =
    let request = encode_delete_request
      (make_delete_request client.factory key ~revision ())
    in
    let message = string_of_delete_request request in
    post client message

  let decode_get client message =
    let response = get_response_of_string message in
    let decoded = decode_get_response client.factory response in
    string_of_get_response decoded

  let get client key ?(revision=None) =
    let request = encode_get_request
      (make_get_request client.factory key ~revision ())
    in
    let message = string_of_get_request request in
    post client message
end
