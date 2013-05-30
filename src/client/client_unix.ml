open Lwt
(*
open Cohttp
open Cohttp_lwt_unix
*)
module CL = Client_base.Client

let choice () =
  let address = Config.host_address in
  let port = Config.host_port in
  let username = "username" in
  let password = "password" in
  let servername = "servername" in
  let keys = None in

  let client = CL.create ~address ~port ~keys ~username ~password ~servername in

  if Array.length Sys.argv <= 1
  then begin
    Printf.printf "Must supply an argument\n";
    exit 1
  end
  else begin
    let endpoint = Sys.argv.(1) in
    match endpoint with
    | "authenticate" -> begin
      CL.authenticate client endpoint
    end
    | "register" -> begin
      CL.register client endpoint
    end
    | "unregister" -> begin
      CL.unregister client endpoint
    end
    | "get-indices" -> begin
      CL.get_indices
        client
        ~decode:(CL.decode_get_indices client)
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
        CL.get_revisions
          client
          index
          ~decode:(CL.decode_get_revisions client)
          endpoint
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
        CL.delete client key ~revision endpoint
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
        CL.get
          client
          key
          ~revision
          ~decode:(CL.decode_get client)
          endpoint
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
        CL.put client index rev value endpoint
      end
    end
    | x -> begin
      Printf.printf "Invalid command %s\n" x;
      exit 1
    end
  end

let main () = Lwt_unix.run(choice ())
