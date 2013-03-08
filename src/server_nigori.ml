let usage () =
  Printf.printf "Allowed parameters: [--sql/--hash]\n";
  exit 1

let run fn =
  Printf.printf "Running server main\n";
  fn ()

module DefaultServer = Server_unix.Make(Hash_database.DB)
module SqlServer = Server_unix.Make(Sql_database.DB)

let _ =
  let l = Array.length Sys.argv in
  if l > 2
  then begin
    usage ()
  end
  else begin
    if l == 2
    then begin
      let main = DefaultServer.main in
      match Sys.argv.(1) with
      | "--sql" -> run SqlServer.main
      | "--hash" -> run DefaultServer.main
      | _ -> usage ()
    end
    else run DefaultServer.main
  end
