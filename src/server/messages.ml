open Messages_j
open Messages_t

let a () = 
  let url = {
    url_path="google.com";
  } in
  let app = {
    app_name="Some name";
    app_url=url;
  } in
  let string_app = string_of_app app in
  let newapp = app_of_string string_app in
  print_endline (string_of_app newapp)
