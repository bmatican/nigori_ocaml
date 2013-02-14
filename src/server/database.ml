type t = {
  user_store = string;
}

let add_user store pub_key pub_hash = false

let have_user store pub_hash = false

let delete_user store user = false

let get_user store pub_hash = None

let get_public_key store pub_hash = None



let check_and_add_nonce store nonce pub_hash = false

let clear_old_nonces store = ()



let get_record store user key = None 

let get_revision store user key revision = None

let get_revisions store user key = None



let put_record store user key revision data = false

let delete_record store user key = false

