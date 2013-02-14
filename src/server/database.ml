open Messages_t

let default_revisions_size = 10
let default_store_size = 100
let default_stores_size = 10
let default_users_size = 10
let default_nonces_size = 100

type map_revisions = (string, string) Hashtbl.t
type map_store = (string, map_revisions) Hashtbl.t
type map_stores = (User.t, map_store) Hashtbl.t
type map_users = (User.hash, User.t) Hashtbl.t
type map_nonces = (User.public_key, Nonce.t list) Hashtbl.t

let make_map_revisions () = Hashtbl.create default_revisions_size
let make_store () = Hashtbl.create default_store_size
let make_stores () = Hashtbl.create default_stores_size
let make_users () = Hashtbl.create default_users_size
let make_nonces () = Hashtbl.create default_nonces_size

type t = {
  stores : map_stores;
  users : map_users;
  nonces : map_nonces;
}

let key_set tbl =
  let f = (fun k v l ->
    k :: l
  ) in
  Hashtbl.fold f tbl []

let value_set tbl =
  let f = (fun k v l ->
    v :: l
  ) in
  Hashtbl.fold f tbl []

let create () =
  let stores = make_stores () in
  let users = make_users () in
  let nonces = make_nonces () in
  {
    stores = stores;
    users = users;
    nonces = nonces;
  }


let have_user store pub_hash = 
  Hashtbl.mem store.users pub_hash


let add_user store pub_key pub_hash =
  if have_user store pub_hash
  then false
  else begin
    let user = User.create pub_key pub_hash in
    Hashtbl.replace store.users pub_hash user;
    Hashtbl.replace store.stores user (make_store ());
    true
  end


let delete_user store user =
  let hash = User.get_hash user in
  if not (have_user store hash)
  then false
  else begin
    Hashtbl.remove store.users hash;
    if not (Hashtbl.mem store.stores user)
    then false
    else begin
      (Hashtbl.remove store.stores user);
      true
    end
  end


let get_user store pub_hash =
  if have_user store pub_hash
  then
    Some (Hashtbl.find store.users pub_hash)
  else None


let get_public_key store pub_hash = 
  if have_user store pub_hash
  then
    Some (User.get_key (Hashtbl.find store.users pub_hash))
  else None
 

let get_indices store user = 
  let hash = User.get_hash user in
  if not (have_user store hash)
  then None
  else begin
    let s = Hashtbl.find store.stores user in
    Some (key_set s)
  end


let get_record store user key =
  let hash = User.get_hash user in
  if not (have_user store hash)
  then None
  else begin
    let s = Hashtbl.find store.stores user in
    if not (Hashtbl.mem s key)
    then None
    else begin
      let revs = Hashtbl.find s key in
      let f = ( fun k v l ->
        let rv = {
          revval_revision = k;
          revval_value = v;
        } in
        rv :: l
      ) in
      Some (Hashtbl.fold f revs [])
    end
  end


let get_revision store user key revision = 
  let hash = User.get_hash user in
  if not (have_user store hash)
  then None
  else begin
    let s = Hashtbl.find store.stores user in
    if not (Hashtbl.mem s key)
    then None
    else begin
      let revs = Hashtbl.find s key in
      if not (Hashtbl.mem revs revision)
      then None
      else begin
        let value = Hashtbl.find revs revision in
        let rv = {
          revval_revision = revision;
          revval_value = value;
        } in
        Some (rv)
      end
    end
  end
  

let get_revisions store user key =
  let hash = User.get_hash user in
  if not (have_user store hash)
  then None
  else begin
    let s = Hashtbl.find store.stores user in
    if not (Hashtbl.mem s key)
    then None
    else begin
      let revs = Hashtbl.find s key in
      Some (key_set revs)
    end
  end


let put_record store user key revision data =
  let hash = User.get_hash user in
  if not (have_user store hash)
  then false
  else begin
    let s = Hashtbl.find store.stores user in
    let revs = 
      if Hashtbl.mem s key
      then Hashtbl.find s key
      else begin
        let revs = make_map_revisions () in
        Hashtbl.replace s key revs;
        revs
      end
    in
    if Hashtbl.mem revs revision
    then 
      let prev = Hashtbl.find revs revision in
      if prev == data
      then true
      else false
    else begin
      Hashtbl.replace revs revision data;
      true
    end
  end


let delete_record store user key =
  let hash = User.get_hash user in
  if not (have_user store hash)
  then false
  else begin
    let s = Hashtbl.find store.stores user in
    if not (Hashtbl.mem s key)
    then false
    else begin
      Hashtbl.remove s key;
      true
    end
  end

let check_and_add_nonce store nonce pub_key =
  if Nonce.is_recent nonce
  then false
  else begin
    if not (Hashtbl.mem store.nonces pub_key)
    then begin
      Hashtbl.replace store.nonces pub_key [nonce];
      true
    end
    else begin
      let nonces = Hashtbl.find store.nonces pub_key in
      Hashtbl.replace store.nonces pub_key (nonce :: nonces);
      true
    end
  end

let clear_old_nonces store =
  let clear = (fun acc el ->
    if Nonce.is_recent el
    then acc
    else el :: acc
  ) in
  let f = (fun k v ->
    let recent = List.fold_left clear [] v in
    Hashtbl.replace store.nonces k recent
  ) in
  Hashtbl.iter f store.nonces
