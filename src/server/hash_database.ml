open Messages_t

module DB = struct
  let default_revisions_size = 10
  let default_store_size = 100
  let default_stores_size = 10
  let default_users_size = 10
  let default_nonce_size = 100
  let default_nonces_size = default_users_size

  type map_revisions = (string, string) Hashtbl.t
  type map_store = (string, map_revisions) Hashtbl.t
  type map_stores = (User.t, map_store) Hashtbl.t

  type map_users = (User.hash, User.t) Hashtbl.t

  (* TODO: Ocaml doesn't seem to have O(1) Sets. *)
  type map_nonce = (Nonce.t, bool) Hashtbl.t
  type map_nonces = (User.hash, map_nonce) Hashtbl.t

  let make_map_revisions () = Hashtbl.create default_revisions_size
  let make_store () = Hashtbl.create default_store_size
  let make_stores () = Hashtbl.create default_stores_size
  let make_users () = Hashtbl.create default_users_size
  let make_nonce () = Hashtbl.create default_nonce_size
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


  let have_user ~database ~pub_hash =
    Hashtbl.mem database.users pub_hash


  let add_user ~database ~pub_key ~pub_hash =
    if have_user database pub_hash
    then false
    else begin
      let user = User.create pub_key pub_hash in
      Hashtbl.replace database.users pub_hash user;
      Hashtbl.replace database.stores user (make_store ());
      true
    end


  let delete_user ~database ~user =
    let hash = User.get_hash user in
    if not (have_user database hash)
    then false
    else begin
      Hashtbl.remove database.users hash;
      if not (Hashtbl.mem database.stores user)
      then false
      else begin
        (Hashtbl.remove database.stores user);
        true
      end
    end


  let get_user ~database ~pub_hash =
    if have_user database pub_hash
    then
      Some (Hashtbl.find database.users pub_hash)
    else None


  let get_public_key ~database ~pub_hash =
    if have_user database pub_hash
    then
      Some (User.get_key (Hashtbl.find database.users pub_hash))
    else None


  let get_indices ~database ~user =
    let hash = User.get_hash user in
    if not (have_user database hash)
    then None
    else begin
      let s = Hashtbl.find database.stores user in
      Some (key_set s)
    end


  let get_record ~database ~user ~key ?(revision=None) () =
    let hash = User.get_hash user in
    if not (have_user database hash)
    then None
    else begin
      let s = Hashtbl.find database.stores user in
      if not (Hashtbl.mem s key)
      then None
      else begin
        let revs = Hashtbl.find s key in
        match revision with
        | None -> begin
          let f = ( fun k v l ->
            let rv = {
              revval_revision = k;
              revval_value = v;
            } in
            rv :: l
          ) in
          Some (Hashtbl.fold f revs [])
        end
        | Some rev -> begin
          if not (Hashtbl.mem revs rev)
          then None
          else begin
            let value = Hashtbl.find revs rev in
            let rv = {
              revval_revision = rev;
              revval_value = value;
            } in
            Some ([rv])
          end
        end
      end
    end


  let get_revisions ~database ~user ~key =
    let hash = User.get_hash user in
    if not (have_user database hash)
    then None
    else begin
      let s = Hashtbl.find database.stores user in
      if not (Hashtbl.mem s key)
      then None
      else begin
        let revs = Hashtbl.find s key in
        Some (key_set revs)
      end
    end


  let put_record ~database ~user ~key ~revision ~data =
    let hash = User.get_hash user in
    if not (have_user database hash)
    then false
    else begin
      let s = Hashtbl.find database.stores user in
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


  let delete_record ~database ~user ~key ?(revision=None) () =
    let hash = User.get_hash user in
    if not (have_user database hash)
    then false
    else begin
      let s = Hashtbl.find database.stores user in
      if not (Hashtbl.mem s key)
      then false
      else begin
        match revision with
        | None -> begin
          Hashtbl.remove s key;
          true
        end
        | Some rev -> begin
          let revs = Hashtbl.find s key in
          if not (Hashtbl.mem revs rev)
          then false
          else begin
            Hashtbl.remove revs rev;
            true
          end
        end
      end
    end

  let check_and_add_nonce ~database ~nonce ~pub_hash =
    if not (Nonce.is_recent nonce)
    then false
    else begin
      if not (Hashtbl.mem database.nonces pub_hash)
      then begin
        let nonce_store = make_nonce () in
        Hashtbl.replace nonce_store nonce true;
        Hashtbl.replace database.nonces pub_hash nonce_store;
        true
      end
      else begin
        let nonces = Hashtbl.find database.nonces pub_hash in
        if Hashtbl.mem nonces nonce
        then false
        else begin
          Hashtbl.replace nonces nonce true;
          true
        end
      end
    end

  let clear_old_nonces ~database =
    let clear_all = (fun hash nonces -> begin
      let clear = (fun nonce boolean -> begin
        if Nonce.is_recent nonce
        then Hashtbl.remove nonces nonce
      end) in
      Hashtbl.iter clear nonces
    end) in
    Hashtbl.iter clear_all database.nonces
end
