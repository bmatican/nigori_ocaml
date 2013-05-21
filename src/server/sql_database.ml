open Messages_t

module DB = struct
  exception IntegrityException
  (* Some of these libraries feel really cumbersome to use...
   * Took me a while to realize I cannot name my types index. *)

  (* Now I realize that FKs cannot be used for querying... *)

  (* Also, if you want to delete, apparently you need to use THE ACTUAL ITEM *)

  (* Why do we keep nonces hashed by pub_key and not hash? *)

  type user = {
    user_hash : string;
    user_string : string;
  } and key = {
    (* key_fk_user : user; *)
    key_fk_user : string; (* using the hash *)
    key : string;
  } and revision = {
    (* revision_fk_key : key; *)
    revision_fk_key : string; (* using the user_hash + key combination *)
    rev : string;
    value : string;
  } and nonce = {
    (* nonce_fk_user : user; *)
    nonce_fk_user : string; (* using the hash *)
    nonce_string : string;
  } with orm

  type t = {
    users : (user, [`RW]) Orm.Db.t;
    keys: (key, [`RW]) Orm.Db.t;
    revisions: (revision, [`RW]) Orm.Db.t;
    nonces: (nonce, [`RW]) Orm.Db.t;
  }

  let db_name = "nigori.db"

  let make_key_fk_user user =
    user.user_hash
  let make_revision_fk_key user key =
    Utils.encode_length [user.user_hash; key]
  let make_nonce_fk_user user =
    user.user_hash

  let create () =
    {
      users = user_init db_name;
      keys = key_init db_name;
      revisions = revision_init db_name;
      nonces = nonce_init db_name;
    }


  let database_get_key database db_user key =
    let key_fk_user = make_key_fk_user db_user in
    let keys = key_get
      ~key_fk_user:(`Eq(key_fk_user))
      ~key:(`Eq(key))
      database.keys
    in
    match keys with
    | [] -> None
    | [k] -> Some(k)
    | _ -> raise IntegrityException


  let database_get_keys database db_user =
    let key_fk_user = make_key_fk_user db_user in
    key_get
      ~key_fk_user:(`Eq(key_fk_user))
      database.keys


  let database_get_revision database db_user key rev =
    let revision_fk_key = make_revision_fk_key db_user key in
    let revs = revision_get
      ~revision_fk_key:(`Eq(revision_fk_key))
      ~rev:(`Eq(rev))
      database.revisions
    in
    match revs with
    | [] -> None
    | [r] -> Some(r)
    | _ -> raise IntegrityException


  let database_get_revisions database db_user key =
    revision_get
      ~revision_fk_key:(`Eq(make_revision_fk_key db_user key))
      database.revisions


  let database_delete_revisions database db_user key =
    let revs = database_get_revisions database db_user key in
    let f rev = revision_delete database.revisions rev in
    List.iter f revs


  let database_delete_keys database db_user =
    let keys = database_get_keys database db_user in
    let f db_key = 
      database_delete_revisions database db_user db_key.key;
      key_delete database.keys db_key
    in
    List.iter f keys


  let database_get_nonce database db_user nonce =
    let nonces = nonce_get
      ~nonce_fk_user:(`Eq(make_nonce_fk_user db_user))
      ~nonce_string:(`Eq(Nonce.to_string nonce))
      database.nonces
    in
    match nonces with
    | [] -> None
    | [n] -> Some(n)
    | _ -> raise IntegrityException


  let database_get_nonces database db_user =
    nonce_get
      ~nonce_fk_user:(`Eq(make_nonce_fk_user db_user))
      database.nonces


  let database_get_user database pub_hash =
    let users = user_get ~user_hash:(`Eq (User.hash_to_string pub_hash)) database.users in
    match users with
    | [] -> None
    | [el] -> Some (el)
    | _ -> raise IntegrityException


  let option_with_user database hash fn =
    match database_get_user database hash with
    | None -> None
    | Some(u) -> fn u


  let bool_with_user database hash fn =
    match database_get_user database hash with
    | None -> false
    | Some(u) -> fn u


  let get_user ~database ~pub_hash =
    option_with_user database pub_hash (fun user -> begin
      Some(User.from_string user.user_string)
    end)


  let have_user ~database ~pub_hash =
    bool_with_user database pub_hash (fun _ -> true)


  let add_user ~database ~pub_key ~pub_hash =
    if have_user database pub_hash
    then false
    else begin
      let user = User.create pub_key pub_hash in
      let user_string = User.to_string user in
      let user_to_save = {
        user_hash = User.hash_to_string pub_hash;
        user_string = user_string;
      } in
      user_save database.users user_to_save;
      true
    end


  let delete_user ~database ~user =
    let hash = User.get_hash user in
    bool_with_user database hash (fun u -> begin
      (* TODO: do proper? *)
      database_delete_keys database u;
      user_delete database.users u;
      true
    end)


  let get_public_key ~database ~pub_hash =
    option_with_user database pub_hash (fun u -> begin
      Some(User.get_key (User.from_string u.user_string))
    end)


  let get_indices ~database ~user =
    let hash = User.get_hash user in
    option_with_user database hash (fun u -> begin
      let key_fk_user = make_key_fk_user u in
      let indices = key_get ~key_fk_user:(`Eq (key_fk_user)) database.keys in
      let get_data = (fun l ind ->
        ind.key :: l
      ) in
      Some(List.fold_left get_data [] indices)
    end)


  let get_record ~database ~user ~key ?(revision=None) () =
    let hash = User.get_hash user in
    option_with_user database hash (fun u -> begin
      match database_get_key database u key with
      | None -> None
      | Some(k) -> begin
        match revision with
        | None -> begin
          let revisions = database_get_revisions database u key in
          let f = (fun l r ->
            let rv = {
              revval_revision = r.rev;
              revval_value = r.value;
            } in
            rv :: l
          ) in
          Some(List.fold_left f [] revisions)
        end
        | Some(rev) -> begin
          match database_get_revision database u k.key rev with
          | None -> None
          | Some(r) -> begin
            let rv = {
              revval_revision = r.rev;
              revval_value = r.value;
            } in
            Some([rv])
          end
        end
      end
    end)


  let get_revisions ~database ~user ~key =
    let hash = User.get_hash user in
    option_with_user database hash (fun u -> begin
      match database_get_key database u key with
      | None -> None
      | Some(k) -> begin
        let revisions = database_get_revisions database u key in
        let f = (fun l r ->
          r.rev :: l
        ) in
        Some(List.fold_left f [] revisions)
      end
    end)


  let put_record ~database ~user ~key ~revision ~data =
    let hash = User.get_hash user in
    bool_with_user database hash (fun u -> begin
    (* TOOD: continue from here *)
      let put () =
        let rev_to_save = {
          revision_fk_key = make_revision_fk_key u key;
          rev = revision;
          value = data;
        } in
        revision_save database.revisions rev_to_save;
        true
      in
      match database_get_key database u key with
      | None -> begin
        let key_to_save = {
          key_fk_user = make_key_fk_user u;
          key = key;
        } in
        key_save database.keys key_to_save;
        put ()
      end
      | Some k -> begin
        put ()
      end
    end)


  let delete_record ~database ~user ~key ?(revision=None) () =
    let hash = User.get_hash user in
    bool_with_user database hash (fun u -> begin
      match revision with
      | None -> begin
        match database_get_key database u key with
        | None -> false
        | Some k -> begin
          database_delete_revisions database u key;
          key_delete database.keys k;
          true
        end
      end
      | Some rev -> begin
        match database_get_revision database u key rev with
        | None -> false
        | Some r -> begin
          revision_delete database.revisions r;
          (* TODO: do proper *)
          let revs = database_get_revisions database u key in
          if List.length revs == 0
          then begin
            match database_get_key database u key with
            | None -> raise IntegrityException
            | Some k -> key_delete database.keys k
          end;
          true
        end
      end
    end)


  let check_and_add_nonce ~database ~nonce ~pub_hash =
    (* TODO: raise issue about this? *)
    bool_with_user database pub_hash (fun u -> begin
      match database_get_nonce database u nonce with
      | None -> begin
        let nonce_to_save = {
          nonce_fk_user = make_nonce_fk_user u;
          nonce_string = Nonce.to_string nonce;
        } in
        nonce_save database.nonces nonce_to_save;
        true
      end
      | Some _ -> false
    end)


  let clear_old_nonces ~database =
    let clear = (fun db_nonce ->
      let nonce = Nonce.from_string db_nonce.nonce_string in
      if Nonce.is_recent nonce
      then nonce_delete database.nonces db_nonce
    ) in
    let nonces = nonce_get database.nonces in
    List.iter clear nonces
end
