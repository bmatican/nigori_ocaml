type public_key = string
type hash = string
type t = {
  name : string;
  public_key : public_key;
  public_hash : hash;
  registration_date : float;
}

let create pub_key pub_hash =
  {
    name = Cohttp.Base64.encode pub_key;
    public_key = pub_key;
    public_hash = pub_hash;
    registration_date = Unix.time ()
  }

let make_key key = key
let make_hash hash = hash

let get_name user = user.name
let get_key user = user.public_key
let get_hash user = user.public_hash
let get_registration_date user = user.registration_date
