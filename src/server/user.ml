type public_key = Primitives.DSA.key
type hash = string
type t = {
  name : string;
  public_key : public_key;
  public_hash : hash;
  registration_date : float;
}

let create pub_key pub_hash =
  {
    name = Cohttp.Base64.encode pub_hash;
    public_key = pub_key;
    public_hash = pub_hash;
    registration_date = Unix.time ()
  }

let string_to_key key = Primitives.DSA.deserialize_key key
let key_to_string key = Primitives.DSA.serialize_key key

let string_to_hash hash = hash
let hash_to_string hash = hash

let get_name user = user.name
let get_key user = user.public_key
let get_hash user = user.public_hash
let get_registration_date user = user.registration_date

let to_string user = 
  let str_name = get_name user in
  let str_key = key_to_string (get_key user) in
  let str_hash = hash_to_string (get_hash user) in
  let str_date = string_of_float (get_registration_date user) in
  Utils.encode_length [str_name; str_key; str_hash; str_date;]

let from_string str = 
  match Utils.decode_length str with
  | [str_name; str_key; str_hash; str_date;] ->
    {
      name = str_name;
      public_key = string_to_key str_key;
      public_hash = string_to_hash str_hash;
      registration_date = float_of_string  str_date;
    }
  | _ -> raise Utils.InvalidEncodingLength
