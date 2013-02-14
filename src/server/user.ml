type t = {
  name : string;
  public_key : string;
  public_hash : string;
  registration_date : float;
}
type public_key = string

let create name pub_key pub_hash =
  {
    name : name;
    public_key : pub_key;
    public_hash : pub_hash;
    registration_date : Unix.time ()
  }

let make_key key = key
