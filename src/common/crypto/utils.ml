exception InvalidListLength
exception InvalidEncodingLength
exception InvalidHashLength

let to_hex message = Cryptokit.(transform_string (Hexa.encode ()) message)

let octet_size = 8

let bin2int str = 
  if String.length str != 4
  then begin
    Printf.eprintf "Failed bin2int\n";
    raise InvalidEncodingLength
  end
  else begin
    let mask = (1 lsl octet_size) - 1 in
    let p1 = (int_of_char str.[0]) land mask in
    let p2 = (int_of_char str.[1]) land mask in
    let p3 = (int_of_char str.[2]) land mask in
    let p4 = (int_of_char str.[3]) land mask in
    (p1 lsl 24) + (p2 lsl 16) + (p3 lsl 8) + (p4)
  end

let int2bin nr =
  let mask = (1 lsl octet_size) -1 in
  let str = String.create 4 in
  str.[0] <- char_of_int ((nr lsr 24) land mask);
  str.[1] <- char_of_int ((nr lsr 16) land mask);
  str.[2] <- char_of_int ((nr lsr 8) land mask);
  str.[3] <- char_of_int (nr land mask);
  str

let take_first l n = 
  let rec take_first_helper l n = 
    if n <= 0 then []
    else match l with
      | [] -> raise InvalidListLength
      | h :: t -> h :: take_first_helper t (n - 1) in
  List.rev (take_first_helper l n)

let rec pad_with_zeros l n = 
  if n <= 0
  then l
  else pad_with_zeros (0 :: l) (n - 1)

let rec split_by_octets nr pieces = 
  if nr <= 0
  then pieces
  else 
    let size = 1 lsl octet_size in
    let mask = size - 1 in
    split_by_octets (nr lsr octet_size) ((nr land mask) :: pieces)

let xor l = 
  let xor_helper a b = 
    if String.length a != String.length b
    then raise InvalidHashLength
    else
      let result = String.copy a in
      let iterator i c = 
        let xor_int = (int_of_char result.[i]) lxor (int_of_char c) in
        let xor_char = char_of_int xor_int in
        String.set result i xor_char in
      String.iteri iterator b;
      result
  in
  match l with
  | [] -> ""
  | [el] -> el
  | h :: t -> List.fold_left xor_helper h t


let concat l = 
  let concat_helper a b = a ^ b in
  match l with 
  | [] -> ""
  | [el] -> el
  | h :: t -> List.fold_left concat_helper h t

let encode_length l =
  let append_length element = 
    let len = String.length element in
    let len_s = int2bin len in
    len_s ^ element
  in
  String.concat "" (List.map append_length l)

let int_size = 4

let rec decode_length s = 
  let len = String.length s in
  if len < int_size
  then begin
    Printf.eprintf "wtf\n";
    raise InvalidEncodingLength
  end
  else
    let encoded_size = String.sub s 0 int_size in
    let size = bin2int encoded_size in
    try
      let message = String.sub s int_size size in
      let offset = int_size + size in
      let rest = len - offset in
      if rest == 0
      then [message]
      else message :: (decode_length (String.sub s offset rest))
    with exn -> raise InvalidEncodingLength
