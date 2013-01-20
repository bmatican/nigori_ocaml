exception InvalidListLength

let take_first l n = 
  let rec take_first_helper l n = 
    if n <= 0 then []
    else match l with
      [] -> raise InvalidListLength
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
    let octet_size = 8 in
    let size = 1 lsl octet_size in
    let mask = size - 1 in
    split_by_octets (nr lsr octet_size) ((nr land mask) :: pieces)

let four_octet_encode nr = 
  let size = 4 in
  let octets = split_by_octets nr [] in
  let diff = size - (List.length octets) in
  let first_four =
    if diff > 0 
    then pad_with_zeros octets diff
    else take_first octets size in
  let result = String.create size in
  let set_result index nr =
    String.set result index (char_of_int nr) in
  List.iteri set_result first_four;
  result

let concat a b = a ^ b

let concat_list l = List.fold_left concat "" l

exception InvalidHashLength

let xor a b = 
  if String.length a == 0
  then b
  else
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

let xor_list l = List.fold_left xor "" l
