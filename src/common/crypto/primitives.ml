module C = Cryptokit

module Random = struct
  module R = C.Random
  let random_seed = R.(string secure_rng 55) (* Maximum size for pseudo_rng *)
  let random_gen size = R.string (R.pseudo_rng random_seed) size

  let generate size = random_gen size
end

module InitializationVector = struct
  type t = string
  exception InvalidVectorSize
  let valid_vector_size = 16 (* Taken from C. *)

  let create value = 
    if String.length value != valid_vector_size
    then raise InvalidVectorSize
    else value

  let create_random () = 
    create (Random.generate valid_vector_size)

  let to_string message = message
end

module AES256 = struct
  type t = string
  type key = string
  exception InvalidKeyLength (* Accepts 16, 24, 32 bytes *)

  let encrypt iv key text =
    try 
      let transform = C.(
        Cipher.aes
          ~iv:iv
          ~mode:Cipher.CBC
          ~pad:Padding.length (* As seen in the Nigori python version. *)
          key
          Cipher.Encrypt
      ) in
      C.(transform_string transform text)
    with 
      exn -> raise InvalidKeyLength

  let decrypt iv key text =
    try 
      let transform = C.(
        Cipher.aes
          ~iv:iv
          ~mode:Cipher.CBC
          ~pad:Padding.length (* As seen in the Nigori python version. *)
          key
          Cipher.Decrypt
      ) in
      C.(transform_string transform text)
    with 
      exn -> raise InvalidKeyLength

  let to_string message = message
end

module SHA1 = struct
  type t = string
  type message = string

  let apply message = C.(hash_string (Hash.sha1 ()) message)

  let to_string message = message
end

module HMAC = struct
  type t = string
  type key = string
  type message = string
  let apply key message = C.(hash_string (MAC.hmac_sha1 key) message)
  let to_string message = message
end

module PBKDF2 = struct
  type t = string
  type hash = SHA1.t
  type password = string
  type salt = string
  type count = int
  type dk_length = int
  exception DerivedKeyTooLong

  let hash_length = 20 (* sha1 output *)

  let rec u_compute password salt count iterations current_count result =
    if current_count > count
    then result
    else 
      let u_partial = (* U_i from RFC2898 *)
        if current_count == 1 (* TODO: is this correct? *)
        then 
          Utils.concat [
            salt;
            (Utils.four_octet_encode iterations);
          ]
        else List.hd result in 
      let message = HMAC.apply password u_partial in
      let next = message :: result in 
      u_compute password salt count iterations (current_count + 1) next

  let f password salt count iterations = 
    let result = u_compute password salt count iterations 1 [] in 
    Utils.xor result

  let rec t_compute password salt count current_step steps last_length result = 
    let f_result = f password salt count current_step in
    if current_step >= steps
    then
      (String.sub f_result ((steps - 1) * hash_length) last_length) :: result
    else
      let next = f_result :: result in
        t_compute password salt count (current_step + 1) steps last_length next

  let apply password salt count dk_length = 
    if dk_length > hash_length * (1 lsl 32 - 1)
    then raise DerivedKeyTooLong
    else
      let l = int_of_float (ceil ((float dk_length) /. (float hash_length))) in
      let r = dk_length - (l - 1) * hash_length in
      let pieces = t_compute password salt count 1 l r [] in
      Utils.concat pieces

  let to_string message = message
end

module Enc = struct
  type t = string
  type key = string
  type plaintext = string

  let create : InitializationVector.t -> AES256.t -> HMAC.t -> t = 
    fun iv cipher hmac -> Utils.concat [iv; cipher; hmac]

  let enc key1 key2 plaintext = 
    let iv = InitializationVector.create_random () in
    let cipher = AES256.encrypt iv key1 plaintext in
    let hmac = HMAC.apply key2 cipher in
    create iv cipher hmac

  let enc_det key1 key2 key3 plaintext = 
    let temp = HMAC.apply key3 plaintext in
    let half = (String.length temp) / 2 in
    let f1 = String.sub temp 0 half in
    let f2 = String.sub temp half half in
    let g = Utils.xor [f1; f2] in
    let cipher = AES256.encrypt g key1 plaintext in
    let hmac = HMAC.apply key2 cipher in
    create g cipher hmac

  let to_string message = message
end

(* Taken from Anil's port of DSA to  *)
open Nat
open Gmp
open Gmp.Z.Infixes
open Cryptokit

(* Conversions between Z.t and btye-strings *)

let zt_conv_table = "0123456789abcdef"

let zt_of_bytes s =
  let l = String.length s in
  let s2 = String.create (2 * l) in
    for i = 0 to l -1 do
      let b = Char.code s.[i] in
        s2.[2*i] <- zt_conv_table.[b lsr 4];
        s2.[2*i + 1] <- zt_conv_table.[b land 0x0f];
    done;
    Z.from_string_base 16 s2;;

let bytes_of_zt z =
  let h2i c =
    match c with
        '0' .. '9' -> Char.code c - 48
      | 'A' .. 'F' -> Char.code c - 55
      | 'a' .. 'f' -> Char.code c - 87
      | _ -> raise (Error Bad_encoding) in
  let s2 = Z.to_string_base 16 z in
  let l2 = String.length s2 in
  let ss = if (l2 mod 2) = 0 then s2 else
    (let t = (String.make (l2 + 1) '0') in
      String.blit s2 0 t 1 l2; t) in
  let l = (String.length ss)/2 in
  let s = String.create l in
    for i=0 to l-1 do
      s.[i] <- Char.chr (((h2i ss.[2*i]) lsl 4) + (h2i ss.[2*i + 1]));
    done;
    s

module RC = struct
  include Cryptokit.Random

  let bit_string rng bits =
    let len = ((bits + 7) / 8) and
    extra = (bits mod 8) in
    let res = string rng len in
      if extra = 0 then res
      else let mask = (255 lsr (8-extra)) in
        res.[0] <- (Char.chr ((Char.code res.[0]) land mask));
        res

  let bit_zt rng bits = 
    zt_of_bytes (bit_string rng bits)

  class gmp_default_rng =
  object (self)
    val mutable state = RNG.default
    method random_bytes buf ofs len =
      if len > 0 then 
        let s = bytes_of_zt (Z.urandomb state (8*len)) in
    if (String.length s) < len then
      let d = len - (String.length s) in
        String.blit (String.make d (Char.chr 0)) 0 buf ofs d;
        String.blit s 0 buf (ofs+d) (String.length s)
    else
      String.blit s 0 buf ofs len
    method wipe = 
      state <- RNG.default
  end

  let gmp_default_rng = new gmp_default_rng

  let rec dsa_primes rng bits =
    if bits < 512 || bits > 2048 || (bits mod 64) <> 0 then
      raise (Error Wrong_key_size)
    else
      let g = 160 and n = (bits - 1) / 160 and b = (bits - 1) mod 160 in
      let twog = Z.pow_ui_ui 2 g and two160 = Z.pow_ui_ui 2 160 and 
	twol1 = Z.pow_ui_ui 2 (bits - 1) in
      let hash x = 
	let h = Hash.sha1 () in
	  hash_string h x in
      let s = ref "" in
	(*  Note that as typed, twog = two160, but g can be changed if desired, 
	    so we use a separate variable for 2^160 here *)
      let q = 
	let q = ref Z.zero and u = ref "" and composite = ref true in
	  while !composite do
	    s := bit_string rng g;
	    u := bytes_of_zt (Z.dmod ((zt_of_bytes !s) +! Z.one) twog);
	    xor_string (hash !s) 0 (hash !u) 0  (Hash.sha1())#hash_size;
	    q := Z.bior (zt_of_bytes !u) ((Z.pow_ui_ui 2 159) +! Z.one);
	    composite := not (Z.is_probab_prime !q 18);
	  done; !q in
      let qq = Z.mul_ui q 2 in
      let rec make_primes i j q p =
	match i with
	    4096 -> dsa_primes rng bits
	  | -1 -> (q,p)
	  | _ ->
	      let v = ref Z.zero and x = ref Z.zero and pos = ref Z.one in
		for k = 0 to n-1 do
		  v := zt_of_bytes 
		    (hash (bytes_of_zt (Z.dmod ((zt_of_bytes !s) +! (Z.of_int (k+j))) twog)));
		  x := !x +! (!v *! !pos);
		  pos := !pos *! two160;
		done;
		v := zt_of_bytes 
		  (hash (bytes_of_zt (Z.dmod ((zt_of_bytes !s) +! (Z.of_int (n+j))) twog)));
		x := !x +! ((Z.dmod !v (Z.pow_ui_ui 2 b)) *! !pos) +! twol1;
		let c = Z.dmod !x qq in
		let candidate = !x -! (c -! Z.one) in
		  if (candidate >=! twol1) && (Z.is_probab_prime candidate 5) then
		    make_primes (-1) j q candidate
		  else 
		    make_primes (i+1) (j+n+1) q Z.zero in
	make_primes 0 2 q Z.zero		
	  
end

module DSA = struct
  type group_info =
      { size : int;
	p : Z.t;
	q : Z.t;
	base : Z.t }

  type key = group_info * Z.t

  let sign ?rng msg key =
    let g x = 
      let h = Hash.sha1 () in
	hash_string h x in
    let (grp,x) = key and
      prng = match rng with
	  Some rng -> rng
	| None -> RC.gmp_default_rng 
    in
    if grp.size < 512 || grp.size > 2048 || (grp.size mod 64) <> 0 then
      raise (Error Wrong_key_size);
      (** Note that according to FIPS 186-2 grp.size should strictly by
	1024.  However, I feel a bit more flexibility is a good thing,
	and I never planned on claiming to be FIPS 186-2 compliant... I
	simply use it as a guide. *)	
      let gen_k_ki_r prng = 
	(** generate k and r as described in appendix 3 of FIPS 186-2 *)
	let t = bytes_of_zt 
		  (Z.from_string_base 16 "67452301efcdab8998badcfe10325476c3d2e1f0") and
	  b = 160 in (* This parameter is user settable, so long
			as 160 <= b <=512 *)
	let kkey = ref (RC.bit_string prng b) in
	let w0 = zt_of_bytes (g (t ^ !kkey)) in
	  kkey := bytes_of_zt 
	    (Z.dmod (Z.one +! (zt_of_bytes !kkey) +! w0) (Z.pow_ui_ui 2 b));
	  let w1 = zt_of_bytes (g (t ^ !kkey)) in
	  kkey := bytes_of_zt 
	    (Z.dmod (Z.one +! (zt_of_bytes !kkey) +! w1) (Z.pow_ui_ui 2 b));
	    let k = (w0 *! (Z.pow_ui_ui 2 b)) +! w1 in
	    let ki = match (Z.inverse k grp.q) with
		None -> raise (Failure "DSA.sign: k not invertable mod q!")
	      | Some ki -> ki and
	      r = Z.dmod (Z.powm grp.base k grp.p) grp.q in
	      (k,ki,r) in
      let (k,ki,r) = gen_k_ki_r prng in
      let s = Z.dmod (ki *! ((zt_of_bytes (g msg)) +! (x *! r))) grp.q in
	((bytes_of_zt r),(bytes_of_zt s))

  let verify msg signature key =
    let g x = 
      let h = Hash.sha1 () in
	hash_string h x in
    let r = zt_of_bytes (fst signature) and
      s = zt_of_bytes (snd signature) and
      (grp,y) = key in
      if r <=! Z.zero || r >=! grp.q || s <=! Z.zero || s >=! grp.q then false
      else
	let w = match (Z.inverse s grp.q) with 
	    None -> raise (Failure "DSA.verify: s is not invertable mod q!")
	  | Some w -> w in
	let u1 = Z.dmod (w *! (zt_of_bytes (g msg))) grp.q and
	  u2 = Z.dmod (r *! w) grp.q in
	let v = Z.dmod 
		  (Z.dmod ((Z.powm grp.base u1 grp.p) *! (Z.powm y u2 grp.p)) grp.p) grp.q in
	  if v =! r then true else false
	    
  let new_key ?rng ?grp_info ?num_bits xseed =
    let prng = match rng with
	Some rng -> rng
      | None -> RC.gmp_default_rng in
    let grp_info = match grp_info with
	Some grp_info -> grp_info
      | None ->
	  let size = match num_bits with 
	      Some size -> size
	    | None -> 1024 in
	  let (q,p) = RC.dsa_primes prng size in
	  let base = 
	    let rec gen_base prng bits = 
	      let g = (Z.dmod (RC.bit_zt prng (2 * bits)) (p -! Z.one)) +! Z.one and
		ex = (p -! Z.one) /! q in
	      let base = Z.powm g ex p in
		if base =! Z.one then gen_base prng bits
		else base in
	      gen_base prng size in
	    {size = size; q = q; p = p; base = base} in
    let gen_x prng =
      (** generate x as described in appendix 3 of FIPS 186-2 *)
      let h = Hash.sha1 () in
      let g x = hash_string h x in
      let t = bytes_of_zt 
		(Z.from_string_base 16 "67452301efcdab8998badcfe10325476c3d2e1f0") and
	b = 160 and (* This parameter is user settable, so long
		      as 160 <= b <=512 *)
	seed = zt_of_bytes xseed in
      let twob = Z.pow_ui_ui 2 b in
      let xkey = ref (RC.bit_zt prng b) in
      let xval = ref (Z.dmod (!xkey +! seed) twob) in
      let w0 = zt_of_bytes (g (t ^ (bytes_of_zt !xval))) in
	xkey := Z.dmod (Z.one +! !xkey +! w0) twob;
	xval := Z.dmod (!xkey +! seed) twob;
	let w1 = zt_of_bytes (g (t ^ (bytes_of_zt !xval))) in
	  Z.dmod ((w0 *! twob) +! w1) grp_info.q in
    let x = gen_x prng in
      ((grp_info, (Z.powm grp_info.base x grp_info.p)),
       (grp_info, x))
end
