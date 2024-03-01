(** Ciphers
    bitarrays based ciphers.
*)

let sign x =
  if x >= 0 then 1 else -1;;

let quot a b =
  let q = a / b in
  if a mod b < 0
  then (q - (sign b))
  else q;;

let modulo a b =
  let r = a mod b in
  if r < 0
  then r + b * sign b
  else r;;

let pow x n =
  let rec pow_rec x n a =
    if n = 0
    then a
    else pow_rec x (n-1) (x*a)
  in pow_rec x n 1;;

let power x n =
  if n = 0
  then 1
  else let rec  power_rec x n =
    if n = 0
    then 1
    else x * power_rec x (n-1)
       in power_rec x n;;

let mod_power x n m =
  let rec power_rec x r = function
      0 -> r
     |n when n mod 2 = 1 -> power_rec (modulo (x*x) m) (modulo (r*x) m) (n/2)
     |n -> power_rec (modulo (x*x) m) r (n/2)
  in power_rec (modulo x m) 1 n;;

let prime_mod_power x n p =
  if x mod p <> 0
  then let r = modulo n (p-1)
       in mod_power x r p
  else mod_power x n p;;

let from_int x =
  let rec from_int_rec x =
    match x with
      n when n < 1 -> []
     |n when n = 1 -> 1::from_int_rec (x/2)
     |_ -> (x mod 2)::from_int_rec (x/2)
  in if sign x < 0
     then 1::from_int_rec (-x)
     else 0::from_int_rec x;;

let to_int bA =
  let rec to_int_rec bA n =
    match bA with
      [] -> 0
     |e::l when e = 1 -> n + to_int_rec l (n*2)
     |e::l -> to_int_rec l (n*2)
  in match bA with
       e::l when e = 1 -> (-1) * to_int_rec l 1
      |e::l -> to_int_rec l 1
      |_ -> failwith "err";;


let bezout a b =
  let rec bezout_rec a b uA vA uB vB =
    if (modulo a b) = 0
    then (uB, vB, b)
    else bezout_rec b (modulo a b) uB vB (uA - uB * (quot a b)) (vA - vB * (quot a b))
  in bezout_rec a b 1 0 0 1;;

let encrypt_cesar k m b =
  let rec encrypt_cesar_rec k m b =
    match m with
      [] -> []
     |e::l1 -> let encrypt_cesar_b count =
                 if count >= 0 && b > count
                 then (count + k) mod b
                 else failwith "err"
               in (encrypt_cesar_b e)::(encrypt_cesar_rec k l1 b)
  in encrypt_cesar_rec k m b;;

let decrypt_cesar k m b =
  let rec decrypt_cesar_rec k m b =
    match m with
      [] -> []
     |e::l1 -> let decrypt_cesar_b count =
                 if count >= 0 && b > count
                 then (count - k) mod b
                 else failwith "err"
               in (decrypt_cesar_b e)::(decrypt_cesar_rec k l1 b)
  in decrypt_cesar_rec k m b;;


let generate_keys_rsa_a p q =
  let e = (p-1)*(q-1)
  in let e2 = e/2 + 1
     in let (a, _, _) = bezout e2 e
        in (((quot p q), e2), ((quot p q), a));;

let encrypt_rsa_a m (n, e) =
  mod_power m e n;;

let decrypt_rsa_a m (n , d) =
  mod_power m d n;;

let rec public_data_g_a p = (0, 0)

let generate_keys_g_a (g, p) = (0, 0)

let encrypt_g_a msg (g, p) kA = (0, 0)

let decrypt_g_a (msgA, msgB) a (g, p) = 0













(**open Scalable
open Scalable_basic_arithmetics
open Scalable_power*)

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q =
  let ((a, b), (c, d)) = generate_keys_rsa_a (to_int p) (to_int q)
  in ((from_int a, from_int b), (from_int c, from_int d));;

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  from_int (encrypt_rsa_a (to_int m) ((to_int n), (to_int e)));;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  from_int (decrypt_rsa_a (to_int m) ((to_int n), (to_int d)));;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p = ([], [])

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = ([], [])

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = ([], [])

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = []
