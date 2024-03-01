(** Ciphers
    Builtin integer based ciphers.
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

let bezout a b =
  let rec bezout_rec a b uA vA uB vB =
    if (modulo a b) = 0
    then (uB, vB, b)
    else bezout_rec b (modulo a b) uB vB (uA - uB * (quot a b)) (vA - vB * (quot a b))
  in bezout_rec a b 1 0 0 1;;

let mod_power x n m =
  let rec power_rec x r = function
      0 -> r
     |n when n mod 2 = 1 -> power_rec (modulo (x*x) m) (modulo (r*x) m) (n/2)
     |n -> power_rec (modulo (x*x) m) r (n/2)
  in power_rec (modulo x m) 1 n;;



(**open Builtin
open Basic_arithmetics
open Power*)

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
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

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
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


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinctcount . Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let e = (p-1)*(q-1)
  in let e2 = e/2 + 1
     in let (a, _, _) = bezout e2 e
        in (((quot p q), e2), ((quot p q), a));;

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = (0, 0)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = (0, 0)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = (0, 0)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = 0
