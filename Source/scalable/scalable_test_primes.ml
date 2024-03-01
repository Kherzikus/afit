(** Testing for primality *)

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


let div a b =
  (quot a b, modulo a b);;


let rec gcd a b =
  let (q, r) = div a b in
  if r = 0
  then b
  else gcd b r;;


let bezout a b =
  let rec bezout_rec a b uA vA uB vB =
    if (modulo a b) = 0
    then (uB, vB, b)
    else bezout_rec b (modulo a b) uB vB (uA - uB * (quot a b)) (vA - vB * (quot a b))
  in bezout_rec a b 1 0 0 1;;


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

let mod_power b e m =
  let rec mod_power_rec b e m =
    if e = 0
    then 1
    else if e mod 2 = 0
    then let res = mod_power_rec b (e/2) m
      in (res*res) mod m
    else let res = mod_power_rec b ((e-1)/2) m
      in (b*res*res) mod m
  in mod_power_rec b e m;;

let is_prime_a n =
  let d2 = n / 2 in
  let rec is_prime_rec n d =
    if d > d2
    then true
    else if n mod d = 0
         then false
         else is_prime_rec n (d+1)
  in is_prime_rec n 2;;

let is_pseudo_prime_a p test_seq =
  if test_seq = []
  then false
  else let rec is_pseudo_prime_rec p test_seq =
         match test_seq with
           [] -> true
          |elm::l -> if (mod_power elm (p-1) p) = 1 || p = 2
                     then is_pseudo_prime_rec p l
                     else false
       in is_pseudo_prime_rec p test_seq;;





(**open Scalable
open Scalable_basic_arithmetics
open Scalable_power*)

(** Deterministic primality test *)
let is_prime n =
  if (is_prime_a (to_int n)) = true
  then true
  else false;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec aux test_seq =
    match test_seq with
      [] -> []
    |e::l -> (to_int e)::(aux l)
  in
  if (is_pseudo_prime_a (to_int p) (aux test_seq)) = true
  then true
  else false;;
