(** Power function implementations for bitarrays *)

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

let pow_a x n =
  let rec pow_rec x n a =
    if n = 0
    then a
    else pow_rec x (n-1) (x*a)
  in pow_rec x n 1;;

let power_a x n =
  if n = 0
  then 1
  else let rec  power_rec x n =
    if n = 0
    then 1
    else x * power_rec x (n-1)
       in power_rec x n;;

let mod_power_a x n m =
  let rec power_rec x r = function
      0 -> r
     |n when n mod 2 = 1 -> power_rec (modulo (x*x) m) (modulo (r*x) m) (n/2)
     |n -> power_rec (modulo (x*x) m) r (n/2)
  in power_rec (modulo x m) 1 n;;

let prime_mod_power_a x n p =
  if x mod p <> 0
  then let r = modulo n (p-1)
       in mod_power_a x r p
  else mod_power_a x n p;;

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





(**open Scalable
open Scalable_basic_arithmetics*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  from_int (pow_a (to_int x) (to_int n));;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  from_int (power_a (to_int x) (to_int n));;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  from_int (mod_power_a (to_int x) (to_int n) (to_int m));;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  from_int (prime_mod_power_a (to_int x) (to_int n) (to_int p));;
