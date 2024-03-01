(** Power function implementations for builtin integers *)


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


(**open Builtin
open Basic_arithmetics*)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  let rec pow_rec x n a =
    if n = 0
    then a
    else pow_rec x (n-1) (x*a)
  in pow_rec x n 1;;



(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  if n = 0
  then 1
  else let rec  power_rec x n =
    if n = 0
    then 1
    else x * power_rec x (n-1)
       in power_rec x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec power_rec x r = function
      0 -> r
     |n when n mod 2 = 1 -> power_rec (modulo (x*x) m) (modulo (r*x) m) (n/2)
     |n -> power_rec (modulo (x*x) m) r (n/2)
  in power_rec (modulo x m) 1 n;;


(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if x mod p <> 0
  then let r = modulo n (p-1)
       in mod_power x r p
  else mod_power x n p;;
