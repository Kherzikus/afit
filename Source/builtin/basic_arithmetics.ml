(** Basic arithmetics with builtin integers *)


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





(**open Builtin*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b =
  let (q, r) = div a b in
  if r = 0
  then b
  else gcd b r;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let rec bezout_rec a b uA vA uB vB =
    if (modulo a b) = 0
    then (uB, vB, b)
    else bezout_rec b (modulo a b) uB vB (uA - uB * (quot a b)) (vA - vB * (quot a b))
  in bezout_rec a b 1 0 0 1;;
