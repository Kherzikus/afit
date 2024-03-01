(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)


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






(**open Scalable*)

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB =
  from_int (gcd (to_int bA) (to_int bB));;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
  let (a, b, c) = bezout (to_int bA) (to_int bB)
in ((from_int a), (from_int b), (from_int c));;
