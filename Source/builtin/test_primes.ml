(** Testing for primality *)

(**open Builtine
open Basic_arithmetics
open Power*)
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








(** Deterministic primality test *)
let is_prime n =
  let d2 = n / 2 in
  let rec is_prime_rec n d =
    if d > d2
    then true
    else if n mod d = 0
         then false
         else is_prime_rec n (d+1)
  in is_prime_rec n 2;;

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  if test_seq = []
  then false
  else let rec is_pseudo_prime_rec p test_seq =
         match test_seq with
           [] -> true
          |elm::l -> if (mod_power elm (p-1) p) = 1 || p = 2
                     then is_pseudo_prime_rec p l
                     else false
       in is_pseudo_prime_rec p test_seq;;
