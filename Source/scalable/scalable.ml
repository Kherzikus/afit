(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

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










(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  let rec from_int_rec x =
    match x with
      n when n < 1 -> []
     |n when n = 1 -> 1::from_int_rec (x/2)
     |_ -> (x mod 2)::from_int_rec (x/2)
  in if sign x < 0
     then 1::from_int_rec (-x)
     else 0::from_int_rec x;;



(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
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



(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = ()



(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB =
  if to_int nA > to_int nB
  then 1
  else if to_int nA < to_int nB
  then (-1)
  else 0;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB =
  if to_int nA > to_int nB
  then true
  else false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
  if to_int nA < to_int nB
  then true
  else false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
  if to_int nA >= to_int nB
  then true
  else false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
  if to_int nA <= to_int nB
  then true
  else false;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  if to_int bA > to_int bB
  then 1
  else if to_int bA < to_int bB
  then (-1)
  else 0;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
  if to_int bA > to_int bB
  then true
  else false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
  if to_int bA < to_int bB
  then true
  else false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB =
  if to_int bA >= to_int bB
  then true
  else false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
  if to_int bA <= to_int bB
  then true
  else false;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  if to_int bA < 1
  then -1
  else 1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  match bA with
    e::l when e = 1 -> 0::l
  |_ -> bA;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  from_int ((to_int nA) + (to_int nB));;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  from_int ((to_int nA) - (to_int nB));;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB =
  from_int ((to_int bA) + (to_int bB));;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
  from_int ((to_int bA) - (to_int bB));;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
  from_int ((to_int bA) * (to_int bB));;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
  from_int ((to_int bA) / (to_int bB));;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB =
  from_int ((to_int bA) mod (to_int bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =
  (from_int (quot (to_int bA) (to_int bB)), from_int (modulo (to_int bA) (to_int bB)));;
