(** Generating prime bitarrays *)

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













let is_prime_a n =
  let d2 = n / 2 in
  let rec is_prime_rec n d =
    if d > d2
    then true
    else if n mod d = 0
         then false
         else is_prime_rec n (d+1)
  in is_prime_rec n 2;;

let is_prime n =
  if (is_prime_a (to_int n)) = true
  then true
  else false;;











let init_eratosthenes_a n =
  let rec init_eratosthenes_rec n i =
    match i with
    |i when i > n -> []
    |1 -> 2::(init_eratosthenes_rec n (i+2))
    |_ -> i::(init_eratosthenes_rec n (i+2))
  in init_eratosthenes_rec n 1;;

let eratosthenes_a n =
  let rec eratosthenes_rec n i =
    match i with
    |i when i > n -> []
    |1 -> 2::(eratosthenes_rec n (i+2))
    |i when is_prime_a i = true -> i::(eratosthenes_rec n (i+2))
    |_ -> eratosthenes_rec n (i+2)
  in eratosthenes_rec n 1;;








let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

let read_list_primes file = []

let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t








let double_primes_a limit isprime =
  let rec double_primes_rec = function
      [_] | [] -> []
      |e::l1 -> let impair = e*2 + 1
                in if isprime impair = true
                   then (e, impair)::double_primes_rec l1
                   else double_primes_rec l1
  in double_primes_rec (eratosthenes_a limit);;

let twin_primes_a limit isprime =
  let rec twin_primes_rec = function
      [_] | [] -> []
      |e::l1 -> let pair = e+2 in
                if isprime pair
                then (e, pair)::twin_primes_rec l1
                else twin_primes_rec l1
in twin_primes_rec (eratosthenes_a limit);;





























(**open Scalable
open Scalable_basic_arithmetics*)

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  let l = init_eratosthenes_a (to_int n) in
  let rec aux l =
    match l with
      [] -> []
     |e::l1 -> (from_int e)::(aux l1)
  in aux l;;

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let l = eratosthenes_a (to_int n) in
  let rec aux l =
    match l with
      [] -> []
     |e::l1 -> (from_int e)::(aux l1)
  in aux l;;

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  let rec double_primes_rec = function
      [_] | [] -> []
      |e::l1 -> let impair = (to_int e)*2 + 1
                in if isprime (from_int impair) = true
                   then (e, (from_int impair))::double_primes_rec l1
                   else double_primes_rec l1
  in double_primes_rec (eratosthenes limit);;


(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec twin_primes_rec = function
      [_] | [] -> []
      |e::l1 -> let pair = (to_int e)+2 in
                if isprime (from_int pair)
                then (e, (from_int pair))::twin_primes_rec l1
                else twin_primes_rec l1
  in twin_primes_rec (eratosthenes limit);;
