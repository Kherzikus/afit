(** Generating primes *)

(**open Builtin
open Basic_arithmetics*)

let is_prime n =
  let d2 = n / 2 in
  let rec is_prime_rec n d =
    if d > d2
    then true
    else if n mod d = 0
         then false
         else is_prime_rec n (d+1)
  in is_prime_rec n 2;;



(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec init_eratosthenes_rec n i =
    match i with
    |i when i > n -> []
    |1 -> 2::(init_eratosthenes_rec n (i+2))
    |_ -> i::(init_eratosthenes_rec n (i+2))
  in init_eratosthenes_rec n 1;;

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  let rec eratosthenes_rec n i =
    match i with
    |i when i > n -> []
    |1 -> 2::(eratosthenes_rec n (i+2))
    |i when is_prime i = true -> i::(eratosthenes_rec n (i+2))
    |_ -> eratosthenes_rec n (i+2)
  in eratosthenes_rec n 1;;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec double_primes_rec = function
      [_] | [] -> []
      |e::l1 -> let impair = e*2 + 1
                in if isprime impair = true
                   then (e, impair)::double_primes_rec l1
                   else double_primes_rec l1
  in double_primes_rec (eratosthenes limit);;



(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec twin_primes_rec = function
      [_] | [] -> []
      |e::l1 -> let pair = e+2 in
                if isprime pair
                then (e, pair)::twin_primes_rec l1
                else twin_primes_rec l1
in twin_primes_rec (eratosthenes limit);;
