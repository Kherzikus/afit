(** Encoding Strings *)


let power x n =
  if n = 0
  then 1
  else let rec  power_rec x n =
    if n = 0
    then 1
    else x * power_rec x (n-1)
       in power_rec x n;;


(**open Builtin
open Basic_arithmetics
open Power*)

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits = 0

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits = ""
