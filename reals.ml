#require "zarith";;

let ( * ) = Z.mul;;
let ( + ) = Z.add;;
let ( - ) = Z.sub;;
let ( / ) = Z.div;;

(* Integers *)
let (~$) = Z.of_int;;
print_string (Z.to_string (Z.mul ~$5 ~$10));;

print_string "\n";;

(* Rationals *)
print_string (Q.to_string (Q.of_ints 3 2));;

print_string "\n";;

(* Convenience methode for Integers *)
(*
let rec factorial x = match x with
  | Z.zero -> Z.one
  | _ -> Z.mul x (Z.pred x)
*)

let rec factorial x = if Z.leq x Z.zero then Z.one else
  Z.mul x (factorial (Z.pred x));;

(* Convinience methods for Rationals *)
let rec summation first last f = if Z.equal first last then Q.zero else
  Q.add (f first) (summation (Z.succ first) last f);;

(* Real Numbers *)
module R =
    struct
      let zero = function n -> Q.zero
      let one = function n -> Q.one
      let e = function n ->
        summation Z.zero n (function i -> Q.inv (Q.of_bigint (factorial i)))
      let of_int x = function n -> Q.of_int x
      let of_bigint x n = Q.of_bigint x
      let to_string x n = Q.to_string (x n)
      let to_float x n = (Z.to_float (Q.num (x n))) /. (Z.to_float (Q.den (x n)))
      let print_decimal x n = print_float (to_float x n)
    end;;

print_string (R.to_string (R.of_int 5) (Z.of_int 100));;

print_string "\n";;

R.print_decimal R.e (Z.of_int 100);;

print_string "\n";;