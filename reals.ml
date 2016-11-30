#require "zarith";;

(* Faster bignum arithmetic *)
let ( * ) = Z.mul;;
let ( + ) = Z.add;;
let ( - ) = Z.sub;;
let ( / ) = Z.div;;
let (~$) = Z.of_int;;

(* Convenience methods for Integers *)
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
      (* TODO: implement long division with bignums for more precision *)
      let to_float x n = (Z.to_float (Q.num (x n))) /. (Z.to_float (Q.den (x n)))
      let println_decimal x n = print_string "\n", print_float (to_float x n)
    end;;

print_string "approximate value of e: ";;
R.println_decimal R.e ~$100;;