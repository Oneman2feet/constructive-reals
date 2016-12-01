#require "zarith";;

(* Faster bignum initialization *)
let (~$) = Z.of_int;;

(* Convenience methods for Integers *)
let max a b = if Z.gt a b then a else b;;
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
      let add a b n =
        let two_n = (Z.mul ~$2 n) in
        Q.add (a two_n) (b two_n)
      let mul a b n =
        let bound x =
          let two  = Q.of_bigint ~$2 in
          Z.succ (Q.to_bigint (Q.add (Q.abs (x Z.one)) two)) in
        let k = max (bound a) (bound b) in
        let two_k_n = Z.mul (Z.mul ~$2 k) n in
        Q.mul (a two_k_n) (b two_k_n)
    end;;

(* Faster Reals arithmetic *)
let ( * ) = R.mul;;
let ( + ) = R.add;;
(*let ( - ) = R.sub;;
let ( / ) = R.div;;*)

print_string "approximate value of e: ";;
R.println_decimal R.e ~$100;;
print_string "adding one to e: ";;
R.println_decimal (R.e + R.one) ~$10;;
print_string "multiplying e by one: ";;
R.println_decimal (R.e * R.one) ~$1;;