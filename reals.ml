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
      (* Built-in values *)
      let zero n = Q.zero
      let one n  = Q.one
      let e n = summation Z.zero n
        (function i -> Q.inv (Q.of_bigint (factorial i)))

      (* Casting between types *)
      let of_int    x n = Q.of_int x
      let of_bigint x n = Q.of_bigint x
      let to_string x n = Q.to_string (x n)
      (* TODO: implement long division with bignums for more precision *)
      let to_float x n = (Z.to_float (Q.num (x n))) /. (Z.to_float (Q.den (x n)))

      (* ------- UNARY OPERATIONS -------- *)
      (* Negation *)
      let neg x n = Q.neg (x n)
      (* Inversion *)
      let inv x n = Q.inv (x n)

      (* ------- BINARY OPERATIONS -------- *)
      (* Addition *)
      let add a b n =
        let two_n = (Z.mul ~$2 n) in
        Q.add (a two_n) (b two_n)
      (* Subtraction *)
      let sub a b = add a (neg b)
      (* Multiplication *)
      let mul a b n =
        let bound x =
          let two  = Q.of_bigint ~$2 in
          Z.succ (Q.to_bigint (Q.add (Q.abs (x Z.one)) two)) in
        let k = max (bound a) (bound b) in
        let two_k_n = Z.mul (Z.mul ~$2 k) n in
        Q.mul (a two_k_n) (b two_k_n)
      (* Division *)
      let div a b = mul a (inv b)

      (* Convenience methods *)
      let println_decimal x n = print_string "\n", print_float (to_float x n)
    end;;

(* Faster Reals arithmetic *)
let ( * ) = R.mul;;
let ( + ) = R.add;;
let ( - ) = R.sub;;
let ( / ) = R.div;;

(* Tests *)
(*
print_string "approximate value of e: ";;
R.println_decimal R.e ~$100;;
print_string "adding one to e: ";;
R.println_decimal (R.e + R.one) ~$10;;
print_string "multiplying e by one: ";;
R.println_decimal (R.e * R.one) ~$1;;
*)