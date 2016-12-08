#require "zarith";;

(* Faster bignum initialization *)
let (~$) = Z.of_int;;

(* Convenience methods for Integers *)
let max a b = if Z.gt a b then a else b;;
let rec powZ x n = if Z.(leq n zero) then Z.one else
  Z.mul x (powZ x (Z.pred n));;
let rec factorial x = Z.(if leq x zero then one else
  mul x (factorial (pred x)));;
let ifact x =
  let rec ifactrec bound p k = if Z.lt bound p then k else
    ifactrec bound Z.(mul p (succ k)) (Z.succ k) in
  ifactrec x Z.one Z.one

(* Convinience methods for Rationals *)
let q_of_zs a b = Q.(div (of_bigint a) (of_bigint b));;
let rec powQ x n = if Z.(leq n zero) then Q.one else
  Q.mul x (powQ x (Z.pred n));;
let rec summation first last f = if Z.gt first last then Q.zero else
  Q.add (f first) (summation (Z.succ first) last f);;

(* Real Numbers *)
module R =
    struct
      type t = Z.t -> Q.t

      (* Built-in values *)
      let zero n = Q.zero
      let one n  = Q.one
      let e n = summation Z.zero (ifact (Z.mul ~$2 n))
        (function i -> Q.(inv (of_bigint (factorial i))))
      (* Something is fishy with pi, look into this *)
      let pi n = summation Z.zero (Z.mul ~$500 n)
        (function k ->
          let four_k = (Z.mul ~$4 k) in
          Q.div (Q.of_bigint ~$8)
            (Q.of_bigint (Z.mul (Z.succ four_k) (Z.add four_k ~$3)))
        )

      (* Casting between types *)
      let of_int      x n = Q.of_int x
      let of_bigint   x n = Q.of_bigint x
      let of_rational x n = x
      let to_string x n = Q.to_string (x n)
      let to_float  x n = (Z.to_float (Q.num (x n))) /. (Z.to_float (Q.den (x n)))
      (* Useful for printing to a certain number of decimal digits *)
      let to_decimal x digits =
        let n = powZ ~$10 Z.(add (of_int digits) ~$1) in
        let characteristic = Q.to_bigint (x n) in
        let size_of_digits = powQ (Q.of_int 10) (Z.of_int digits) in
        let scaled_up = Q.(to_bigint (mul (x n) size_of_digits)) in
        let sign = if Z.(equal characteristic zero) && Z.(lt scaled_up zero)
          then "-" else "" in
        let mantissa = if Z.(equal characteristic zero) then Z.abs scaled_up else
          Z.rem scaled_up (Z.mul characteristic (Q.to_bigint size_of_digits)) in
        let mantissa_str = Z.to_string mantissa in
        let padding = String.make (digits - (String.length mantissa_str)) '0' in
        sign^(Z.to_string characteristic)^"."^padding^mantissa_str

      (* ------- UNARY OPERATIONS -------- *)
      (* Negation *)
      let neg x n = Q.neg (x n)
      (* Inversion *)
      let inv x n = Q.inv (x n)
      (* Cosine *)
      let cos x n = summation Z.one n
        (function k ->
          let sign = Q.of_bigint Z.(if is_odd k then one else minus_one) in
          let two_k_minus_two = Z.(sub (mul ~$2 k) ~$2) in
          Q.mul sign (Q.div
            (powQ (x n) two_k_minus_two)
            (Q.of_bigint (factorial two_k_minus_two))
          )
        )

      (* ------- BINARY OPERATIONS -------- *)
      (* Addition *)
      let add a b n =
        let two_n = Z.mul ~$2 n in
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
      (* Square root *)
      let sqrt x n =
        let accel_n = Z.pow n 10 in (* TODO: figure out right accel *)
        let two_accel_n = Z.mul ~$2 accel_n in
        let x_n = x accel_n in
        let a = Z.div (Z.mul (Q.num x_n) two_accel_n) (Q.den x_n) in
        q_of_zs (Z.sqrt (Z.mul a two_accel_n)) two_accel_n

      (* Convenience methods *)
      let println_decimal x n = print_string "\n", print_float (to_float x n)
    end;;