#require "zarith";;
#use "R.ml";;

(* Faster Reals arithmetic *)
let ( * ) = R.mul;;
let ( + ) = R.add;;
let ( - ) = R.sub;;
let ( / ) = R.div;;

(* Faster input of integers *)
let one   = R.one;;
let two   = one+one;;
let three = two+one;;
let four  = three+one;;
let five  = four+one;;
let six   = five+one;;
let seven = six+one;;
let eight = seven+one;;
let nine  = eight+one;;
let ten   = nine+one;;

(* Inline to_decimal. Usage:
 *     utop # R.sqrt two + R.e =~ 5;;
 *     - : string = "4.13249"
 * is a string containing [root 2 plus e] to 10 decimal places
 *)
let (=~) = R.to_decimal;;
