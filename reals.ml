#require "zarith";;

(*
let ( * ) = mult_big_int;;
let ( + ) = add_big_int;;
let ( - ) = sub_big_int;;
let ( / ) = div_big_int;;
let ( =~) = eq_big_int;;

print_endline "hello"
*)

print_string(Z.to_string Z.one)
