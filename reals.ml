#require "zarith";;

let ( * ) = Z.mul;;
let ( + ) = Z.add;;
let ( - ) = Z.sub;;
let ( / ) = Z.div;;
let (~$) = Z.of_int;;

print_string (Z.to_string (~$5 * ~$10))
