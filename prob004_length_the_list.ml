(*
Find the number of elements of a list. (easy)
*)

let length l =
    let rec length_aux n = function
        | [] -> n
        | h::t -> length_aux (n+1) t
    in length_aux 0 l

let sample0 = [];;
let sample1 = [1];;
let sample2 = [1; 2];;
let sample3 = [1; 2; 3];;
let sample4 = [1; 2; 3; 4];;

length sample0;;
length sample1;;
length sample2;;
length sample3;;
length sample4;;

(*
Non tail-recursive function:

let length = function
  | [] -> 0
  | h::t -> 1 + length t

*)

