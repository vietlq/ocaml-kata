(* Given a list, return the last element of it.
If the list is empty, return None *)

let rec last = function
  [] -> None
  | [x] -> Some x
  | _::l -> last l;;

let sample0 = [];;
let sample1 = [1];;
let sample2 = [1; 2];;
let sample3 = [1; 2; 3];;
let sample4 = [1; 2; 3; 4];;

last sample0;;
last sample1;;
last sample2;;
last sample3;;
last sample4;;

