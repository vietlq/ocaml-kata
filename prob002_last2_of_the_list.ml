(* Given a list, return the last 2 elements of it.
If the list is empty or has only 1 element, return None *)

let rec last_two = function
  [] | [_] -> None
  | [x; y] -> Some [x; y]
  | _::l -> last_two l;;

let sample0 = [];;
let sample1 = [1];;
let sample2 = [1; 2];;
let sample3 = [1; 2; 3];;
let sample4 = [1; 2; 3; 4];;

last_two sample0;;
last_two sample1;;
last_two sample2;;
last_two sample3;;
last_two sample4;;

