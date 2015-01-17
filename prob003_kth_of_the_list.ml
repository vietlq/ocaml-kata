(* Given a list, return the k-th element of it. If the input is invalid, return None *)

let rec at k = function
  [] -> None
  | h::t -> if k < 1 then None
    else if k = 1 then Some h
    else at (k-1) t;;

let sample0 = [];;
let sample1 = [1];;
let sample2 = [1; 2];;
let sample3 = [1; 2; 3];;
let sample4 = [1; 2; 3; 4];;

at 3 sample0;;
at 3 sample1;;
at 3 sample2;;
at 3 sample3;;
at 3 sample4;;

