(*
Reverse the list
*)

let reverse l = 
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (h::acc) t
    in aux [] l

let sample0 = [];;
let sample1 = [1];;
let sample2 = [1; 2];;
let sample3 = [1; 2; 3];;
let sample4 = [1; 2; 3; 4];;

reverse sample0;;
reverse sample1;;
reverse sample2;;
reverse sample3;;
reverse sample4;;

