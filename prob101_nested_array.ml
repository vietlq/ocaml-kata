(*
Problem 101: Create a nested array from an array satisfying:

[1;2;3;4] -> [[1;2;3;4]; [2;3;4]; [3;4]; [4]; []]
*)

(*
Basically the list is appened to its tail and
continue the process until the tail is empty
*)

(* This solution is elegant but not tail-recursive *)
let rec nested = function
    [] -> [[]]
    | _::t as l -> l::(nested t);;

(* This solution is elegant too and tail-recursive *)
let nested2 l =
    let rec aux acc = function
        [] -> acc
        | _::t as l -> aux (l::acc) t
    in List.rev_append (aux [] l) [[]];;

(* This one uses List.fold_left & no direct recursion *)
let nested3 l =
    let prepender a b = (b::List.hd a)::a
    in List.fold_left prepender [[]] (List.rev l)

let sample0 = []
let sample1 = [1]
let sample2 = [1; 2]
let sample3 = [1; 2; 3]
let sample4 = [1; 2; 3; 4]
