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
    in List.fold_left prepender [[]] (List.rev l);;

(* For the sake of completeness, here's the version using List.fold_right*)
let nested4 l =
    let prepender a b = (a::List.hd b)::b
    in List.fold_right prepender l [[]];;

(* The versions nested2 & nested3 are the most efficient *)
(* The version 1 nested is the most elegant *)

let sample0 = []
and sample1 = [1]
and sample2 = [1; 2]
and sample3 = [1; 2; 3]
and sample4 = [1; 2; 3; 4];;

nested sample0;;
nested sample1;;
nested sample2;;
nested sample3;;
nested sample4;;

nested2 sample0;;
nested2 sample1;;
nested2 sample2;;
nested2 sample3;;
nested2 sample4;;

nested3 sample0;;
nested3 sample1;;
nested3 sample2;;
nested3 sample3;;
nested3 sample4;;

nested4 sample0;;
nested4 sample1;;
nested4 sample2;;
nested4 sample3;;
nested4 sample4;;
