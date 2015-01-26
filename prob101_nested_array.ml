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
    | _::t as l -> l::(nested t)
