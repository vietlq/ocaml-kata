(*
Problem 17: Split a list into two parts; the length of the first part is given.
If the length of the first part is longer than the entire list,
then the first part is the list and the second part is empty.
*)

let split l n =
    let rec aux acc n = function
        [] -> (List.rev acc, [])
        | h::t as l -> if n < 1 then (List.rev acc, l)
            else aux (h::acc) (n-1) t
    in aux [] n l;;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

split ["a";"b";"c";"d"] 5;;
