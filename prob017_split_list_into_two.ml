(*
Problem 17: Split a list into two parts; the length of the first part is given.
*)

let split l n =
    let rec aux acc n = function
        [] -> (List.rev acc, [])
        | h::t -> if n < 1 then (List.rev acc, t)
            else aux (h::acc) (n-1) t
    in aux [] n l;;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

split ["a";"b";"c";"d"] 5;;
