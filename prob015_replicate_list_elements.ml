(*
Problem 15: Replicate the elements of a list.
*)

let replicate l n =
    let rec make_list acc x n =
        if n < 1 then acc
        else make_list (x::acc) x (n-1)
    in let rec aux acc n = function
        | [] -> acc
        | h::t -> aux (make_list acc h n) n t
    in List.rev (aux [] n l);;

replicate [1;2;3] 3;;
