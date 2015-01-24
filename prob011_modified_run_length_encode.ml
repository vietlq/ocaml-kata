(*
Problem 11: Write modified run-length encoding.

type 'a rle =
    | One of 'a
    | Many of int * 'a;;
*)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode l =
    let merge count x acc = if count > 1
        then Many (count, x)::acc
        else One x::acc
    in let rec aux count acc = function
        | [] -> acc
        | [x] -> merge (count+1) x acc
        | h::(x::_ as t) -> if h = x
            then aux (count+1) acc t
            else aux 0 (merge (count+1) h acc) t
    in List.rev (aux 0 [] l);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
