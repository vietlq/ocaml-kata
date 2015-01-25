(*
Problem 12: Decode a run-length encoded list.

type 'a rle =
    | One of 'a
    | Many of int * 'a;;
*)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let decode l =
    let rec make_list n x acc =
        if n < 1 then acc
        else make_list (n-1) x (x::acc)
    in let folder a = function
        | One x -> x::a
        | Many (n, x) -> make_list n x a
    in List.rev (List.fold_left folder [] l);;

(decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

(*
This can be done without List.fold_left as well
*)
let decode2 l =
    let rec make_list n x acc =
        if n < 1 then acc
        else make_list (n-1) x (x::acc)
    in let rec aux acc = function
        | [] -> acc
        | One x::t -> aux (x::acc) t
        | Many (n, x)::t -> aux (make_list n x acc) t
    in List.rev (aux [] l);;

(decode2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
