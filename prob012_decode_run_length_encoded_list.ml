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

(decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]) = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
