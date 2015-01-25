(*
Problem 14: Duplicate the elements of a list.
*)

let duplicate l = 
    let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::h::acc) t
    in List.rev (aux [] l);;

duplicate ["a";"b";"c";"c";"d"];;

duplicate [1;2;3];;

(*
The page http://ocaml.org/learn/tutorials/99problems.html
offers a non-tail-recursive solution,
which is elegant and pleases the eye:

let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t;;
*)