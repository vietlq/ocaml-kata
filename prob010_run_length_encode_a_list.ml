(*
Problem 10:
Run-length encoding of a list. 
*)

let run_length_encode l =
    let rec aux acc = function
        | [] -> acc
        | h::t -> match acc with
            | [] -> aux [(1, h)] t
            | (n, v)::t1 -> if h = v
                then aux ((n+1, v)::t1) t
                else aux ((1, h)::acc) t
    in List.rev (aux [] l);;

run_length_encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(*
Alternative solution provided by http://ocaml.org/learn/tutorials/99problems.html
can reduce the number of :: operations
*)
let encode list =
    let rec aux count acc = function
        | [] -> [] (* Can only be reached if original list is empty *)
        | [x] -> (count+1, x) :: acc
        | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
            else aux 0 ((count+1,a) :: acc) t in
    List.rev (aux 0 [] list);;
