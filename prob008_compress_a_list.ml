(*
Eliminate consecutive duplicates of list elements. (medium)
*)

let compress l =
    let rec aux acc = function
        | [] -> acc
        | h::t -> match acc with
            | [] -> aux [h] t
            | h1::_ -> if h = h1
                then aux acc t
                else aux (h::acc) t
    in List.rev (aux [] l);;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

(*
Elegant but not tail-recursive.
Suggested by http://ocaml.org/learn/tutorials/99problems.html
This has an interesting pattern for the tail:
    a :: (b :: _ as t)
*)

let rec compress_not_tail_rec = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;
