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

