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
