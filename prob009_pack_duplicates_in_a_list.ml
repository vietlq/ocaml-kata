(*
Pack consecutive duplicates of list elements into sublists
*)

let pack l =
    let rec aux acc = function
        | [] -> acc
        | h::t -> match acc with
            | [] -> aux ([h]::acc) t
            | h1::t1 -> if h = (List.hd h1)
                then aux ((h::h1)::t1) t
                else aux ([h]::acc) t
    in List.rev (aux [] l);;

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
