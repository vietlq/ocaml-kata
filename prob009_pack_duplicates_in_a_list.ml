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

(*
Suggested by http://ocaml.org/learn/tutorials/99problems.html
Not so elegant as mine but has an interesting pattern:
    a :: (b :: _ as t)
*)

let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;
