(*
Problem 16: Drop every N'th element from a list.
*)

let drop l n =
    let rec aux acc count n = function
        | [] -> acc
        | h::t -> if count = n
            (* Skip every N-th element, reset the counter *)
            then aux acc 1 n t
            else aux (h::acc) (count+1) n t
    (* If the n < 1, return the whole list back *)
    in let helper l n = if n < 1 then l
        (* If n = 1, every element is removed *)
        else if n = 1 then []
        (* Otherwise, do the counting! *)
        else aux [] 1 n l
    in List.rev (helper l n);;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

(*
The page http://ocaml.org/learn/tutorials/99problems.html
offers an elegant but non-tail-recursive solution
*)
let drop_not_tail_recursive list n =
    let rec aux i = function
        | [] -> []
        | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
    aux 1 list;;
