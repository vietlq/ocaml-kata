(*
Problem 11: Write modified run-length encoding.

type 'a rle =
    | One of 'a
    | Many of int * 'a;;
*)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode l =
    (* Utility function to merge the accumulator
        when the count of a value is known *)
    let merge count x acc = if count > 1
        then Many (count, x)::acc
        else One x::acc
    (* The actual recursive function *)
    in let rec aux count acc = function
        (* When the list is empty, return the accumulator *)
        | [] -> acc
        (* When processing the last element of the list, merge *)
        | [x] -> merge (count+1) x acc
        (* When the list has at least 2 elements,
            compare the head and the next *)
        | h::(x::_ as t) -> if h = x
            (* If forward-looking is positive,
                keep going, don't merge yet *)
            then aux (count+1) acc t
            (* If forward-looking is negative,
                do the merge and move next *)
            else aux 0 (merge (count+1) h acc) t
    in List.rev (aux 0 [] l);;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
