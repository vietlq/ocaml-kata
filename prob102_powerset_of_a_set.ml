(*
Problem 102: Given a set, produce its powerset P(S).

S = [1; 2; 3]

P(S) = [[1; 2; 3]; [1; 2]; [1; 3]; [1]; [2; 3]; [2]; [3]; []]

Note: The order is not important when producing P(S), becase it's a set.
*)

(* Tail-recursive solution *)
let powerset s =
    let prepend l x = List.fold_left (fun a b -> (x::b)::a) [] l
    in let fold_func a b = List.rev_append (prepend a b) a
    in List.fold_left fold_func [[]] (List.rev s);;

(* Helper function to generate a list from a range *)
let range a b =
    let rec aux x y acc = if x = y then x::acc
        else if x > y then aux (x-1) y (x::acc)
        else aux (x+1) y (x::acc)
    in aux b a [];; (* Save from invoking List.rev *)

powerset (range 1 3);;
powerset (range 1 10);;
powerset (range 1 22);;
