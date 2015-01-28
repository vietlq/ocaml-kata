(*
Problem 22: Create a list containing all integers within a given range. (easy)

If first argument is smaller than second, produce a list in decreasing order.

# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
# range 9 4;;
- : int list = [9; 8; 7; 6; 5; 4]
*)

let range a b =
    let rec aux x y acc = if x = y then x::acc
        else if x > y then aux (x-1) y (x::acc)
        else aux (x+1) y (x::acc)
    in aux b a [];; (* Save from invoking List.rev *)

range 4 9;;
range 9 4;;
