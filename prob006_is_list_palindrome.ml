(*
Find out whether a list is a palindrome
*)

(*
The simplest way:

let is_palindrome list =
    list = List.rev list
*)

(*
If you want to challenge yourself...
*)
let palindrome l =
    let reverse alist = 
        let rec aux acc = function
            | [] -> acc
            | h::t -> aux (h::acc) t
        in aux [] alist
    in
    let rec are_arrays_equal a1 a2 = match a1, a2 with
        [], [] -> true
        | _::_, [] -> false
        | [], _::_ -> false
        | h1::t1, h2::t2 -> if h1 = h2 then are_arrays_equal t1 t2 else false
    in are_arrays_equal l (reverse l)

let sample0 = [];;
let sample1 = [1];;
let sample2 = [1; 2];;
let sample3 = [1; 2; 1];;
let sample4 = [1; 2; 2; 1];;

palindrome sample0;;
palindrome sample1;;
palindrome sample2;;
palindrome sample3;;
palindrome sample4;;

