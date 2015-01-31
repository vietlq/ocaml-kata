(*
Problem 18: Extract a slice from a list. (medium)

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).

# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let slice l first last =
    (* Pre-condition: first <= last *)
    let rec aux first last i acc = function
        [] -> acc
        | h::t -> if i > last then acc (* Terminate before reaching the end *)
            else if i >= first then aux first last (i+1) (h::acc) t
            else aux first last (i+1) acc t
    (* Handling of negative indices is not available yet *)
    in if (first < 0) || (last < 0) then []
    (* This is additional feature *)
    else if first <= last then List.rev (aux first last 0 [] l)
    (* Swap to satisfy pre-condition, but don't reverse the result *)
    else aux last first 0 [] l;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 2 = ["c"];;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"];;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 6 2 = ["g"; "f"; "e"; "d"; "c"];;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 66 = ["c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;

(*
The page http://ocaml.org/learn/tutorials/99problems.html
offered solution in terms of picking & dropping elements, which is handy as well.
There's no error handling. And their solution is non-recursive.
*)
let slice2 list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n-1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n-1) t
    in
    take (k - i + 1) (drop i list);;

(*
Another solution derived from function in Problem 17.
*)
let slice3 l first last =
    let split l n =
        let rec aux acc n = function
            [] -> (List.rev acc, [])
            | h::t as l -> if n < 1 then (List.rev acc, l)
                else aux (h::acc) (n-1) t
        in aux [] n l
    in fst (split (snd (split l first)) (last - first + 1))
