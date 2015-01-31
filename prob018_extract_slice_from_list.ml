(*
Problem 18: Extract a slice from a list. (medium)

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).

# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"]
*)

let slice l first last =
    let rec aux first last i acc = function
        [] -> acc
        | h::t -> if i > last then acc
            else if i >= first then aux first last (i+1) (h::acc) t
            else aux first last (i+1) acc t
    (* Handling of negative indices is not available yet *)
    in if (first < 0) || (last < 0) then []
    (* This is additional feature *)
    else if first <= last then List.rev (aux first last 0 [] l)
    else aux last first 0 [] l;;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 2 = ["c"];;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"];;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 6 2 = ["g"; "f"; "e"; "d"; "c"];;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 66 = ["c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"];;
