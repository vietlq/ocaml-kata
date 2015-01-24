(*
Flatten a nested list defined by:

type 'a node =
    | One of 'a 
    | Many of 'a node list;;

*)

type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let flatten l =
    (* Iterate through the list by recursion *)
    let rec aux acc = function
        (* If the list is empty, return the accumulator *)
        | [] -> acc
        (* If the element is a value, prepend it to the accumulator, and continue *)
        | One single::t -> aux (single::acc) t
        (* If the element is a value, recurse into it, use the result and continue *)
        | Many alist::t -> aux (aux acc alist) t
    in List.rev (aux [] l);;

flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

