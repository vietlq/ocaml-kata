(*
Problem 30: Determine the greatest common divisor of two positive integer numbers.
*)

let gcd a b =
    let rec aux x y =
        let residue = x mod y
        in if residue = 0 then y
        else aux y residue
    in match a, b with
        | 0, 0 -> failwith "Both arguments cannot be 0"
        | 0, _ -> b
        | _, 0 -> a
        | a, b -> aux a b;;

gcd 0 0;;
gcd 0 1;;
gcd 1 0;;
gcd 11 17;;
gcd 19 27;;
gcd 20536 7826;;

(*
The page http://ocaml.org/learn/tutorials/99problems.html
offers a very elegant solution (and allows both to be 0, which is wrong!)
*)
let rec gcd2 a b =
    if b = 0 then a else gcd2 b (a mod b);;
