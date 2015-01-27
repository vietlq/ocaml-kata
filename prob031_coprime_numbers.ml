(*
Problem 31: Determine whether two positive integer numbers are coprime.
*)

let coprime x y =
    let rec gcd2 a b =
        if b = 0 then a else gcd2 b (a mod b)
    in (gcd2 x y) = 1;;

coprime 0 0;;
coprime 0 1;;
coprime 1 0;;
coprime 11 17;;
coprime 19 27;;
coprime 20536 7826;;
coprime 16 9;;
