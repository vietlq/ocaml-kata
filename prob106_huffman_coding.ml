type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;

type direction = L | R;;

let sample_tree = Node (Leaf 3, Node (Leaf 4, Leaf 5));;

let code_tree = Node (
    Node (
        Node (Leaf 'a', Leaf 'b'),
        Node (Leaf 'c', Leaf 'd')
    ),
    Leaf 'e');;

let rec map f = function | Leaf v -> Leaf (f v)
    | Node (l, r) -> Node (map f l, map f r)

let traverse t = let rec aux path acc = function
    | Leaf v -> (v, List.rev path) :: acc
    | Node (l, r) -> (aux (0 :: path) acc l) @ (aux (1 :: path) acc r)
    in aux [] [] t;;

let rec subtree dl t = match dl, t with
    | [], _ -> Some t
    | L :: tdl, Node (l, _) -> subtree tdl l
    | R :: tdl, Node (_, r) -> subtree tdl r
    | _, _ -> None;;

let rec print_direction = function [] -> ()
    | L :: tl -> print_char 'L' ; print_string " " ; print_direction tl
    | R :: tl -> print_char 'R' ; print_string " " ; print_direction tl;;

let decode dl t = let rec aux dl1 t1 acc =
    match dl1, t1 with
        | L :: tdl, Node (l, _) -> aux tdl l acc
        | R :: tdl, Node (_, r) -> aux tdl r acc
        | [], Leaf v -> Some (List.rev (v :: acc))
        | dl2, Leaf v -> aux dl2 t (v :: acc)
        | _, _ -> None
    in aux dl t [];;

decode [L;L;R;R;R] code_tree;;

let bindecode dl t = let rec aux dl1 t1 acc =
    match dl1, t1 with
        | 0 :: tdl, Node (l, _) -> aux tdl l acc
        | 1 :: tdl, Node (_, r) -> aux tdl r acc
        | [], Leaf v -> Some (List.rev (v :: acc))
        | dl2, Leaf v -> aux dl2 t (v :: acc)
        | _, _ -> None
    in aux dl t [];;

bindecode [0;0;1;1;1] code_tree;;

let huffman_coding l =
    let join_tree t1 t2 = Node (fst t1, fst t2), (snd t1) +. (snd t2)
    and comp_snd a b = compare (snd a) (snd b)
    and tlist = List.rev_map (fun (a, b) -> Leaf a, b) l
    in let sort_tree_list = List.stable_sort comp_snd
    in let rec merge_tree_list = function
        | [] -> failwith "No love for empty lists!"
        | [x] -> fst x
        | h1 :: h2 :: tl -> let newlist = (join_tree h1 h2) :: tl
            in merge_tree_list (sort_tree_list newlist)
    in merge_tree_list (sort_tree_list tlist);;

huffman_coding ['a', 0.3; 'b', 0.1; 'c', 0.2; 'd', 0.3];;
huffman_coding [6, 0.2; 8, 0.3; 4, 0.1; 9, 0.4];;

let byte_freq astr =
    let bmap = Array.make 256 0.
    in let it_func c = bmap.(Char.code c) <- bmap.(Char.code c) +. 1.
    in let () = String.iter it_func astr
    in let fold_func (k, total, acc) freq =
        if freq > 0. then (k + 1, total +. freq, (Char.chr k, freq) :: acc)
        else (k + 1, total, acc)
    in let size, total, freqs = Array.fold_left fold_func (0, 0., []) bmap
    in List.rev_map (fun (c, v) -> (c, v /. total)) freqs;;

let freqs = byte_freq "A programmer started to cuss
Because getting to sleep was a fuss
As she lay there in bed
Looping 'round in her head
was: while(!asleep()) sheep++

Source: http://stackoverflow.com/a/245307/128028";;

huffman_coding freqs;;
