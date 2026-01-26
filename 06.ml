(* palindrome *)

let is_palindrome list =
    let old_list = list in
        let rec aux acc = function
            | [] -> acc
            | hd :: tl -> aux (hd :: acc) tl
        in
        aux [] list = old_list;;

(* alt sol: using rev from 05.ml *)
let rev list =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] list;;

let is_palindrome list =
    list = rev list;;
