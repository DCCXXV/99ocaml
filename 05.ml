(* reverse a list *)

let rec rev list =
    match list with
    | [] -> []
    | hd :: tl -> rev tl @ [ hd ];;

(* better sol: only :: and tail recursive *)
let rev list =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] list;;
