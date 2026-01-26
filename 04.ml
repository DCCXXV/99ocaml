(* length of a list *)

let rec length list =
    match list with
    | [] -> 0
    | _ :: tl -> 1 + length tl;;

(* better sol (tail recursive) *)
let length list =
    let rec aux n list =
        match list with
        | [] -> n
        | _ :: tl -> aux (n + 1) tl
    in
    aux 0 list;;
