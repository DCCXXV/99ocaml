(* n'th element of a list *)

let rec at n list =
    match list with
    | [] -> None
    | hd :: tl ->
        if n = 0 then
            Some hd
        else
            at (n-1) tl;;

(* better sol (just use if then else inline) *)
let rec at n list =
    match list with
    | [] -> None
    | hd :: tl -> if n = 0 then Some hd else at (n-1) tl;;
