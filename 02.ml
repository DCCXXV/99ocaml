(* last two elements of a list *)

let rec last_two list =
    match list with
    | [] -> None
    | [ x ] -> None
    | [ x; y ] -> Some (x, y)
    | _ :: tl -> last_two tl;;

(* better sol *)
let rec last_two' list =
    match list with
    | [] | [_] -> None
    | [ x; y ] -> Some (x, y)
    | _ :: tl -> last_two tl;;
