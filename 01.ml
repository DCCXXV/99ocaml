(* tail of a list *)

let rec last list =
    match list with
    | [] -> None
    | [ x ] -> Some x
    | _ :: tl -> last tl;;
