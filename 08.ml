(* eliminate duplicates *)

let rec compress = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: tl -> if x = y then compress (y :: compress tl) else x :: y :: compress tl;;

(* better/provided sol *)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

(*
    NOTE:
    Less redundant work, a :: (b :: _ as t) defines a case where there are at least 2 elements:
    a         -> head
    (b :: _ ) -> which can also be called t to refer as a whole

    smaller -> smaller is covers every other case ([] -> [] and [ x ] -> [ x ] in a cleaner way
*)
