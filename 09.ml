(* pack consecutive duplicates *)

let rec group = function
  | [] -> []
  | [ x ] -> [ x ]
  | (ah :: _ as a) :: (bh :: _ as b) :: c -> if ah = bh then group ((a @ b) :: group c) else a :: group (b :: c)
  | other -> other;;

let rec pack = function
  | [] -> []
  | h :: t -> group ([ h ] :: pack t);;

(* provided sol: *)
let pack list =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
    	if a = b then aux (a :: current) acc t
     	else aux [] ((a  :: current) :: acc) t in
    List.rev (aux [] [] list);;
