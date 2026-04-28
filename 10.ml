(* run-length encoding *)

(* 09.ml *)
let pack list =
  let rec aux current acc = function
    | [] -> []
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
    	if a = b then aux (a :: current) acc t
     	else aux [] ((a  :: current) :: acc) t in
    List.rev (aux [] [] list);;

let encode_one list = (List.length list, List.nth list 0);;

let rec encode_aux = function
	| [] -> []
	| hd :: tl -> encode_one hd :: encode_aux tl;;

let encode list = pack list |> encode_aux;;

(* provided solutions: *)

(* w/o using pack *)
let encode list =
	let rec aux count acc = function
	| [] -> []
	| [ x ] -> (count + 1, x) :: acc
	| a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
							else aux 0 ((count + 1, a) :: acc) t in
List.rev (aux 0 [] list);;

(* using it *)
let pack list =
	let rec aux current acc = function
		| [] -> []
		| [x] -> (x :: current) :: acc
		| a :: (b :: _ as t) ->
			if a = b then aux (a :: current) acc t
         	else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;

let encode list =
	List.map (fun l -> (List.length l, List.hd l)) (pack list);;
