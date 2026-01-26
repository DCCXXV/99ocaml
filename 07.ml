(* flatten a list *)

type 'a node =
    | One of 'a
    | Many of 'a node list

let rec flatten = function
    | [] -> []
    | hd :: tl ->
        let rec aux acum = function
            | One x -> x :: acum
            | Many (hd :: tl) -> aux acum hd @ flatten tl
            | Many _ -> acum (* unreachable? *)
        in aux [] hd @ flatten tl;;

(* better sol: only using :: *)
let flatten list =
    let rec aux acc = function
        | [] -> acc
        | One x :: t -> aux (x :: acc) t
        | Many l :: t -> aux (aux acc l) t
    in
    List.rev (aux [] list);;
