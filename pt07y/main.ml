module Graph : sig
  type t

  val create : nodes:int -> edges:int -> t

  val is_connected : t -> bool
end = struct
  type t = int list array

  let create ~nodes ~edges =
    let arr = Array.make nodes [] in
    let rec loop1 = function
      | i when i = edges -> arr
      | i ->
          let x = read_int () in
          let y = read_int () in
          arr.(x - 1) <- (y - 1) :: arr.(x - 1) ;
          arr.(y - 1) <- (x - 1) :: arr.(y - 1) ;
          loop1 (i + 1)
    in
    loop1 0

  let is_connected t =
    let sizes = List.map (fun l -> List.fold_left (fun x y -> x + y) 0 l) in
    let target_size = List.fold_left ( + ) sizes in
    let h = Hashtbl.create 10000 in
    false
end

type t = int list list

let () =
  let nodes = read_int () in
  let edges = read_int () in
  let s =
    match edges with
    | n when n = nodes - 1 -> "YES"
    | _ -> ( match Graph.is_connected t with true -> "YES" | false -> "NO" )
  in
  print_endline s
