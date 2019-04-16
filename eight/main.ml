let nums_less_than_1000 : (int, int) Hashtbl.t =
  let fast_suffix_cube n =
    let sq = n * n in
    let left = sq mod 1000 in
    left * n mod 1000
  in
  let h = Hashtbl.create 1000 in
  let rec loop = function
    | i when i >= 1000 -> h
    | i ->
        ( match fast_suffix_cube i with
        | 888 -> Hashtbl.add h (Hashtbl.length h) i
        | _ -> () ) ;
        loop (i + 1)
  in
  loop 1

let f n =
  let num_less_than_1000 = Hashtbl.length nums_less_than_1000 in
  let prefix =
    match n / num_less_than_1000 with
    | x when x = 0 -> ""
    | n -> Printf.sprintf "%d" n
  in
  let suffix =
    let rem = n mod num_less_than_1000 in
    Hashtbl.find nums_less_than_1000 rem |> Printf.sprintf "%d"
  in
  Printf.sprintf "%s%s" prefix suffix

let () =
  let t = int_of_string (read_line ()) in
  let rec loop = function
    | i when i = t -> ()
    | i ->
        let x = int_of_string (read_line ()) - 1 in
        print_endline (f x) ;
        loop (i + 1)
  in
  loop 0
