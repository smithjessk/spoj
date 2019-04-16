let f x y =
  let o = if (x < 0 || y < 0) then None else (
    if (y = x) then (
      if (x mod 2 = 0) then Some (2 * x) else Some (2 * x - 1)
    ) else if (y = x - 2) then (
      if (x mod 2 = 0) then Some (2 * x - 2) else Some (2 * x - 3)
    ) else None
  ) in
  match o with
  | None -> "No Number"
  | Some n -> string_of_int n

let () =
  let rec g = function
    | 0 -> ()
    | n ->
      let s = read_line () in
      print_endline (Scanf.sscanf s "%d %d" f);
      g (n - 1) in
  g (read_int ())
