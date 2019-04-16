(**

formula = the number of (l, w) s.t. l * w <= number of squares


1 -> 1
2 -> (1, 1), (1, 2)
3 -> (1, 1), (1, 2), (1, 3)
6 -> (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), (2, 3)


*)

let f num_sq =
  let rec loop1 l ~cnt =
    let rec loop2 w ~cnt =
      match l * w <= num_sq with
      | true -> loop2 (w + 1) ~cnt:(cnt + 1)
      | false -> cnt
    in
    let cnt = loop2 l ~cnt in
    match (l + 1) * (l + 1) > num_sq with
    | true -> cnt
    | false -> loop1 (l + 1) ~cnt in
  loop1 1 ~cnt:0

let () =
  Printf.printf "%d\n" (f (read_int()))
