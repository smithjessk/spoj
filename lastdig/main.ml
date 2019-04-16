let rec f a b =
  let rec g k =
    let lhs = int_of_float (float_of_int a ** float_of_int k) in
    match lhs mod 10 = a mod 10 with
    | true -> (k, b / k, b - (k * (b / k)))
    | false -> g (k + 1)
  in
  let k, m, z = g 2 in
  match m + z < b with
  | true -> f a (m + z)
  | false ->
      let res = float_of_int (a mod 10) ** float_of_int b in
      int_of_float res mod 10

let () =
  let t = int_of_string (read_line ()) in
  let rec loop i =
    if i < t then
      (Scanf.sscanf (read_line ()) "%d %d" (fun a b ->
          Printf.printf "%d\n" (f a b) ) ;
    loop (i + 1))
  in
  loop 0
