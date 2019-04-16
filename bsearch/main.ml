let find target arr =
  let rec f ~low_inclusve ~high_inclusive =
    if low_inclusve > high_inclusive then None
    else
      let mid_idx = (low_inclusve + high_inclusive) / 2 in
      match arr.(mid_idx) with
      | mid when mid > target -> f ~low_inclusve ~high_inclusive:(mid_idx - 1)
      | mid when mid < target -> f ~low_inclusve:(mid_idx + 1) ~high_inclusive
      | target -> Some mid_idx
  in
  f ~low_inclusve:0 ~high_inclusive:(Array.length arr - 1)

let () =
  let read_int () = Scanf.bscanf Scanf.Scanning.stdib " %d " (fun x -> x) in
  let n = read_int () in
  let arr = Array.make n 0 in
  let q = read_int () in
  Printf.printf "n, q = %d, %d\n" n q ;
  let rec loop = function
    | i when i = n -> ()
    | i ->
        arr.(i) <- read_int () ;
        loop (i + 1)
  in
  loop 0 ;
  let rec loop = function
    | i when i = q -> ()
    | i ->
        let target = read_int () in
        print_int target ;
        let idx = match find target arr with None -> -1 | Some x -> x in
        Printf.printf "%d\n" idx ;
        loop (i + 1)
  in
  loop 0
