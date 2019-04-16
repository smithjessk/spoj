let f length =
  let rec loop x ~count =
    if (x = length) then count else (
      let rec inner_loop y ~count =
        if (y = length) then count else (
          let min_dist = min (length - x) (length - y) in
          inner_loop (y + 1) ~count:(count + min_dist)
        ) in
      loop (x + 1) ~count:(inner_loop 0 count)
    ) in
  loop 0 ~count:0

let () =
  let rec loop () =
    let n = read_int () in
    if (n = 0) then () else (
      Printf.printf "%d\n" (f n);
      loop ()
    )
  in
  loop ()
