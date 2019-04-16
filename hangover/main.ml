let f n =
  let rec loop ~this_card ~sum_before_this_card =
    match sum_before_this_card with
    | s when s >= n -> this_card - 1
    | s ->
        let this_ratio = 1. /. float_of_int (this_card + 1) in
        loop ~this_card:(this_card + 1)
          ~sum_before_this_card:(sum_before_this_card +. this_ratio)
  in
  loop 1 0.

let () =
  let rec loop () =
    match float_of_string (read_line ()) with
    | 0. -> ()
    | n ->
        Printf.printf "%d card(s)\n" (f n) ;
        loop ()
  in
  loop ()
