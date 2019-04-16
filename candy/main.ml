(**

   can divide = (float(sum candies)) /. num candies is an integer
   if can't divide -> print -1
   if can divide ->
     insight: you must take candies away from each "big" person and give them
       to the littles. this corresponds to subtraction.
       once there are no more "big people" then things are balanced. (no more littles)
       thus we only need to count getting rid of big people
     count = 0
     for each person:
       if person > avg then count += (person - avg)
     print count
*)

let f l =
  let sum = List.fold_left (+) 0 l in
  let can_div = (sum mod (List.length l) = 0) in
  if (not can_div) then -1 else begin
    let avg = sum / (List.length l) in
    List.fold_left (fun count person ->
        if (person > avg) then (count + (person - avg)) else count) 0 l
  end

let () =
  let rec loop () =
    match read_int () with
    | -1 -> ()
    | n ->
      let rec inner_loop i l =
        if (i = n) then l else (inner_loop (i + 1) ((read_int ()) :: l)) in
      Printf.printf "%d\n" (f (inner_loop 0 []));
      loop ()
  in
  loop ()
