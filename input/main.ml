let () =
  let read_int () =
    Scanf.bscanf Scanf.Scanning.stdin " %d " (fun d -> d) in
  let n = 3 in
  let rec loop i l =
    if (i = n) then (List.fold_left (+) 0 l) else begin
      loop (i + 1) ((read_int ()) :: l)
    end
  in
  Printf.printf "%d\n" (loop 0 [])
