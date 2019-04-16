let f a b c =
  if ((c - b) = (b - a)) then
    Printf.printf "AP %d\n" (c + (c - b))
  else
    Printf.printf "GP %d\n" (c * (c / b))

let () =
  let rec loop () =
    Scanf.sscanf (read_line ()) "%d %d %d" (fun a b c ->
        if (a = 0 && b = 0 && c = 0) then () else (f a b c; loop ()))
  in
  loop ()
