(** precondition: l1, l2 are the unsorted hotness ratings
    postcondition: sum(a[i] * b[i]) for i in 0...len - 1 where a, b are sorted l1, l2
*)
let f l1 l2 =
  let combined = List.combine (List.sort compare l1) (List.sort compare l2) in
  List.fold_left (fun sum (a, b) -> sum + a * b) 0 combined

let h s n =
  let inner s n_left =
    match n_left with
    | 1 -> (int_of_string s, "")
    | _ ->
      let idx = String.index s ' ' in
      let rest = (String.sub s idx (String.length s - idx)) in
      (int_of_string (String.sub s 0 idx), String.trim rest) in
  let rec loop s i l =
    if (i = n) then l else begin
      let (int, rest) = inner s (n - i) in
      loop rest (i + 1) (l @ [int])
    end in
  loop s 0 []

let () =
  let n = read_int () in
  let rec loop i =
    if (i = n) then () else (
      let n = read_int () in
      let q () = h (read_line ()) n in
      Printf.printf "%d\n" (f (q ()) (q ()));
      loop (i + 1)
    ) in
  loop 0
