(**

   1. print the entire first column
   2. print the entire second column
   3. etc..

   (starting at n = 0)
   printing the entire n-th column =
     for each 0 <= j < n_row:
       print s[j * n_col + n]

*)

let f s n_col =
  let n_row = String.length s / n_col in
  let rec loop1 col =
    if (col = n_col) then () else begin
      let rec loop2 row =
        if (row = n_row) then () else begin
          let idx = match row with
            | n when n mod 2 = 0 -> row * n_col + col
            | _ -> row * n_col + (n_col - col - 1)
          in
          Printf.printf "%c" (String.get s idx);
          loop2 (row + 1);
        end
      in
      loop2 0;
      loop1 (col + 1);
    end
  in
  loop1 0;
  print_endline ""

let () =
  let rec loop () =
    match read_int () with
    | 0 -> ()
    | n ->
      let orig_s = read_line () in
      f orig_s n;
      loop ()
  in loop ()
