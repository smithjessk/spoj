(**

if total mod N  = 0 then "YES" else "NO"

*)

let () =
  let rec loop = function
    | 0 -> ()
    | t ->
      ignore (read_line ());
      Scanf.sscanf (read_line ()) "%d" (fun num_kids ->
          let rec loop2 remainder = function
            | 0 -> remainder
            | times_left ->
              let x = int_of_string (read_line ()) in
              loop2 ((remainder + x) mod num_kids) (times_left - 1)in
          let total = loop2 0 num_kids in
          (match total mod num_kids with
          | 0 -> print_endline "YES";
          | _ -> print_endline "NO");
          loop (t - 1)
        )
      in
      Scanf.sscanf (read_line ()) "%d" (fun t ->
      loop t
    )
