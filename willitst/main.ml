let () =
  let i = read_float () in
  let s =
    if i <= 1. then "TAK"
    else
      let rez = log i /. log 2. in
      match mod_float rez 1.0 with 0. -> "TAK" | _ -> "NIE"
  in
  print_string s
