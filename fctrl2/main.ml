let rec add a b =
  let pad s desired_len =
    let pad_len = desired_len - (String.length s) in
    let pad = String.make pad_len '0' in
    String.concat "" [pad; s] in
  if (String.length a < String.length b) then (
    add (pad a (String.length b)) b
  ) else if (String.length a > String.length b) then (
    add a (pad b (String.length a))
  ) else (

    let rec trim_leading_zeroes_if_s_len_gr_1 s =
      if (String.get s 0 == '0') then (
        if (String.length s > 1) then trim_leading_zeroes_if_s_len_gr_1 (String.sub s 1 (String.length s - 1)) else s
      ) else s in

    let rec same_length_str_add (s1: string) s2 carry: string =
      let prefix s: string = String.sub s 0 (String.length s - 1) in
      let last s = String.sub s (String.length s - 1) 1 in
      let sum = (int_of_string (last s1)) + (int_of_string (last s2)) + carry in
      if (String.length s1 == 1) then (string_of_int sum) else (
        let rest = same_length_str_add (prefix s1) (prefix s2) (sum / 10) in
        let s = String.concat "" [rest; (string_of_int (sum mod 10))] in
        trim_leading_zeroes_if_s_len_gr_1 s
      ) in
    same_length_str_add a b 0)

let mul top bottom =
  let rec f n ~sum =
    if (n == 0) then sum else (f (n - 1) ~sum:(add sum top)) in
  f bottom ~sum:"0"

let rec fac n hashtbl =
  if (not (Hashtbl.mem hashtbl n)) then (
    let s = mul (fac (n - 1) hashtbl) n in
    Hashtbl.add hashtbl n s
  );
  Hashtbl.find hashtbl n

let () =
  let h = Hashtbl.create 100 in
  Hashtbl.add h 0 "1";
  Hashtbl.add h 1 "1";
  let rec f n_times_left =
    if (n_times_left == 0) then () else (
      print_endline (fac (read_int ()) h);
      f (n_times_left - 1)
    ) in
  f (read_int ())

