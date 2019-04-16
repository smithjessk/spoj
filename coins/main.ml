let h = Hashtbl.create 10000

let rec f n =
  if (Hashtbl.mem h n) then Hashtbl.find h n else begin
    if (n = 0) then 0 else begin
      let sum = (f (n/2)) + (f (n/3)) + (f (n/4)) in
      let ans = max n sum in
      Hashtbl.add h n ans;
      ans
    end
  end

let () =
  let rec loop () =
    Scanf.bscanf Scanf.Scanning.stdin " %s " (fun s ->
        if (s = "") then () else begin
          Printf.printf "%d\n" (f (int_of_string s));
          loop ()
        end
      ) in loop ()
