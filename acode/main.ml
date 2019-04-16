(**

   case 1: s has two characters at the beginning that are b/w 10 and 26 inclusive
   answer = answer if we just take 1 char + answer if we take 2 chars
   case 2: s has at least 1 character
   answer = answer if we just take 1 char
   case 3: 0 characters -> 1

   ***********
   sanity checks:

   f "0" = raise exn
   f "1" -> 1
   f "12" -> 2
   f "120" -> f "20" -> 1
   f "560" -> f "60" -> none
*)

module Computable : sig
  type t = | Empty | Valid of string

  val create_exn : string -> t

  type suffix_or_invalid = | Suffix of t | Invalid
  val suffix : t -> int -> suffix_or_invalid
end = struct
  type t = | Empty | Valid of string

  let create_exn = function
    | "" -> Empty
    | s -> Valid s

  type suffix_or_invalid = | Suffix of t | Invalid
  let suffix t n =
    let rest s n_chopped =
      String.sub s n_chopped (String.length s - n_chopped) in
    match t with
    | Empty -> Invalid
    | Valid s -> (
        match n with
        | n when n > (String.length s) -> Invalid
        | n when n = (String.length s) -> Suffix Empty
        | 1 ->
          (match s.[0] with
            | '0' -> Invalid
            | _ -> Suffix (Valid (rest s 1)))
        | 2 ->
          (match s.[0] with
           | '1' | '2' -> Suffix (Valid (rest s 2))
           | _ -> Invalid)
        | _ -> raise (Invalid_argument "n must be either 1 or 2")
     )
end

let rec f = function
  | Computable.Empty -> 1
  | t ->
    (match Computable.suffix t 1 with
     | Invalid -> 0
     | Suffix t -> f t) +
    (match Computable.suffix t 2 with
     | Invalid -> 0
     | Suffix t -> f t)

let () =
  let rec loop () =
    let s = read_line () in
    if (not (s = "0")) then (
      Printf.printf "%d\n" (f (Computable.create_exn s));
      loop ()) in
  loop ()
