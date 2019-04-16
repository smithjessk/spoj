(** design decisions captured here: 
 *
 * 1. you don't need random access to the digits
 * 2. there's some notion of adding
 * 3. the internal data structure only matters for performance
 * 4. 
 * 5. 
 *
 *)

module Number : sig
  type t

  val create : string -> t

  val add : t -> index_from_right:int -> amount:int -> t

  val to_string : t -> string

  val length : t -> int

  val outer_digits_equal : t -> digits_traversed:int -> bool
end = struct
  type t = (int, int) Hashtbl.t

  let create s =
    let t = Hashtbl.create (String.length s) in
    let rec loop index s =
      match String.length s with
      | n when n == index -> ()
      | _ ->
          let c =
            int_of_char s.[String.length s - 1 - index] - int_of_char '0'
          in
          Hashtbl.replace t index c ;
          loop (index + 1) s
    in
    loop 0 s ; t

  let length = Hashtbl.length

  let to_string t : string =
    Hashtbl.fold
      (fun index value s ->
        s.[String.length s - 1 - index] <- char_of_int (value + int_of_char '0') ;
        s )
      t
      (String.create (length t))

  let rec add t ~index_from_right ~amount =
    ( match Hashtbl.mem t index_from_right with
    | true -> ()
    | false -> Hashtbl.replace t index_from_right 0 ) ;
    let x = Hashtbl.find t index_from_right in
    match x + amount with
    | n when n < 10 ->
        Hashtbl.replace t index_from_right n ;
        t
    | n ->
        Hashtbl.replace t index_from_right (n mod 10) ;
        add t ~index_from_right:(index_from_right + 1) ~amount:1

  let outer_digits_equal t ~digits_traversed =
    let left_index = length t - 1 - digits_traversed in
    Hashtbl.find t left_index == Hashtbl.find t digits_traversed
end

let rec palin_starting_at_t (t : Number.t) digits_traversed : Number.t =
  let effective_length = Number.length t - (digits_traversed * 2) in
  match effective_length with
  | 1 -> t
  | 2 -> (
    match Number.outer_digits_equal t ~digits_traversed with
    | true -> t
    | false ->
        let t = Number.add t ~index_from_right:digits_traversed ~amount:1 in
        palin_starting_at_t t digits_traversed )
  | _ -> (
    match Number.outer_digits_equal t digits_traversed with
    | true -> palin_starting_at_t t (digits_traversed + 1)
    | false ->
        let t = Number.add t ~index_from_right:digits_traversed ~amount:1 in
        palin_starting_at_t t digits_traversed )

let () =
  let t = int_of_string (read_line ()) in
  let rec loop = function
    | n when n = t -> ()
    | i ->
        let t = Number.create (read_line ()) in
        let t = Number.add t ~index_from_right:0 ~amount:1 in
        let next = palin_starting_at_t t 0 in
        print_endline (Number.to_string next) ;
        loop (i + 1)
  in
  loop 0
