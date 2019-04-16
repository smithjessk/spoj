module Stalls : sig
  type t

  val create_stdin : size:int -> t

  val get : t -> index:int -> int

  (** must only search over those that are still available *)
  val idx_of_smallest_above_midpoint : t -> left_idx:int -> right_idx:int -> int

  val place : t -> index:int -> t

  val get_min_adjacent_distance : t -> int
end

let rec f t = function
  | 0 -> Stalls.get_min_adjacent_distance t
  | n ->
    
    f (Stalls.place t idx) (n - 1)
