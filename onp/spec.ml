(**
precondition: s is a nonempty string that contains a paren at the left-most idx
postcondition: result is the 'paren expression' starting at the left-most index,
   WITHOUT wrapping parens
*)
let parse_paren s =
  let rec g idx open_count =
    match s.[idx] with
    | '(' -> g (idx + 1) (open_count + 1)
    | ')' -> (match open_count with
      | 1 -> String.sub s 1 (idx - 1)
      | _ -> g (idx + 1) (open_count - 1))
    | _ -> g (idx + 1) open_count in
  g 1 1

type token =
  | Exponent
  | Times
  | Divide
  | Plus
  | Minus
  | Paren of string
  | Atom of char
  | Wrapped of token * token * char

(**
precondition: s is a parseable string (one letter variable names, etc.)
postcondition: result is a tuple (token, rest_of_string). rest_of_string can be
   parsed by calling this function
*)
let parse s =
  let rest s idx = String.sub s idx (String.length s - idx) in
  let s = String.trim s in
  match s.[0] with
  | '^' -> (Exponent, rest s 1)
  | '*' -> (Times, rest s 1)
  | '/' -> (Divide, rest s 1)
  | '+' -> (Plus, rest s 1)
  | '-' -> (Minus, rest s 1)
  | '(' ->
    let x = parse_paren s in
    (Paren x, rest s (String.length x + 2))
  | c -> (Atom c, rest s 1)

let tokenize s =
  let rec f s tkns =
    if (String.length s = 0) then tkns else (
      let tkn, rest = parse s in
      f rest (tkns @ [tkn])
    ) in
  f s []

exception Unmatched

let char_of = function
  | Exponent -> '^'
  | Times -> '*'
  | Divide -> '/'
  | Plus -> '+'
  | Minus -> '-'
  | _ -> raise Unmatched

let combine tkns target =
  let rec f tkns_to_parse tkns_parsed =
    match tkns_to_parse with
    | lhs :: x :: rhs :: rest when x = target ->
      f rest (tkns_parsed @ [Wrapped(lhs, rhs, char_of x)])
    | [] -> tkns_parsed
    | hd :: tl -> f tl (tkns_parsed @ [hd]) in
  f tkns []

let to_one_token s =
  let tkns = tokenize s in
  let tkns = List.fold_left combine tkns [Exponent; Times; Divide; Plus; Minus] in
  match tkns with
  | hd :: [] -> hd
  | _ -> raise Unmatched

let rec rpn_str = function
  | Paren s -> (rpn_str (to_one_token s))
  | Atom c -> Printf.sprintf "%c" c
  | Wrapped (op1, op2, op) ->
    Printf.sprintf "%s%s%c" (rpn_str op1) (rpn_str op2) op
  | _ -> raise Unmatched

let () =
  let rec f n =
    let s = read_line () in
    print_endline (rpn_str (to_one_token s));
    if (n > 1) then (f (n - 1))
  in
  f (read_int ())
