open Utils

type expr =
  Fixnum of int
  | Read
  | Add of expr * expr
  | Neg of expr

let rec exp = function
  | Fixnum i -> i
  | Read ->
      let user_input = read_line () in
      int_of_string user_input
  | Add (a, b) -> exp a + exp b
  | Neg e -> -exp e

let interpret program = exp program

type tok_type =
  | LeftParen
  | RightParen
  | Plus
  | Minus
  | Identifier
  | Number of int
  | EOF
type literal = Num

type token = { token_type : tok_type }
type cursor = {
  source : string;
  start : int;
  curr : int; (** Current position *)
  line : int }

type lexer = {
  cur: cursor;
  tokens: token list
}

let advance cursor = { cursor with curr = cursor.curr + 1 }
let peek cursor = String.get cursor.source (cursor.curr + 1)
let is_at_end cursor = cursor.curr >= String.length cursor.source
let push_token tok tokens =
  let new_token = { token_type = tok } in
  tokens @ [new_token] 

let scan_number cursor =
  let rec scan_until_end cursor acc =
    let next = peek cursor in
    if is_digit next then
      scan_until_end (advance cursor) (acc ^ Char.escaped next)
    else (cursor, acc) in
  scan_until_end cursor (String.get cursor.source cursor.curr |> Char.escaped)

let rec scan cursor tokens =
  if is_at_end cursor then tokens else
  let c = String.get cursor.source cursor.curr in
  match c with
  | '(' -> scan (advance cursor) (push_token LeftParen tokens)
  | ')' -> scan (advance cursor) (push_token RightParen tokens)
  | '+' -> scan (advance cursor) (push_token Plus tokens)
  | '-' -> scan (advance cursor) (push_token Minus tokens)
  | c when is_digit c ->
      let cursor, number = scan_number cursor in
      scan (advance cursor) (push_token (Number (int_of_string number)) tokens) 
  | ' ' -> scan (advance cursor) tokens
  | unknown -> failwith ("Unknown token: " ^ (Char.escaped unknown))

let string_of_token tok =
  let string_of_tok_type = function
    | LeftParen -> "LeftParen"
    | RightParen -> "RightParen"
    | Plus -> "Plus"
    | Identifier -> "Identifier"
    | Number i -> "Number " ^ (string_of_int i)
    | Minus -> "Minus"
    | _ -> "" in
string_of_tok_type tok.token_type

let tokenise ?(print=false) (source: string) =
  let cursor = { source; start = 0; curr = 0; line = 0 } in
  let tokens = scan cursor [] in
  if print then begin
    List.iter (fun t -> print_endline (string_of_token t)) tokens;
    tokens
  end else tokens

let rec parse_prim tokens = match tokens with
  | { token_type = Number i } :: r -> Fixnum i, r 
  | a :: _r -> print_endline (string_of_token a); failwith "not done"
  | _ -> failwith ""

and parse_expr tokens = match tokens with
| { token_type = Plus } :: r ->
  let a, r = parse' r in
  let b, r  = parse' r in
  Add (a, b), r
| { token_type = Minus } :: r ->
  let exp, r = parse_expr r in
  Neg exp, r
| _ -> parse_prim tokens

and parse' (tokens: token list) : expr * token list = match tokens with
  | hd :: r when hd.token_type = LeftParen -> begin
    let e, r = parse_expr r in
    match r with
    | hd :: r when hd.token_type = RightParen -> e, r
    | _ -> failwith "expected closing parenthesis"
  end
  | _ -> failwith "cooked"

let parse tokens = fst (parse' tokens)