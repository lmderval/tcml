type id = string
type location = Lexing.position * Lexing.position

type token =
  (* Reserved Keywords *)
  | ARRAY of location
  | BREAK of location
  | DO of location
  | ELSE of location
  | END of location
  | FOR of location
  | FUNCTION of location
  | IF of location
  | IN of location
  | LET of location
  | NIL of location
  | OF of location
  | THEN of location
  | TO of location
  | TYPE of location
  | VAR of location
  | WHILE of location
  (* Punctuation Symbols *)
  | AND of location
  | ASSIGN of location
  | COLUMN of location
  | COMMA of location
  | DIV of location
  | DOT of location
  | EQ of location
  | GE of location
  | GT of location
  | LBRACK of location
  | LCURLY of location
  | LPAREN of location
  | LE of location
  | LT of location
  | MINUS of location
  | MUL of location
  | NE of location
  | OR of location
  | PLUS of location
  | RBRACK of location
  | RCURLY of location
  | RPAREN of location
  | SEMI of location
  (* Identifiers and Literals *)
  | ID of id * location
  | INT of int * location
  | STRING of string * location
  (* Others *)
  | ERROR of string * location
  | EOF of location

let string_from_location (loc : location) =
  match loc with
  | start_p, end_p when start_p.pos_cnum == end_p.pos_cnum ->
      Printf.sprintf "%s:%d:%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol)
  | start_p, end_p when start_p.pos_lnum == end_p.pos_lnum ->
      Printf.sprintf "%s:%d:%d-%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol)
        (end_p.pos_cnum - end_p.pos_bol)
  | start_p, end_p ->
      Printf.sprintf "%s:%d:%d-%d:%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol)
        end_p.pos_lnum
        (end_p.pos_cnum - end_p.pos_bol)
