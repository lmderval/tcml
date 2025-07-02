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

let string_of_location (loc : location) =
  match loc with
  | start_p, end_p when start_p.pos_cnum == end_p.pos_cnum ->
      Printf.sprintf "%s:%d:%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol + 1)
  | start_p, end_p when start_p.pos_lnum == end_p.pos_lnum ->
      Printf.sprintf "%s:%d:%d-%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol + 1)
        (end_p.pos_cnum - end_p.pos_bol + 1)
  | start_p, end_p ->
      Printf.sprintf "%s:%d:%d-%d:%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol + 1)
        end_p.pos_lnum
        (end_p.pos_cnum - end_p.pos_bol + 1)

let string_of_token (token : token) =
  match token with
  (* Reserved Keywords *)
  | ARRAY loc -> Printf.sprintf "ARRAY at %s" (string_of_location loc)
  | BREAK loc -> Printf.sprintf "BREAK at %s" (string_of_location loc)
  | DO loc -> Printf.sprintf "DO at %s" (string_of_location loc)
  | ELSE loc -> Printf.sprintf "ELSE at %s" (string_of_location loc)
  | END loc -> Printf.sprintf "END at %s" (string_of_location loc)
  | FOR loc -> Printf.sprintf "FOR at %s" (string_of_location loc)
  | FUNCTION loc -> Printf.sprintf "FUNCTION at %s" (string_of_location loc)
  | IF loc -> Printf.sprintf "IF at %s" (string_of_location loc)
  | IN loc -> Printf.sprintf "IN at %s" (string_of_location loc)
  | LET loc -> Printf.sprintf "LET at %s" (string_of_location loc)
  | NIL loc -> Printf.sprintf "NIL at %s" (string_of_location loc)
  | OF loc -> Printf.sprintf "OF at %s" (string_of_location loc)
  | THEN loc -> Printf.sprintf "THEN at %s" (string_of_location loc)
  | TO loc -> Printf.sprintf "TO at %s" (string_of_location loc)
  | TYPE loc -> Printf.sprintf "TYPE at %s" (string_of_location loc)
  | VAR loc -> Printf.sprintf "VAR at %s" (string_of_location loc)
  | WHILE loc -> Printf.sprintf "WHILE at %s" (string_of_location loc)
  (* Punctuation Symbols *)
  | AND loc -> Printf.sprintf "AND at %s" (string_of_location loc)
  | ASSIGN loc -> Printf.sprintf "ASSIGN at %s" (string_of_location loc)
  | COLUMN loc -> Printf.sprintf "COLUMN at %s" (string_of_location loc)
  | COMMA loc -> Printf.sprintf "COMMA at %s" (string_of_location loc)
  | DIV loc -> Printf.sprintf "DIV at %s" (string_of_location loc)
  | DOT loc -> Printf.sprintf "DOT at %s" (string_of_location loc)
  | EQ loc -> Printf.sprintf "EQ at %s" (string_of_location loc)
  | GE loc -> Printf.sprintf "GE at %s" (string_of_location loc)
  | GT loc -> Printf.sprintf "GT at %s" (string_of_location loc)
  | LBRACK loc -> Printf.sprintf "LBRACK at %s" (string_of_location loc)
  | LCURLY loc -> Printf.sprintf "LCURLY at %s" (string_of_location loc)
  | LPAREN loc -> Printf.sprintf "LPAREN at %s" (string_of_location loc)
  | LE loc -> Printf.sprintf "LE at %s" (string_of_location loc)
  | LT loc -> Printf.sprintf "LT at %s" (string_of_location loc)
  | MINUS loc -> Printf.sprintf "MINUS at %s" (string_of_location loc)
  | MUL loc -> Printf.sprintf "MUL at %s" (string_of_location loc)
  | NE loc -> Printf.sprintf "NE at %s" (string_of_location loc)
  | OR loc -> Printf.sprintf "OR at %s" (string_of_location loc)
  | PLUS loc -> Printf.sprintf "PLUS at %s" (string_of_location loc)
  | RBRACK loc -> Printf.sprintf "RBRACK at %s" (string_of_location loc)
  | RCURLY loc -> Printf.sprintf "RCURLY at %s" (string_of_location loc)
  | RPAREN loc -> Printf.sprintf "RPAREN at %s" (string_of_location loc)
  | SEMI loc -> Printf.sprintf "SEMI at %s" (string_of_location loc)
  (* Identifiers and Literals *)
  | ID (v, loc) -> Printf.sprintf "ID (%s) at %s" v (string_of_location loc)
  | INT (v, loc) -> Printf.sprintf "INT (%d) at %s" v (string_of_location loc)
  | STRING (v, loc) ->
      Printf.sprintf "STRING (%S) at %s" v (string_of_location loc)
  (* Others *)
  | ERROR (e, loc) ->
      Printf.sprintf "ERROR (%s) at %s" e (string_of_location loc)
  | EOF loc -> Printf.sprintf "EOF at %s" (string_of_location loc)
