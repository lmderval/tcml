open Tcml

let lexbuf = Lexing.from_channel stdin

let () =
  while true do
    let token = Lexer.token lexbuf in
    match token with
    (* Reserved Keywords *)
    | ARRAY loc ->
        Printf.printf "ARRAY at %s\n" (Tokens.string_from_location loc)
    | BREAK loc ->
        Printf.printf "BREAK at %s\n" (Tokens.string_from_location loc)
    | DO loc -> Printf.printf "DO at %s\n" (Tokens.string_from_location loc)
    | ELSE loc -> Printf.printf "ELSE at %s\n" (Tokens.string_from_location loc)
    | END loc -> Printf.printf "END at %s\n" (Tokens.string_from_location loc)
    | FOR loc -> Printf.printf "FOR at %s\n" (Tokens.string_from_location loc)
    | FUNCTION loc ->
        Printf.printf "FUNCTION at %s\n" (Tokens.string_from_location loc)
    | IF loc -> Printf.printf "IF at %s\n" (Tokens.string_from_location loc)
    | IN loc -> Printf.printf "IN at %s\n" (Tokens.string_from_location loc)
    | LET loc -> Printf.printf "LET at %s\n" (Tokens.string_from_location loc)
    | NIL loc -> Printf.printf "NIL at %s\n" (Tokens.string_from_location loc)
    | OF loc -> Printf.printf "OF at %s\n" (Tokens.string_from_location loc)
    | THEN loc -> Printf.printf "THEN at %s\n" (Tokens.string_from_location loc)
    | TO loc -> Printf.printf "TO at %s\n" (Tokens.string_from_location loc)
    | TYPE loc -> Printf.printf "TYPE at %s\n" (Tokens.string_from_location loc)
    | VAR loc -> Printf.printf "VAR at %s\n" (Tokens.string_from_location loc)
    | WHILE loc ->
        Printf.printf "WHILE at %s\n" (Tokens.string_from_location loc)
    (* Punctuation Symbols *)
    | AND loc -> Printf.printf "AND at %s\n" (Tokens.string_from_location loc)
    | ASSIGN loc ->
        Printf.printf "ASSIGN at %s\n" (Tokens.string_from_location loc)
    | COLUMN loc ->
        Printf.printf "COLUMN at %s\n" (Tokens.string_from_location loc)
    | COMMA loc ->
        Printf.printf "COMMA at %s\n" (Tokens.string_from_location loc)
    | DIV loc -> Printf.printf "DIV at %s\n" (Tokens.string_from_location loc)
    | DOT loc -> Printf.printf "DOT at %s\n" (Tokens.string_from_location loc)
    | EQ loc -> Printf.printf "EQ at %s\n" (Tokens.string_from_location loc)
    | GE loc -> Printf.printf "GE at %s\n" (Tokens.string_from_location loc)
    | GT loc -> Printf.printf "GT at %s\n" (Tokens.string_from_location loc)
    | LBRACK loc ->
        Printf.printf "LBRACK at %s\n" (Tokens.string_from_location loc)
    | LCURLY loc ->
        Printf.printf "LCURLY at %s\n" (Tokens.string_from_location loc)
    | LPAREN loc ->
        Printf.printf "LPAREN at %s\n" (Tokens.string_from_location loc)
    | LE loc -> Printf.printf "LE at %s\n" (Tokens.string_from_location loc)
    | LT loc -> Printf.printf "LT at %s\n" (Tokens.string_from_location loc)
    | MINUS loc ->
        Printf.printf "MINUS at %s\n" (Tokens.string_from_location loc)
    | MUL loc -> Printf.printf "MUL at %s\n" (Tokens.string_from_location loc)
    | NE loc -> Printf.printf "NE at %s\n" (Tokens.string_from_location loc)
    | OR loc -> Printf.printf "OR at %s\n" (Tokens.string_from_location loc)
    | PLUS loc -> Printf.printf "PLUS at %s\n" (Tokens.string_from_location loc)
    | RBRACK loc ->
        Printf.printf "RBRACK at %s\n" (Tokens.string_from_location loc)
    | RCURLY loc ->
        Printf.printf "RCURLY at %s\n" (Tokens.string_from_location loc)
    | RPAREN loc ->
        Printf.printf "RPAREN at %s\n" (Tokens.string_from_location loc)
    | SEMI loc -> Printf.printf "SEMI at %s\n" (Tokens.string_from_location loc)
    (* Identifiers and Literals *)
    | ID (v, loc) ->
        Printf.printf "ID (%s) at %s\n" v (Tokens.string_from_location loc)
    | INT (v, loc) ->
        Printf.printf "INT (%d) at %s\n" v (Tokens.string_from_location loc)
    | STRING (v, loc) ->
        Printf.printf "STRING (%S) at %s\n" v (Tokens.string_from_location loc)
    (* Others *)
    | ERROR (e, loc) ->
        Printf.printf "ERROR (%s) at %s\n" e (Tokens.string_from_location loc)
    | EOF loc ->
        Printf.printf "EOF at %s\n" (Tokens.string_from_location loc);
        exit 0
  done
