open Tcml

let lexbuf = Lexing.from_channel stdin

let () =
  while true do
    let token = Lexer.token lexbuf in
    match token with
    | IF loc -> Printf.printf "IF at %s\n" (Tokens.string_from_location loc)
    | ID (id, loc) ->
        Printf.printf "ID (%s) at %s\n" id (Tokens.string_from_location loc)
    | NUM (n, loc) ->
        Printf.printf "NUM (%d) at %s\n" n (Tokens.string_from_location loc)
    | REAL (x, loc) ->
        Printf.printf "REAL (%f) at %s\n" x (Tokens.string_from_location loc)
    | ERROR (e, loc) ->
        Printf.printf "ERROR (%s) at %s\n" e (Tokens.string_from_location loc)
    | EOF loc ->
        Printf.printf "EOF at %s\n" (Tokens.string_from_location loc);
        exit 0
  done
