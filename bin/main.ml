open Tcml

let lexbuf = Lexing.from_channel stdin

let () =
  while true do
    let token = Lexer.token lexbuf in
    match token with
    | IF -> Printf.printf "IF\n"
    | ID id -> Printf.printf "ID (%s)\n" id
    | NUM n -> Printf.printf "NUM (%d)\n" n
    | REAL x -> Printf.printf "REAL (%f)\n" x
    | ERROR e -> Printf.printf "ERROR (%s)\n" e
    | EOF ->
        Printf.printf "EOF\n";
        exit 0
  done
