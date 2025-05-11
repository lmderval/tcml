open Tcml

let rec _scan (lexbuf : Lexing.lexbuf) =
  let token = Lexer.token lexbuf in
  Printf.printf "%s\n" (Tokens.string_of_token token);
  match token with EOF _ -> () | _ -> _scan lexbuf

let scan (channel : in_channel) = _scan (Lexing.from_channel channel)
let () = scan stdin
