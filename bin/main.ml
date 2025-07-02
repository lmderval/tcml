open Tcml

let rec _scan lexbuf ctx =
  let token, ctx = Lexer.token ctx lexbuf in
  Printf.printf "%s\n" (Tokens.string_of_token token);
  match token with EOF _ -> () | _ -> _scan lexbuf ctx

let scan channel =
  let lexbuf = Lexing.from_channel channel in
  let ctx = Context.create lexbuf 16 in
  _scan lexbuf ctx

let () = scan stdin
