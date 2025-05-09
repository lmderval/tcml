{
let location lexbuf = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let error lexbuf =
  Printf.eprintf "Invalid token: '%s'\n" (Lexing.lexeme lexbuf);
  Tokens.ERROR (Lexing.lexeme lexbuf, location lexbuf)
}

rule token = parse
| "if" { Tokens.IF (location lexbuf) }
| ['a'-'z']['a'-'z' '0'-'9']* as lxm { Tokens.ID (lxm, location lexbuf) }
| ['0'-'9']+ as lxm { Tokens.NUM (int_of_string lxm, location lexbuf) }
| ( ['0'-'9']+ '.' ['0'-'9']* ) | ( ['0'-'9']* '.' ['0'-'9']+ ) as lxm { Tokens.REAL (float_of_string lxm, location lexbuf) }
| [' ' '\t'] { token lexbuf }
| ( "--" ['a'-'z']* )? '\n' { Lexing.new_line lexbuf; token lexbuf }
| eof { Tokens.EOF (location lexbuf) }
| _ { error lexbuf }

{}
