{
let error lxm =
  Printf.eprintf "Invalid token: '%s'\n" lxm;
  Tokens.ERROR (lxm)
}

rule token = parse
| "if" { Tokens.IF }
| ['a'-'z']['a'-'z' '0'-'9']* as lxm { Tokens.ID (lxm) }
| ['0'-'9']+ as lxm { Tokens.NUM (int_of_string lxm) }
| ( ['0'-'9']+ '.' ['0'-'9']* ) | ( ['0'-'9']* '.' ['0'-'9']+ ) as lxm { Tokens.REAL (float_of_string lxm) }
| ( "--" ['a'-'z']* '\n' ) | [' ' '\n' '\t'] { token lexbuf }
| eof { Tokens.EOF }
| _ { error (Lexing.lexeme lexbuf) }

{}
