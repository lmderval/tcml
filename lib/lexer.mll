{
let location lexbuf = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let error lexbuf =
  Printf.eprintf "Invalid token: '%s'\n" (Lexing.lexeme lexbuf);
  Tokens.ERROR (Lexing.lexeme lexbuf, location lexbuf)
}

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']
let ws = [' ' '\t']
let nl = '\n' | '\r' | "\n\r" | "\r\n"

rule token = parse
(* Reserved Keywords *)
| "array" { Tokens.ARRAY (location lexbuf) }
| "break" { Tokens.BREAK (location lexbuf) }
| "do" { Tokens.DO (location lexbuf) }
| "else" { Tokens.ELSE (location lexbuf) }
| "end" { Tokens.END (location lexbuf) }
| "for" { Tokens.FOR (location lexbuf) }
| "function" { Tokens.FUNCTION (location lexbuf) }
| "if" { Tokens.IF (location lexbuf) }
| "in" { Tokens.IN (location lexbuf) }
| "let" { Tokens.LET (location lexbuf) }
| "nil" { Tokens.NIL (location lexbuf) }
| "of" { Tokens.OF (location lexbuf) }
| "then" { Tokens.THEN (location lexbuf) }
| "to" { Tokens.TO (location lexbuf) }
| "type" { Tokens.TYPE (location lexbuf) }
| "var" { Tokens.VAR (location lexbuf) }
| "while" { Tokens.WHILE (location lexbuf) }
(* Punctuation Symbols *)
| "&" { Tokens.AND (location lexbuf) }
| ":=" { Tokens.ASSIGN (location lexbuf) }
| ":" { Tokens.COLUMN (location lexbuf) }
| "," { Tokens.COMMA (location lexbuf) }
| "/" { Tokens.DIV (location lexbuf) }
| "." { Tokens.DOT (location lexbuf) }
| "=" { Tokens.EQ (location lexbuf) }
| ">=" { Tokens.GE (location lexbuf) }
| ">" { Tokens.GT (location lexbuf) }
| "[" { Tokens.LBRACK (location lexbuf) }
| "{" { Tokens.LCURLY (location lexbuf) }
| "(" { Tokens.LPAREN (location lexbuf) }
| "<=" { Tokens.LE (location lexbuf) }
| "<" { Tokens.LT (location lexbuf) }
| "-" { Tokens.MINUS (location lexbuf) }
| "*" { Tokens.MUL (location lexbuf) }
| "<>" { Tokens.NE (location lexbuf) }
| "|" { Tokens.OR (location lexbuf) }
| "+" { Tokens.PLUS (location lexbuf) }
| "]" { Tokens.RBRACK (location lexbuf) }
| "}" { Tokens.RCURLY (location lexbuf) }
| ")" { Tokens.RPAREN (location lexbuf) }
| ";" { Tokens.SEMI (location lexbuf) }
(* Identifiers and Literals *)
| id { Tokens.ID (Lexing.lexeme lexbuf, location lexbuf) }
| digit+ { Tokens.INT (int_of_string (Lexing.lexeme lexbuf), location lexbuf) }
| '"' { growing_string (Lexing.lexeme_start_p lexbuf)
          (Buffer.create 16) lexbuf }
(* Blanks *)
| ws+ { token lexbuf }
| nl { Lexing.new_line lexbuf; token lexbuf }
| "/*" { comment 1 lexbuf }
(* Others *)
| eof { Tokens.EOF (location lexbuf) }
| _ { error lexbuf }

and growing_string start_p buffer = parse
| '"' { let end_p = Lexing.lexeme_end_p lexbuf in
        Tokens.STRING (Buffer.contents buffer, (start_p, end_p)) }
| nl { Lexing.new_line lexbuf;
         Buffer.add_string buffer (Lexing.lexeme lexbuf);
         growing_string start_p buffer lexbuf }
| eof { error lexbuf }
| _ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
      growing_string start_p buffer lexbuf }

and comment level = parse
| nl { Lexing.new_line lexbuf;
       comment level lexbuf }
| "*/" { if level == 0
           then token lexbuf
           else comment (level - 1) lexbuf }
| eof { error lexbuf }
| _ { comment level lexbuf }

{}
