{
let location lexbuf = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let error lexbuf =
  Printf.eprintf "Invalid token: '%s'\n" (Lexing.lexeme lexbuf);
  Tokens.ERROR (Lexing.lexeme lexbuf, location lexbuf)

let error_str msg =
  Printf.eprintf "%s\n" msg

exception Bad_escape

let control ctrl =
  match ctrl with
  | '@' -> '\x00'
  | 'A' -> '\x01'
  | 'B' -> '\x02'
  | 'C' -> '\x03'
  | 'D' -> '\x04'
  | 'E' -> '\x05'
  | 'F' -> '\x06'
  | 'G' -> '\x07'
  | 'H' -> '\x08'
  | 'I' -> '\x09'
  | 'J' -> '\x0a'
  | 'K' -> '\x0b'
  | 'L' -> '\x0c'
  | 'M' -> '\x0d'
  | 'N' -> '\x0e'
  | 'O' -> '\x0f'
  | 'P' -> '\x10'
  | 'Q' -> '\x11'
  | 'R' -> '\x12'
  | 'S' -> '\x13'
  | 'T' -> '\x14'
  | 'U' -> '\x15'
  | 'V' -> '\x16'
  | 'W' -> '\x17'
  | 'X' -> '\x18'
  | 'Y' -> '\x19'
  | 'Z' -> '\x1a'
  | '[' -> '\x1b'
  | '\\' -> '\x1c'
  | ']' -> '\x1d'
  | '^' -> '\x1e'
  | '_' -> '\x1f'
  | '?' -> '\x7f'
  | _ -> raise Bad_escape
}

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let digit = ['0'-'9']
let byte = digit digit digit
let ws = [' ' '\t']
let nl = '\n' | '\r' | "\n\r" | "\r\n"
let fmt = [' ' '\t' '\x0c']

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
| '"' { let raw = Buffer.create 16 and buffer = Buffer.create 16 in
        Buffer.add_char raw '"';
        growing_string raw (Lexing.lexeme_start_p lexbuf) buffer false lexbuf }
(* Blanks *)
| ws+ { token lexbuf }
| nl { Lexing.new_line lexbuf;
       token lexbuf }
| "/*" { comment 1 lexbuf }
(* Others *)
| eof { Tokens.EOF (location lexbuf) }
| _ { error lexbuf }

and format buffer error_state = parse
| nl { Lexing.new_line lexbuf;
       Buffer.add_string buffer (Lexing.lexeme lexbuf);
       format buffer error_state lexbuf }
| fmt+ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
         format buffer error_state lexbuf }
| '\\' { Buffer.add_char buffer '\\';
         error_state }
| eof { true }
| _ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
      format buffer true lexbuf }

and escape buffer = parse
| 'n' { Buffer.add_char buffer 'n';
        "\x0a" }
| 't' { Buffer.add_char buffer 't';
        "\x09" }
| '^' _ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
          Printf.sprintf "%c" (control (Lexing.lexeme_char lexbuf 1)) }
| byte { let raw_code = (Lexing.lexeme lexbuf) in
         let code = int_of_string raw_code in
         Buffer.add_string buffer raw_code;
         if code <= 255
           then Printf.sprintf "%c" (Char.chr code)
           else raise Bad_escape }
| '"' { Buffer.add_char buffer '"';
        "\x22" }
| '\\' { Buffer.add_char buffer '\\';
         "\x5c" }
| nl { Lexing.new_line lexbuf;
       Buffer.add_string buffer (Lexing.lexeme lexbuf);
       if format buffer false lexbuf
         then raise Bad_escape;
       "" }
| fmt+ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
         if format buffer false lexbuf
           then raise Bad_escape;
         "" }
| eof { raise Bad_escape }
| _ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
      raise Bad_escape }

and growing_string raw start_p buffer error_state = parse
| '"' { Buffer.add_char raw '"';
        let end_p = Lexing.lexeme_end_p lexbuf in
        match error_state with
        | false -> Tokens.STRING (Buffer.contents buffer, (start_p, end_p))
        | true -> Tokens.ERROR (Printf.sprintf "%S" (Buffer.contents buffer), (start_p, end_p)) }
| nl { Lexing.new_line lexbuf;
       let lxm = (Lexing.lexeme lexbuf) in
       Buffer.add_string raw lxm;
       Buffer.add_string buffer lxm;
       growing_string raw start_p buffer error_state lexbuf }
| '\\' { let escape_buffer = Buffer.create 16 in
         Buffer.add_char escape_buffer '\\';
         try
           Buffer.add_string buffer (escape escape_buffer lexbuf);
           Buffer.add_string raw (Buffer.contents escape_buffer);
           growing_string raw start_p buffer error_state lexbuf
         with Bad_escape ->
           let e = Buffer.contents escape_buffer in
           error_str (Printf.sprintf "Invalid escape: '%s'" e);
           Buffer.add_string buffer e;
           Buffer.add_string raw e;
           growing_string raw start_p buffer true lexbuf }
| eof { error_str "Unexpected EOF";
        let end_p = Lexing.lexeme_end_p lexbuf in
        Tokens.ERROR (Printf.sprintf "%s" (Buffer.contents raw), (start_p, end_p)) }
| _ { Buffer.add_string raw (Lexing.lexeme lexbuf);
      Buffer.add_string buffer (Lexing.lexeme lexbuf);
      growing_string raw start_p buffer error_state lexbuf }

and comment level = parse
| nl { Lexing.new_line lexbuf;
       comment level lexbuf }
| "*/" { if level == 0
           then token lexbuf
           else comment (level - 1) lexbuf }
| eof { error lexbuf }
| _ { comment level lexbuf }

{}
