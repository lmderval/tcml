{
let location lexbuf = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let error ctx lexbuf =
  Printf.eprintf "Invalid token: '%s'\n" (Lexing.lexeme lexbuf);
  (Tokens.ERROR (Lexing.lexeme lexbuf, location lexbuf), Context.error ctx)

let error_str ctx msg =
  (Printf.eprintf "%s\n" msg, Context.error ctx)

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

rule token ctx = parse
(* Reserved Keywords *)
| "array" { (Tokens.ARRAY (location lexbuf), ctx) }
| "break" { (Tokens.BREAK (location lexbuf), ctx) }
| "do" { (Tokens.DO (location lexbuf), ctx) }
| "else" { (Tokens.ELSE (location lexbuf), ctx) }
| "end" { (Tokens.END (location lexbuf), ctx) }
| "for" { (Tokens.FOR (location lexbuf), ctx) }
| "function" { (Tokens.FUNCTION (location lexbuf), ctx) }
| "if" { (Tokens.IF (location lexbuf), ctx) }
| "in" { (Tokens.IN (location lexbuf), ctx) }
| "let" { (Tokens.LET (location lexbuf), ctx) }
| "nil" { (Tokens.NIL (location lexbuf), ctx) }
| "of" { (Tokens.OF (location lexbuf), ctx) }
| "then" { (Tokens.THEN (location lexbuf), ctx) }
| "to" { (Tokens.TO (location lexbuf), ctx) }
| "type" { (Tokens.TYPE (location lexbuf), ctx) }
| "var" { (Tokens.VAR (location lexbuf), ctx) }
| "while" { (Tokens.WHILE (location lexbuf), ctx) }
(* Punctuation Symbols *)
| "&" { (Tokens.AND (location lexbuf), ctx) }
| ":=" { (Tokens.ASSIGN (location lexbuf), ctx) }
| ":" { (Tokens.COLUMN (location lexbuf), ctx) }
| "," { (Tokens.COMMA (location lexbuf), ctx) }
| "/" { (Tokens.DIV (location lexbuf), ctx) }
| "." { (Tokens.DOT (location lexbuf), ctx) }
| "=" { (Tokens.EQ (location lexbuf), ctx) }
| ">=" { (Tokens.GE (location lexbuf), ctx) }
| ">" { (Tokens.GT (location lexbuf), ctx) }
| "[" { (Tokens.LBRACK (location lexbuf), ctx) }
| "{" { (Tokens.LCURLY (location lexbuf), ctx) }
| "(" { (Tokens.LPAREN (location lexbuf), ctx) }
| "<=" { (Tokens.LE (location lexbuf), ctx) }
| "<" { (Tokens.LT (location lexbuf), ctx) }
| "-" { (Tokens.MINUS (location lexbuf), ctx) }
| "*" { (Tokens.MUL (location lexbuf), ctx) }
| "<>" { (Tokens.NE (location lexbuf), ctx) }
| "|" { (Tokens.OR (location lexbuf), ctx) }
| "+" { (Tokens.PLUS (location lexbuf), ctx) }
| "]" { (Tokens.RBRACK (location lexbuf), ctx) }
| "}" { (Tokens.RCURLY (location lexbuf), ctx) }
| ")" { (Tokens.RPAREN (location lexbuf), ctx) }
| ";" { (Tokens.SEMI (location lexbuf), ctx) }
(* Identifiers and Literals *)
| id { (Tokens.ID (Lexing.lexeme lexbuf, location lexbuf), ctx) }
| digit+ { (Tokens.INT (int_of_string (Lexing.lexeme lexbuf), location lexbuf), ctx) }
(* | '"' { let raw = Buffer.create 16 and buffer = Buffer.create 16 in *)
        (* Buffer.add_char raw '"'; *)
        (* growing_string raw (Lexing.lexeme_start_p lexbuf) buffer false lexbuf } *)
(* Blanks *)
| ws+ { token ctx lexbuf }
| nl { Lexing.new_line lexbuf;
       token ctx lexbuf }
(* | "/*" { let buffer = Buffer.create 16 in *)
         (* Buffer.add_string buffer "/*"; *)
         (* comment 0 (Lexing.lexeme_start_p lexbuf) buffer lexbuf } *)
(* Others *)
| eof { (Tokens.EOF (location lexbuf), ctx) }
| _ { error ctx lexbuf }

(* and format buffer error_state = parse *)
(* | nl { Lexing.new_line lexbuf; *)
       (* Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
       (* format buffer error_state lexbuf } *)
(* | fmt+ { Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
         (* format buffer error_state lexbuf } *)
(* | '\\' { Buffer.add_char buffer '\\'; *)
         (* error_state } *)
(* | eof { true } *)
(* | _ { Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
      (* format buffer true lexbuf } *)

(* and escape buffer = parse *)
(* | 'n' { Buffer.add_char buffer 'n'; *)
        (* "\x0a" } *)
(* | 't' { Buffer.add_char buffer 't'; *)
        (* "\x09" } *)
(* | '^' _ { Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
          (* Printf.sprintf "%c" (control (Lexing.lexeme_char lexbuf 1)) } *)
(* | byte { let raw_code = (Lexing.lexeme lexbuf) in *)
         (* let code = int_of_string raw_code in *)
         (* Buffer.add_string buffer raw_code; *)
         (* if code <= 255 *)
           (* then Printf.sprintf "%c" (Char.chr code) *)
           (* else raise Bad_escape } *)
(* | '"' { Buffer.add_char buffer '"'; *)
        (* "\x22" } *)
(* | '\\' { Buffer.add_char buffer '\\'; *)
         (* "\x5c" } *)
(* | nl { Lexing.new_line lexbuf; *)
       (* Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
       (* if format buffer false lexbuf *)
         (* then raise Bad_escape; *)
       (* "" } *)
(* | fmt+ { Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
         (* if format buffer false lexbuf *)
           (* then raise Bad_escape; *)
         (* "" } *)
(* | eof { raise Bad_escape } *)
(* | _ { Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
      (* raise Bad_escape } *)

(* and growing_string raw start_p buffer error_state = parse *)
(* | '"' { Buffer.add_char raw '"'; *)
        (* let end_p = Lexing.lexeme_end_p lexbuf in *)
        (* match error_state with *)
        (* | false -> Tokens.STRING (Buffer.contents buffer, (start_p, end_p)) *)
        (* | true -> Tokens.ERROR (Printf.sprintf "%S" (Buffer.contents buffer), (start_p, end_p)) } *)
(* | nl { Lexing.new_line lexbuf; *)
       (* let lxm = (Lexing.lexeme lexbuf) in *)
       (* Buffer.add_string raw lxm; *)
       (* Buffer.add_string buffer lxm; *)
       (* growing_string raw start_p buffer error_state lexbuf } *)
(* | '\\' { let escape_buffer = Buffer.create 16 in *)
         (* Buffer.add_char escape_buffer '\\'; *)
         (* try *)
           (* Buffer.add_string buffer (escape escape_buffer lexbuf); *)
           (* Buffer.add_string raw (Buffer.contents escape_buffer); *)
           (* growing_string raw start_p buffer error_state lexbuf *)
         (* with Bad_escape -> *)
           (* let e = Buffer.contents escape_buffer in *)
           (* error_str (Printf.sprintf "Invalid escape: '%s'" e); *)
           (* Buffer.add_string buffer e; *)
           (* Buffer.add_string raw e; *)
           (* growing_string raw start_p buffer true lexbuf } *)
(* | eof { error_str "Unexpected EOF"; *)
        (* let end_p = Lexing.lexeme_end_p lexbuf in *)
        (* Tokens.ERROR (Printf.sprintf "%s" (Buffer.contents raw), (start_p, end_p)) } *)
(* | _ { Buffer.add_string raw (Lexing.lexeme lexbuf); *)
      (* Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
      (* growing_string raw start_p buffer error_state lexbuf } *)

(* and comment level start_p buffer = parse *)
(* | nl { Lexing.new_line lexbuf; *)
       (* Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
       (* comment level start_p buffer lexbuf } *)
(* | "/*" { Buffer.add_string buffer "/*"; *)
         (* comment (level + 1) start_p buffer lexbuf } *)
(* | "*/" { Buffer.add_string buffer "*/"; *)
         (* if level == 0 *)
           (* then token lexbuf *)
           (* else comment (level - 1) start_p buffer lexbuf } *)
(* | eof { error_str "Unexpected EOF"; *)
        (* let end_p = Lexing.lexeme_end_p lexbuf in *)
        (* Tokens.ERROR (Printf.sprintf "%s" (Buffer.contents buffer), (start_p, end_p)) } *)
(* | _ { Buffer.add_string buffer (Lexing.lexeme lexbuf); *)
      (* comment level start_p buffer lexbuf } *)

{}
