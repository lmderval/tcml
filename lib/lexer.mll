{
let location lexbuf = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

let error ctx lexbuf =
  Printf.eprintf "Invalid token: '%s'\n" (Lexing.lexeme lexbuf);
  (Tokens.ERROR (Lexing.lexeme lexbuf, location lexbuf), Context.error ctx)

let error_str msg ctx =
  Printf.eprintf "%s\n" msg;
  Context.error ctx

let control ctrl ctx =
  match ctrl with
  | '@' -> ('\x00', ctx)
  | 'A' -> ('\x01', ctx)
  | 'B' -> ('\x02', ctx)
  | 'C' -> ('\x03', ctx)
  | 'D' -> ('\x04', ctx)
  | 'E' -> ('\x05', ctx)
  | 'F' -> ('\x06', ctx)
  | 'G' -> ('\x07', ctx)
  | 'H' -> ('\x08', ctx)
  | 'I' -> ('\x09', ctx)
  | 'J' -> ('\x0a', ctx)
  | 'K' -> ('\x0b', ctx)
  | 'L' -> ('\x0c', ctx)
  | 'M' -> ('\x0d', ctx)
  | 'N' -> ('\x0e', ctx)
  | 'O' -> ('\x0f', ctx)
  | 'P' -> ('\x10', ctx)
  | 'Q' -> ('\x11', ctx)
  | 'R' -> ('\x12', ctx)
  | 'S' -> ('\x13', ctx)
  | 'T' -> ('\x14', ctx)
  | 'U' -> ('\x15', ctx)
  | 'V' -> ('\x16', ctx)
  | 'W' -> ('\x17', ctx)
  | 'X' -> ('\x18', ctx)
  | 'Y' -> ('\x19', ctx)
  | 'Z' -> ('\x1a', ctx)
  | '[' -> ('\x1b', ctx)
  | '\\' -> ('\x1c', ctx)
  | ']' -> ('\x1d', ctx)
  | '^' -> ('\x1e', ctx)
  | '_' -> ('\x1f', ctx)
  | '?' -> ('\x7f', ctx)
  | _ -> ('\x00', Context.error ctx)
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
| '"' { let raw = Buffer.create 16 in
        let ctx = Context.set_start_p (Context.recreate_string_buffer ctx 16) lexbuf in
        Buffer.add_char raw '"';
        growing_string raw ctx lexbuf }
(* Blanks *)
| ws+ { token ctx lexbuf }
| nl { Lexing.new_line lexbuf;
       token ctx lexbuf }
| "/*" { let ctx = Context.set_start_p (Context.comment_add_string (Context.recreate_comment_buffer ctx 16) "/*") lexbuf in
         comment (Context.enter_comment ctx) lexbuf }
(* Others *)
| eof { (Tokens.EOF (location lexbuf), ctx) }
| _ { error ctx lexbuf }

and format ctx = parse
| nl { let lxm = Lexing.lexeme lexbuf in
       let ctx = Context.escape_add_string ctx lxm in
       Lexing.new_line lexbuf;
       format ctx lexbuf }
| fmt+ { let lxm = Lexing.lexeme lexbuf in
         let ctx = Context.escape_add_string ctx lxm in
         format ctx lexbuf }
| '\\' { Context.escape_add_char ctx '\\' }
| eof { Context.error ctx }
| _ { let lxm = Lexing.lexeme lexbuf in
      let ctx = Context.escape_add_string ctx lxm in
      format (Context.error ctx) lexbuf }

and escape ctx = parse
| 'n' { ("\x0a", Context.escape_add_char ctx 'n') }
| 't' { ("\x09", Context.escape_add_char ctx 't') }
| '^' _ { let lxm = Lexing.lexeme lexbuf in
          let ctrl, ctx = control (Lexing.lexeme_char lexbuf 1) (Context.escape_add_string ctx lxm) in
          (Printf.sprintf "%c" ctrl, ctx) }
| byte { let raw_code = (Lexing.lexeme lexbuf) in
         let code = int_of_string raw_code in
         let ctx = Context.escape_add_string ctx raw_code in
         if code <= 255
           then (Printf.sprintf "%c" (Char.chr code), ctx)
           else ("\x00", Context.error ctx) }
| '"' { ("\x22", Context.escape_add_char ctx '"') }
| '\\' { ("\x5c", Context.escape_add_char ctx '\\') }
| nl { let lxm = Lexing.lexeme lexbuf in
       let ctx = format (Context.escape_add_string ctx lxm) lexbuf in
       Lexing.new_line lexbuf;
       match ctx.error_state with
       | false -> ("", ctx)
       | true -> ("\x00", ctx) }
| fmt+ { let lxm = Lexing.lexeme lexbuf in
         let ctx = format (Context.escape_add_string ctx lxm) lexbuf in
         match ctx.error_state with
         | false -> ("", ctx)
         | true -> ("\x00", ctx) }
| eof { ("\x00", Context.error ctx) }
| _ { let lxm = Lexing.lexeme lexbuf in
      let ctx = Context.escape_add_string ctx lxm in
      ("\x00", Context.error ctx) }

and growing_string raw ctx = parse
| '"' { let ctx = Context.set_end_p ctx lexbuf in
        Buffer.add_char raw '"';
        match ctx.error_state with
        | false -> (Tokens.STRING (Buffer.contents ctx.string_buf, (ctx.start_p, ctx.end_p)), ctx)
        | true -> (Tokens.ERROR (Printf.sprintf "%S" (Buffer.contents ctx.string_buf), (ctx.start_p, ctx.end_p)), ctx) }
| nl { let lxm = Lexing.lexeme lexbuf in
       let ctx = Context.string_add_string ctx lxm in
       Lexing.new_line lexbuf;
       Buffer.add_string raw lxm;
       growing_string raw ctx lexbuf }
| '\\' { let esc, ctx = escape (Context.escape_add_char (Context.recreate_escape_buffer ctx 16) '\\') lexbuf in
         match ctx.error_state with
         | false -> let ctx = Context.string_add_string ctx esc in
                    let esc = Buffer.contents ctx.escape_buf in
                    Buffer.add_string raw esc;
                    growing_string raw ctx lexbuf
         | true -> let esc = Buffer.contents ctx.escape_buf in
                   let ctx = Context.string_add_string (error_str (Printf.sprintf "Invalid escape: '%s'" esc) ctx) esc in
                   Buffer.add_string raw esc;
                   growing_string raw ctx lexbuf }
| eof { let ctx = Context.set_end_p (error_str "Unexpected EOF" ctx) lexbuf in
        (Tokens.ERROR (Printf.sprintf "%s" (Buffer.contents raw), (ctx.start_p, ctx.end_p)), ctx) }
| _ { let lxm = Lexing.lexeme lexbuf in
      let ctx = Context.string_add_string ctx lxm in
      Buffer.add_string raw (Lexing.lexeme lexbuf);
      growing_string raw ctx lexbuf }

and comment ctx = parse
| nl { let lxm = Lexing.lexeme lexbuf in
       let ctx = Context.comment_add_string ctx lxm in
       Lexing.new_line lexbuf;
       comment ctx lexbuf }
| "/*" { comment (Context.enter_comment (Context.comment_add_string ctx "/*")) lexbuf }
| "*/" { let ctx = Context.leave_comment (Context.comment_add_string ctx "*/") in
         if ctx.comment_level == 0
           then token ctx lexbuf
           else comment ctx lexbuf }
| eof { let ctx = error_str "Unexpected EOF" (Context.set_end_p ctx lexbuf) in
        (Tokens.ERROR (Printf.sprintf "%s" (Buffer.contents ctx.comment_buf), (ctx.start_p, ctx.end_p)), ctx) }
| _ { let lxm = Lexing.lexeme lexbuf in
      let ctx = Context.comment_add_string ctx lxm in
      comment ctx lexbuf }

{}
