{
let error ctx lexbuf =
  let lxm = Lexing.lexeme lexbuf in
  let ctx = Context.error (Context.set_p ctx lexbuf) in
  Printf.eprintf "Invalid token: '%s'\n" lxm;
  (Tokens.ERROR (lxm, Context.location ctx), ctx)

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
| "array" { let ctx = Context.set_p ctx lexbuf in
            (Tokens.ARRAY (Context.location ctx), ctx) }
| "break" { let ctx = Context.set_p ctx lexbuf in
            (Tokens.BREAK (Context.location ctx), ctx) }
| "do" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.DO (Context.location ctx), ctx) }
| "else" { let ctx = Context.set_p ctx lexbuf in
           (Tokens.ELSE (Context.location ctx), ctx) }
| "end" { let ctx = Context.set_p ctx lexbuf in
          (Tokens.END (Context.location ctx), ctx) }
| "for" { let ctx = Context.set_p ctx lexbuf in
          (Tokens.FOR (Context.location ctx), ctx) }
| "function" { let ctx = Context.set_p ctx lexbuf in
               (Tokens.FUNCTION (Context.location ctx), ctx) }
| "if" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.IF (Context.location ctx), ctx) }
| "in" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.IN (Context.location ctx), ctx) }
| "let" { let ctx = Context.set_p ctx lexbuf in
          (Tokens.LET (Context.location ctx), ctx) }
| "nil" { let ctx = Context.set_p ctx lexbuf in
          (Tokens.NIL (Context.location ctx), ctx) }
| "of" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.OF (Context.location ctx), ctx) }
| "then" { let ctx = Context.set_p ctx lexbuf in
           (Tokens.THEN (Context.location ctx), ctx) }
| "to" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.TO (Context.location ctx), ctx) }
| "type" { let ctx = Context.set_p ctx lexbuf in
           (Tokens.TYPE (Context.location ctx), ctx) }
| "var" { let ctx = Context.set_p ctx lexbuf in
          (Tokens.VAR (Context.location ctx), ctx) }
| "while" { let ctx = Context.set_p ctx lexbuf in
            (Tokens.WHILE (Context.location ctx), ctx) }
(* Punctuation Symbols *)
| "&" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.AND (Context.location ctx), ctx) }
| ":=" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.ASSIGN (Context.location ctx), ctx) }
| ":" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.COLUMN (Context.location ctx), ctx) }
| "," { let ctx = Context.set_p ctx lexbuf in
        (Tokens.COMMA (Context.location ctx), ctx) }
| "/" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.DIV (Context.location ctx), ctx) }
| "." { let ctx = Context.set_p ctx lexbuf in
        (Tokens.DOT (Context.location ctx), ctx) }
| "=" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.EQ (Context.location ctx), ctx) }
| ">=" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.GE (Context.location ctx), ctx) }
| ">" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.GT (Context.location ctx), ctx) }
| "[" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.LBRACK (Context.location ctx), ctx) }
| "{" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.LCURLY (Context.location ctx), ctx) }
| "(" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.LPAREN (Context.location ctx), ctx) }
| "<=" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.LE (Context.location ctx), ctx) }
| "<" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.LT (Context.location ctx), ctx) }
| "-" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.MINUS (Context.location ctx), ctx) }
| "*" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.MUL (Context.location ctx), ctx) }
| "<>" { let ctx = Context.set_p ctx lexbuf in
         (Tokens.NE (Context.location ctx), ctx) }
| "|" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.OR (Context.location ctx), ctx) }
| "+" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.PLUS (Context.location ctx), ctx) }
| "]" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.RBRACK (Context.location ctx), ctx) }
| "}" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.RCURLY (Context.location ctx), ctx) }
| ")" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.RPAREN (Context.location ctx), ctx) }
| ";" { let ctx = Context.set_p ctx lexbuf in
        (Tokens.SEMI (Context.location ctx), ctx) }
(* Identifiers and Literals *)
| id { let lxm = Lexing.lexeme lexbuf in
       let ctx = Context.set_p ctx lexbuf in
       (Tokens.ID (lxm, Context.location ctx), ctx) }
| digit+ { let lxm = Lexing.lexeme lexbuf in
           let ctx = Context.set_p ctx lexbuf in
           (Tokens.INT (int_of_string lxm, Context.location ctx), ctx) }
| '"' { let raw = Buffer.create 16 in
        let ctx = Context.set_p (Context.recreate_string_buffer ctx 16) lexbuf in
        Buffer.add_char raw '"';
        growing_string raw ctx lexbuf }
(* Blanks *)
| ws+ { token ctx lexbuf }
| nl { Lexing.new_line lexbuf;
       token ctx lexbuf }
| "/*" { let ctx = Context.set_p (Context.comment_add_string (Context.recreate_comment_buffer ctx 16) "/*") lexbuf in
         comment (Context.enter_comment ctx) lexbuf }
(* Others *)
| eof { let ctx = Context.set_p ctx lexbuf in
        (Tokens.EOF (Context.location ctx), ctx) }
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
| byte { let lxm = (Lexing.lexeme lexbuf) in
         let code = int_of_string lxm in
         let ctx = Context.escape_add_string ctx lxm in
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
        | false -> (Tokens.STRING (Buffer.contents ctx.string_buf, Context.location ctx), ctx)
        | true -> (Tokens.ERROR (Printf.sprintf "%S" (Buffer.contents ctx.string_buf), Context.location ctx), ctx) }
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
        (Tokens.ERROR (Printf.sprintf "%s" (Buffer.contents raw), Context.location ctx), ctx) }
| _ { let lxm = Lexing.lexeme lexbuf in
      let ctx = Context.string_add_string ctx lxm in
      Buffer.add_string raw lxm;
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
        (Tokens.ERROR (Printf.sprintf "%s" (Buffer.contents ctx.comment_buf), Context.location ctx), ctx) }
| _ { let lxm = Lexing.lexeme lexbuf in
      let ctx = Context.comment_add_string ctx lxm in
      comment ctx lexbuf }

{}
