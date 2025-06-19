type context = {
  start_p : Lexing.position;
  end_p : Lexing.position;
  buf : Buffer.t;
  string_buf : Buffer.t;
  escape_buf : Buffer.t;
  error_buf : Buffer.t;
  error_state : bool;
}

type t = context

let create lexbuf buf_size =
  {
    start_p = Lexing.lexeme_start_p lexbuf;
    end_p = Lexing.lexeme_end_p lexbuf;
    buf = Buffer.create buf_size;
    string_buf = Buffer.create buf_size;
    escape_buf = Buffer.create buf_size;
    error_buf = Buffer.create buf_size;
    error_state = false;
  }

let set_start_p ctx lexbuf = { ctx with start_p = Lexing.lexeme_start_p lexbuf }
let set_end_p ctx lexbuf = { ctx with end_p = Lexing.lexeme_end_p lexbuf }
let recreate_buffer ctx buf_size = { ctx with buf = Buffer.create buf_size }

let add_char ctx chr =
  Buffer.add_char ctx.buf chr;
  ctx

let add_string ctx str =
  Buffer.add_string ctx.buf str;
  ctx

let recreate_string_buffer ctx buf_size =
  { ctx with string_buf = Buffer.create buf_size }

let string_add_char ctx chr =
  Buffer.add_char ctx.string_buf chr;
  ctx

let string_add_string ctx str =
  Buffer.add_string ctx.string_buf str;
  ctx

let recreate_escape_buffer ctx buf_size =
  { ctx with escape_buf = Buffer.create buf_size }

let escape_add_char ctx chr =
  Buffer.add_char ctx.escape_buf chr;
  ctx

let escape_add_string ctx str =
  Buffer.add_string ctx.escape_buf str;
  ctx

let recreate_error_buffer ctx buf_size =
  { ctx with error_buf = Buffer.create buf_size }

let error_add_char ctx chr =
  Buffer.add_char ctx.error_buf chr;
  ctx

let error_add_string ctx str =
  Buffer.add_string ctx.error_buf str;
  ctx

let error ctx = { ctx with error_state = true }
