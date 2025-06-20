type context = {
  start_p : Lexing.position;
  end_p : Lexing.position;
  buf : Buffer.t;
  string_buf : Buffer.t;
  escape_buf : Buffer.t;
  error_buf : Buffer.t;
  comment_buf : Buffer.t;
  error_state : bool;
  comment_level : int;
}

type t = context

val create : Lexing.lexbuf -> int -> context
val set_start_p : context -> Lexing.lexbuf -> context
val set_end_p : context -> Lexing.lexbuf -> context
val set_p : context -> Lexing.lexbuf -> context
val location : context -> Lexing.position * Lexing.position
val recreate_buffer : context -> int -> context
val add_char : context -> char -> context
val add_string : context -> string -> context
val recreate_string_buffer : context -> int -> context
val string_add_char : context -> char -> context
val string_add_string : context -> string -> context
val recreate_escape_buffer : context -> int -> context
val escape_add_char : context -> char -> context
val escape_add_string : context -> string -> context
val recreate_error_buffer : context -> int -> context
val error_add_char : context -> char -> context
val error_add_string : context -> string -> context
val recreate_comment_buffer : context -> int -> context
val comment_add_char : context -> char -> context
val comment_add_string : context -> string -> context
val error : context -> context
val enter_comment : context -> context
val leave_comment : context -> context
