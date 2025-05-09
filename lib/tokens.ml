type id = string
type location = Lexing.position * Lexing.position

type token =
  | IF of location
  | ID of id * location
  | NUM of int * location
  | REAL of float * location
  | ERROR of string * location
  | EOF of location

let string_from_location (loc : location) =
  match loc with
  | start_p, end_p when start_p.pos_cnum == end_p.pos_cnum ->
      Printf.sprintf "%s:%d:%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol)
  | start_p, end_p when start_p.pos_lnum == end_p.pos_lnum ->
      Printf.sprintf "%s:%d:%d-%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol)
        (end_p.pos_cnum - end_p.pos_bol)
  | start_p, end_p ->
      Printf.sprintf "%s:%d:%d-%d:%d" start_p.pos_fname start_p.pos_lnum
        (start_p.pos_cnum - start_p.pos_bol)
        end_p.pos_lnum
        (end_p.pos_cnum - end_p.pos_bol)
