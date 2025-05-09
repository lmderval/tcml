type id = string
type location = Lexing.position * Lexing.position

type token =
  | IF of location
  | ID of id * location
  | NUM of int * location
  | REAL of float * location
  | ERROR of string * location
  | EOF of location

val string_from_location : location -> string
