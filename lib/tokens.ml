type id = string

type token =
  | IF
  | ID of id
  | NUM of int
  | REAL of float
  | ERROR of string
  | EOF
