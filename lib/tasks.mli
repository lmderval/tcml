type action = unit -> unit

type task = {
  name : string;
  short : char option;
  description : string;
  depends_on : task list;
  action : action;
}

exception Task_not_found of string

val get : string -> task
val register : string -> string -> action -> string -> unit
val run : string -> unit
