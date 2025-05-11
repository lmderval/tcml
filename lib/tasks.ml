type action = unit -> unit

type task = {
  name : string;
  short : char option;
  description : string;
  depends_on : task list;
  action : action;
}

exception Task_not_found of string

let _tasks = Hashtbl.create 16

let get (name : string) =
  match Hashtbl.find_opt _tasks name with
  | Some task -> task
  | None -> raise (Task_not_found name)

let _validate_name (name : string) =
  String.iter
    (fun chr ->
      match chr with
      | 'a' .. 'z' | '-' -> ()
      | _ -> raise (Invalid_argument (Printf.sprintf "Invalid name: '%s'" name)))
    name;
  name

let _short_name (short : string) =
  if String.length short != 1 then
    raise (Invalid_argument (Printf.sprintf "Invalid short name: '%s'" short))
  else
    match short.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' -> short.[0]
    | _ ->
        raise
          (Invalid_argument (Printf.sprintf "Invalid short name: '%s'" short))

let _parse_name (name : string) =
  match String.split_on_char '|' name with
  | [ name ] -> (_validate_name name, Option.none)
  | [ short; name ] -> (_validate_name name, Option.some (_short_name short))
  | _ -> raise (Invalid_argument (Printf.sprintf "Invalid name: '%s'" name))

let _parse_depends_on (depends_on : string) =
  if depends_on = "" then []
  else
    let depends_on = String.split_on_char ' ' depends_on in
    List.map get depends_on

let register (name : string) (description : string) (action : action)
    (depends_on : string) =
  let name, short = _parse_name name
  and depends_on = _parse_depends_on depends_on in
  Hashtbl.add _tasks name { name; short; description; depends_on; action }

let rec run (name : string) =
  let task = get name in
  List.iter (fun task -> run task.name) task.depends_on;
  task.action ()
