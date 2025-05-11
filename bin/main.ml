open Tcml

let hello () = Printf.printf "Hello!\n"
let world () = Printf.printf "World!\n"
let foo () = Printf.printf "foo\n"
let bar () = Printf.printf "bar\n"

let () =
  Tasks.register "hello" "Print 'Hello!' on standard output" hello "";
  Tasks.register "world" "Print 'World!' on standard output" world "hello";
  Tasks.register "f|foo" "Foo" foo "";
  Tasks.register "b|bar" "Bar" bar "";
  Tasks.register "foobar" "Foobar" (fun () -> ()) "foo bar";
  Tasks.register "A|all" "Run all tasks" (fun () -> ()) "hello foobar world";
  Tasks.run "all"
