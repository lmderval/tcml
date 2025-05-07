# TC-ML
A Tiger Compiler implementation in OCaml based on the Andrew Appel book 'Modern
Compiler Implementation in ML'.

## Setup
The use the `nix` package manager is highly recommended. A `flake.nix` is
already written to be able to build and develop TC-ML.

To open a new development shell with all required tools and packages, you can
run the following command
```sh
nix develop .#tcml
```

A `.helix` configuration directory is already given if you use helix as your
code editor.

## How To Build?

### By using `nix build`
You can compile TC-ML by running the following command.
```sh
nix build .#tcml
```
The `main.exe` executable can then be found inside `result/bin`.

### By using `nix develop`
In development mode, it is recommended to run the following command to enter a
development shell inside which `ocaml` and `dune` are available to build the
project.
```sh
nix develop .#tcml
```
You can then build TC-ML by running
```sh
dune build
```
The `main.exe` executable is therefore located inside `_build/default/bin`.
