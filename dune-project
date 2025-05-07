(lang dune 3.15)

(name tcml)

(generate_opam_files true)

(source
 (github lmderval/tcml))

(authors "Louis-Maël DERVAL")

(maintainers "Louis-Maël DERVAL")

(license MIT)

(documentation https://github.com/lmderval/tcml)

(package
 (name tcml)
 (synopsis "A Tiger Compiler in OCaml")
 (description
  "Implementation of a Tiger Compiler based on the Andrew Appel book 'Modern Compiler Implementation in ML'")
 (depends ocaml dune)
 (tags
  (tiger compiler ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
