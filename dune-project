(lang dune 3.9)

(name coqfmt)

(generate_opam_files true)

(source
 (github toku-sa-n/coqfmt))

(authors "Hiroki Tokunaga <tokusan441@gmail.com>")

(maintainers "Hiroki Tokunaga <tokusan441@gmail.com>")

(license AGPL-3.0-or-later)

(package
 (name coqfmt)
 (synopsis "Coq code formatter")
 (description "Coqfmt formats the given Coq source code in a uniform style.")
 (depends ocaml dune))
