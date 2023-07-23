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
 (depends
  ocaml
  alcotest
  coq ; The `coqfmt` library does not depend on `coq` directly but modules in 
      ; `coq-core`. However, without the `coq` dependency, building `coqfmt` 
      ; fails due to `cannot guess a path for Coq libraries;` error.
  dune))
