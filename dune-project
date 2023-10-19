(lang dune 3.9)

(name coqfmt)

(generate_opam_files true)

(source
 (github toku-sa-n/coqfmt))

(authors "Hiroki Tokunaga <tokusan441@gmail.com>")

(maintainers "Hiroki Tokunaga <tokusan441@gmail.com>")

(license AGPL-3.0-or-later)

(version 0.1.0)

(package
 (name coqfmt)
 (synopsis "Coq code formatter")
 (description "Coqfmt formats the given Coq source code in a uniform style.")
 (depends
  (alcotest
   (and :with-test))
  (coq
   (= 8.18.0))
  coq-core
  coq-serapi
  dune
  dune-build-info
  ocaml))
