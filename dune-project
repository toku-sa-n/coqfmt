(lang dune 3.9)

(name rocqfmt)

(generate_opam_files true)

(source
 (github toku-sa-n/rocqfmt))

(authors "Hiroki Tokunaga <tokusan441@gmail.com>")

(maintainers "Hiroki Tokunaga <tokusan441@gmail.com>")

(license AGPL-3.0-or-later)

(version 0.1.0)

(package
 (name rocqfmt)
 (synopsis "Coq code formatter")
 (description "Rocqfmt formats the given Coq source code in a uniform style.")
 (depends
  (alcotest :with-test)
  (coq-core
   (= 9.2.0))
  (rocq-core
   (= 9.2.0))
  (rocq-stdlib
   (= 9.1.0))
  dune
  dune-build-info
  ocaml))
