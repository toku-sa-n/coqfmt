#!/bin/bash

opam exec -- coq_makefile -f _CoqProject -o Makefile Foo.v Bar.v
opam exec -- make -j
OCAMLRUNPARAM=b opam exec -- dune exec ../../bin/main.exe < Bar.v > Bar.output.v
diff Bar.formatted.v Bar.output.v
