#!/bin/bash

opam exec -- coqc -Q . X Foo.v
OCAMLRUNPARAM=b opam exec -- dune exec ../../bin/main.exe -- -Q . X < Bar.v > Bar.output.v 
diff Bar.formatted.v Bar.output.v
