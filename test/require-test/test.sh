#!/bin/bash

opam exec -- coqc -Q . X Foo.v

# Note that the working directory of the following command is
# `_build/default/test/`.
OCAMLRUNPARAM=b opam exec -- dune exec ../../bin/main.exe -- -Q ../../test/require-test X < Bar.v > Bar.output.v 
diff Bar.formatted.v Bar.output.v
