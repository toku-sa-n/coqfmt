#!/bin/bash

opam exec -- coqc -Q . X foo.v

# Note that the working directory of the following command is
# `_build/default/test/`.
OCAMLRUNPARAM=b opam exec -- dune exec ../../bin/main.exe -- -Q ../../test/require-test X bar.v < bar.v > bar.output.v 
diff bar.formatted.v bar.output.v
