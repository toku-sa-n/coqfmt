#!/bin/bash

coqc -Q . X foo.v
OCAMLRUNPARAM=b dune exec ../../bin/main.exe -- -Q ../../test/require-test X bar.v < bar.v > bar.output.v 
diff bar.formatted.v bar.output.v
