Require Coq.extraction.Extraction.

Extraction Language OCaml.

Extraction "imp1.ml" pred.

Extract Constant plus => "( + )".
Extract Constant mult => "( * )".

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive nat => "int" ["0" "(fun x->x+1)"]
  "(fun zero succ n->if n=0 then zero() else succ (n-1))".

From Coq Require Import ZArith.
