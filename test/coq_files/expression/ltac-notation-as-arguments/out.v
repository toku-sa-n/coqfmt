From Coq Require Import Arith.Arith.

Ltac foo x := destruct x.

Example foo : 1 = 1.
Proof.
  foo (1 <? 1).
Abort.
