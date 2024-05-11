From Coq Require Import Arith.Arith.
Definition geb(n m:nat):=m<=?n.
Infix ">=?":=geb(at level 70):nat_scope.
