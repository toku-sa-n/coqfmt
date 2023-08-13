Fixpoint inc(n:nat):nat:=match n with |O=>S O |S n'=>S (inc n') end.

Fixpoint inc'(n:nat):nat:=match n with |O=>S O |S n'=>S (inc' n') end.
