Fixpoint add(n:nat)(m:nat):nat:=match n with 0=>m|S n'=>S(add n' m)end.
