Reserved Notation "x !! y"(at level 40,left associativity).
Reserved Notation "x ?? y"(at level 40,left associativity).
Fixpoint foo (x y:nat):=x!!y where "x !! y" := (x+y) and "x ?? y":=(x-y).
