Reserved Notation "x !! y" (at level 50, left associativity).

Inductive foo : nat -> nat -> Type :=
  | bar : 0 !! 0
  where
    "x !! y" := (foo x y).
