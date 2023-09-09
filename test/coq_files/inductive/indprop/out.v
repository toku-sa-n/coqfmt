Inductive reaches_1 : nat -> Prop :=
  | term_done : reaches_1 1
  | term_more : forall (n : nat), reaches_1 (f n) -> reaches_1 n.
