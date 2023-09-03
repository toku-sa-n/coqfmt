Theorem foo : forall (n m p : nat), n = m -> m = p -> n = p.
Proof.
  intros n m p H I.
  transitivity m.
Abort.
