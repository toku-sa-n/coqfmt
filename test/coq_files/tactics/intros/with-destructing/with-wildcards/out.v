Theorem foo : forall (n : nat), n = n.
Proof.
  intros [ | _].
Abort.
