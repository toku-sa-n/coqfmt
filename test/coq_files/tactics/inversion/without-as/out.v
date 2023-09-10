Theorem foo : forall (n : nat), n = n.
  intros n.
  inversion n.
Abort.
