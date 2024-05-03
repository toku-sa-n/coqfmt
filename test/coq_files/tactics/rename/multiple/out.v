Theorem foo : 1 = 1 -> 1 = 1 -> 1 = 1.
Proof.
  intros H1 H2.
  rename H1 into X, H2 into Y.
Abort.
