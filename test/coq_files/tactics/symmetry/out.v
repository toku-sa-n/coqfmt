Theorem foo : 1 = 1 -> 1 = 1.
Proof.
  intros H.
  symmetry in H.
Abort.
