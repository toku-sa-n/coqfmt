Theorem foo : forall n, exists m, n + 1 = m.
Proof.
  intros n.
  exists (n + 1).
  reflexivity.
Qed.
