Theorem foo : (forall (n : nat), n = n) -> 1 = 1.
Proof.
  intros.
  specialize H with 1.
  apply H.
Qed.
