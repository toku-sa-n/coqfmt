Theorem foo : forall (n : nat), n = n.
Proof.
  intros n.
  induction n as [ | n' I].
    - reflexivity.
    - reflexivity.
Qed.
