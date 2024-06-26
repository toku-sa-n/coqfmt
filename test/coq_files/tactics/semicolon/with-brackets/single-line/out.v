Theorem foo : forall n, n = n + 0.
Proof.
  intros n; induction n as [ | n IHn]; [idtac | simpl; rewrite <- IHn; simpl];
  reflexivity.
Qed.
