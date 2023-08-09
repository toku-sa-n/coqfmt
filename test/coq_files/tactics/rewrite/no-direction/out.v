Theorem foo : forall (n m : nat), n = m -> n = m.
Proof.
  intros n m H.
  rewrite -> H.
  reflexivity.
Qed.
