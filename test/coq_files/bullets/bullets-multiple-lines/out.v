Theorem foo : forall (n : nat), n = n.
Proof.
  destruct n.
    - simpl.
      reflexivity.
    - simpl.
      reflexivity.
Qed.
