Theorem foo : forall (n : nat), n = n.
Proof.
  destruct n.
    * reflexivity.
    * reflexivity.
Qed.
