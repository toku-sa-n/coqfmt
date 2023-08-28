Theorem add_shuffle3' : forall (n m p : nat), n + (m + p) = m + (n + p).
Proof.
  intros n m p.
  rewrite -> add_assoc.
  rewrite -> add_assoc.
  replace (n + m) with (m + n).
    - reflexivity.
    - rewrite -> add_comm.
      reflexivity.
Qed.
