Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Example trans_eq_example'' :
  forall (a b c d e f : nat),
    [a; b] = [c; d] -> [c; d] = [e; f] -> [a; b] = [e; f].
Proof.
  intros a b c d e f eq_1 eq_2.
  transitivity [c; d].
  apply eq_1.
  apply eq_2.
Qed.
