Notation"[ x ; .. ; y ]":=(cons x..(cons y nil)..).

Example trans_eq : forall (X : Type) (n m o : X), n = m -> m = o -> n = o.  Proof.  intros X n m o eq1 eq2.  rewrite -> eq1.  rewrite -> eq2.  reflexivity.  Qed.

Example trans_eq_example' : forall (a b c d e f : nat), [a; b] = [c; d] -> [c; d] = [e; f] -> [a; b] = [e; f].  Proof.  intros a b c d e f eq_1 eq_2.  apply trans_eq with (m:=[c;d]).  apply eq_1.  apply eq_2.  Qed.
