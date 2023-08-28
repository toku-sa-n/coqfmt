Theorem foo:forall(m n:nat),m=n->n=m. Proof. intros m n H. replace m with n. -reflexivity. -reflexivity. Qed.
