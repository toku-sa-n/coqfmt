Theorem foo : forall (n : nat), n = n.
Proof.
  destruct n;
  [simpl; simpl; simpl; simpl; simpl; simpl; simpl; simpl; simpl; simpl | simpl;
   simpl; simpl; simpl; simpl; simpl; simpl; simpl; simpl; simpl]; reflexivity.
Qed.
