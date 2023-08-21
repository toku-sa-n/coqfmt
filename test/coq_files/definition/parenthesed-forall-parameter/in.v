Theorem foo:forall(f:nat->nat),(forall(x:nat),f x=x)->forall(x:nat),f(x)=x. Proof. intros f H x. rewrite H. reflexivity. Qed.
