Fixpoint eqb (n m : nat) : bool := match n, m with | O, O => true | S n', S m' => eqb n' m' | _, _ => false
  end.

Notation"x =? y":=(eqb x y)(at level 70):nat_scope.

Theorem foo:forall (x y:nat),x=?y=true. Proof. intros x y. destruct (x=?y). Abort.
