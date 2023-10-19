Fixpoint eqb (n m : nat) : bool :=
  match n, m with
  | O, O => true
  | S n', S m' => eqb n' m'
  | _, _ => false
  end.

Notation "x =? y" := (eqb x y) (at level 70) : nat_scope.

Theorem foo : forall (n m : nat), (n =? m) = true.
Proof.
  intros n m.
Abort.
