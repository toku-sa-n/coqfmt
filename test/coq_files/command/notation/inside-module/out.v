Comments "See https://github.com/toku-sa-n/coqfmt/blob/a6d6191ea703812a36d76f796002882a8fc97036/lib/ppast.ml#L1284-L1287 about this test".

Module Foo.
  Fixpoint eqb (n m : nat) : bool :=
    match n, m with
    | O, O => true
    | S n', S m' => eqb n' m'
    | _, _ => false
    end.

  Notation "x =? y" := (eqb x y) (at level 70) : nat_scope.

  Theorem foo : forall (x y : nat), (x =? y) = true.
  Proof.
    intros x y.
    destruct (x =? y).
  Abort.
End Foo.
