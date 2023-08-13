Fixpoint add (n : nat) (m : nat) : nat :=
  match n with
  | O => m
  | S n' => S (add n' m)
  end.
