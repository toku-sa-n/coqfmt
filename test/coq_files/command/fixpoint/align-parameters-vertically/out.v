Fixpoint build_proof
  (P : nat -> Prop)
  (evP0 : P 0)
  (evPS : forall (n : nat), P n -> P (S n))
  (n : nat) : P n :=
  match n with
  | 0 => evP0
  | S n' => evPS n' (build_proof P evP0 evPS n')
  end.
