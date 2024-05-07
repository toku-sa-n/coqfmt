Inductive foo : nat -> Prop :=
  | bar : forall (n m : nat), foo (n + m)
where "n + m" := (plus n m) : type_scope.
