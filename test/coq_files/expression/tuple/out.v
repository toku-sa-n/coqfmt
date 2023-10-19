Inductive natprod : Type :=
  | pair (n_1 n_2 : nat).

Notation "( x , y )" := (pair x y).

Definition swap_pair (p : natprod) : natprod :=
  match p with
  | (x, y) => (y, x)
  end.
