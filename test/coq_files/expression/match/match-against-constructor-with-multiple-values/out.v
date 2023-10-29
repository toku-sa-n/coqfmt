Inductive foo : Type :=
  | bar (x : nat) (y : nat).

Definition allzero (x : foo) : bool :=
  match x with
  | bar O O => true
  | bar 0 1 => true
  | _ => false
  end.
