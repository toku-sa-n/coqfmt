Inductive foo : Type :=
  | bar (x : nat) (y : nat).

Definition allzero (x : foo) : bool :=
  match x with
  | bar O O => true
  | _ => false
  end.
