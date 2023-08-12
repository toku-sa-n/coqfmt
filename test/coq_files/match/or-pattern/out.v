Inductive foo : Type :=
  | A
  | B
  | C
  | D
  | E
  | F.

Definition bar (x : foo) : nat :=
  match x with
  | A => 1
  | B | C => 2
  | D | E | F => 3
  end.
