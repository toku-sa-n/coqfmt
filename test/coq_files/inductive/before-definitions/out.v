Inductive foo: Type :=
  | bar
  | baz.

Definition isbar (x: foo) : bool :=
  match x with
  | bar => true
  | baz => false
  end.
