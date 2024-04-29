Ltac foo :=
  match goal with
  | H : ?T |- _ =>
    match type of T with
    _ =>idtac
    end
  end.
