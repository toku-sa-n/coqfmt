Ltac foo :=
  match goal with
  | H : ?E = true |- _ =>
    match goal with
    | H : ?E = true |- _ => idtac
    end
  end.
