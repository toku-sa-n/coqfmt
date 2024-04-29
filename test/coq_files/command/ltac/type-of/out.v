Ltac foo :=
  match goal with
  | H : ?T |- _ =>
    match type of T with
    | Prop => idtac "Prop"
    | _ => idtac
    end
  end.
