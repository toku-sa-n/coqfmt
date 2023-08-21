Definition allzero (n m : nat) :=
  match m with
  | O =>
    match n with
    | O => true
    | S _ => false
    end
  | _ => false
  end.
