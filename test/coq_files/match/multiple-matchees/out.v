Definition allzero (x y: nat) : bool :=
  match x, y with
  | O, O => true
  | _ => false
  end.
