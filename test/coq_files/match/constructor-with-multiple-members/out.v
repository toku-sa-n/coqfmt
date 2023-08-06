Definition or (xy: prod bool) :=
  match xy with
  | (true, true) => true
  | _ => false
  end.
