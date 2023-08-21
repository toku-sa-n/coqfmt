Definition xor (xy : prod bool) :=
  match xy with
  | (true, false) => true
  | (false, true) => true
  | _ => false
  end.
