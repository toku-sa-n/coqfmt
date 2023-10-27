Definition foo (bar : bool * bool * bool) :=
  match bar with
  | (true, true, true) => true
  | (false, false, false) => true
  | _ => false
  end.
