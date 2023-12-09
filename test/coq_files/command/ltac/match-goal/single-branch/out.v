Ltac find_rwd :=
  match goal with
  | H1 : ?E = true,
    H2 : ?E = false
    |- _ => rewrite -> H1 in H2; discriminate
  end.
