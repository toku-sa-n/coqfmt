Comments "TODO: Remove spaces around the parentheses.".

Ltac foo :=
  let x := fresh in
  evar ( x : Prop ).
