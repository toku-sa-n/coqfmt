Ltac foo:=match goal with | |- context [if _ then _ else _] => idtac end.
