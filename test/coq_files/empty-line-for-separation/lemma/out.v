Lemma foo : 1 = 1.
Abort.

Ltac foo := simpl.
Ltac bar := simpl.

Lemma baz : 1 = 1.
Abort.
