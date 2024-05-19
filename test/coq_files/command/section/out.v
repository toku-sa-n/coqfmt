Section Foo.
  Definition x : nat := 3.
End Foo.

Section Bar.
  Definition y : nat := 5.

  Section Baz.
    Definition z : nat := 7.
  End Baz.
End Bar.

Definition w : nat := 3.
