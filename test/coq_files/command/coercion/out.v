Inductive Foo :=
  | Bar (n : nat).

Coercion Bar : nat >-> Foo.
