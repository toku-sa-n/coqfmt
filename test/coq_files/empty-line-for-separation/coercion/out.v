Inductive foo : Type :=
  | Bar (x : nat)
  | Baz (x : bool).

Coercion Bar : nat >-> foo.
Coercion Baz : bool >-> foo.

Declare Custom Entry hoge.
