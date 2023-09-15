Theorem foo : 1 = 1.
Proof.
  Fail remember a as b.
  Fail remember (Foo a) as b.
Abort.
