Theorem foo : 1 = 1.
Proof.
  Fail rewrite -> (bar 3 4).
Abort.
