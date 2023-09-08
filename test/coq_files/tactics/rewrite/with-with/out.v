Theorem foo : 1 = 1.
Proof.
  Fail rewrite -> IHl with (l2 := l).
Abort.
