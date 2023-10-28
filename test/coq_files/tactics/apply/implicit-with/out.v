Theorem foo : 1 = 1.
Proof.
  Fail apply foo with bar.
  Fail apply foo with (S n).
Abort.
