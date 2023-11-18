Theorem foo : 1 = 1.
Proof.
  Fail rewrite -> bar with (a := 1) (b := 1) (c := 1) (d := 1) (e := 1) (f := 1) (g := 1) (h := 1).
Abort.
