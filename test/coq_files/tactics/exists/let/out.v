Goal exists (x : nat), x = x.
  exists (let y := 1 in
          y).
  reflexivity.
Qed.
