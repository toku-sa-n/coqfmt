Inductive bin : Type := | Z | B_0 (n : bin) | B_1 (n : bin).
Fixpoint bin_to_nat (m : bin) : nat := match m with | Z => 0 | B_0 n => 2 * bin_to_nat n | B_1 n => 2 * bin_to_nat n + 1 end.
Fixpoint incr (m : bin) : bin := match m with | Z => B_1 Z | B_0 n => B_1 n | B_1 n => B_0 (incr n) end.
Theorem bin_to_nat_pres_incrr : forall (b : bin), bin_to_nat (incr b) = 1 + bin_to_nat b.  Proof.  intros b.  induction b as [ | b' IHb'| b' IHb']. Abort.
