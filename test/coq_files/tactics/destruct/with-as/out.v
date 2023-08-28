Inductive bin : Type :=
  | Z
  | B_0 (n : bin)
  | B_1 (n : bin).

Theorem bin_to_nat_pres_incrr :
  forall (b : bin), bin_to_nat (incr b) = 1 + bin_to_nat b.
Proof.
  intros b.
  induction b as [ | b' IHb' | b' IHb'].
Abort.
