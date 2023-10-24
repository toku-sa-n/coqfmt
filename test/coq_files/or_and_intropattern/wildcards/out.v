Theorem foo : forall P Q, P /\ Q -> P.
Proof.
  intros P Q [HP _].
Abort.
