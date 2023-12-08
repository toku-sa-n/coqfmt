Theorem foo : forall (n : nat), n = n.
Proof.
  try (destruct n; [reflexivity | reflexivity]).
Qed.
