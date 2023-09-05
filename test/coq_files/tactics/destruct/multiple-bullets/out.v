Theorem foo : forall (a b c : bool), a = b -> b = c.
Proof.
  destruct a.
    -- destruct b.
         ++ destruct c.
              ** simpl.
Abort.
