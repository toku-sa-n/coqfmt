Theorem really_extremely_unnecessarily_too_very_long_name_for_replacing :
  (1 = 1) = (1 = 1).
Proof.
  auto.
Qed.

Theorem foo : 1 = 1.
Proof.
  replace (1 = 1) with (1 = 1)
    by apply really_extremely_unnecessarily_too_very_long_name_for_replacing.
Abort.
