Theorem match_ex2:True/\True. Proof. match goal with |[|-True]=>apply I|[|-True/\True]=>split; apply I end. Qed.
