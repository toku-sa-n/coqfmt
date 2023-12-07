Ltac rwd H1 H2:=rewrite H1 in H2; discriminate. Theorem foo:1=1. Proof. Fail rwd H1 H2. Abort.
