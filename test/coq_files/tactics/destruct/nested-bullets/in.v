Theorem or_comm:forall(x y:bool),orb x y=orb y x. Proof. destruct x. -destruct y. +reflexivity. +reflexivity. -destruct y. +reflexivity. +reflexivity. Qed.
