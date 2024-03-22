Definition foo (n : nat) := n.

Coercion foo : nat >-> nat.

Add Printing Coercion foo foo.
