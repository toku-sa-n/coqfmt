Notation "a ; b" := b (at level 90, right associativity).
Notation "a := b" := (a + b) (at level 80, no associativity).

Definition foo := 1 := 3; 2 := 3; 1 := 3.
