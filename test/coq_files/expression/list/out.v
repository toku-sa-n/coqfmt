Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Definition foo := [1; 2; 3] ++ [4; 5] ++ [6].
