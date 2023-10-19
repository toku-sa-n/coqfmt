Fixpoint count n:=match n with 0 => 0 | S n => 1 + count n end.
