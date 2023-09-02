Fixpoint repeat''' {X : Type} (x : X) (count : nat) : list X := match count with | O => nil | S count' => cons x (repeat''' x count') end.
