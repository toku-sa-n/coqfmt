Definition id {X : Type} (x : X) := x.

Theorem id_id : forall (X : Type) (x : X), @id X x = @id X x.
Proof.
  reflexivity.
Qed.
