Declare Custom Entry com.
Declare Scope com_scope.

Notation "<{ e }>" := e (at level 0, e custom com at level 99) : com_scope.
Notation "( x )" := x (in custom com, x at level 99) : com_scope.
Notation "x" := x (in custom com at level 0, x constr at level 0) : com_scope.
Notation "f x .. y" :=
  (.. (f x) .. y) (in custom com at level 0,
                   only parsing,
                   f constr at level 0,
                   x constr at level 9,
                   y constr at level 9) : com_scope.
Notation "x + y" := (x + y) (in custom com at level 50, left associativity).

Definition foo (x y z : nat) := x + y + z.

Open Scope com_scope.

Definition bar := <{ (foo 1 2 3) + (foo 4 5 6) }>.
