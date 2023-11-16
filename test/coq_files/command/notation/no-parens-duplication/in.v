Declare Custom Entry com.
Declare Scope com_scope.

Notation "<{ x }>" := x ( x custom com at level 99) : com_scope.
Notation "x" := x (in custom com at level 0, x constr at level 0) : com_scope.

Fail Definition foo := <{ 1 }>.
