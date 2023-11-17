From Coq Require Import Strings.String.

Inductive aexp : Type :=
  | AId (x : string).

Definition X : string := "X".

Coercion AId : string >-> aexp.

Declare Custom Entry com.
Declare Scope com_scope.

Notation "<{ e }>" := e (at level 0, e custom com at level 99) : com_scope.
Notation "( x )" := x (in custom com, x at level 99) : com_scope.
Notation "x" := x (in custom com at level 0, x constr at level 0) : com_scope.

Open Scope com_scope.

Inductive com : Type :=
  | CAsgn (x : string) (a : aexp)
  | CSeq (c1 c2 : com).

Notation "x := y" :=
  (CAsgn x y) (in custom com at level 0,
               x constr at level 0,
               y at level 85,
               no associativity) : com_scope.
Notation "x ; y" :=
  (CSeq x y) (in custom com at level 90, right associativity) : com_scope.

Definition fact_in_coq : com := <{ X := X; X := X; X := X }>.
