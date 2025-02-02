From Coq Require Import Recdef.
Function foo (x:nat){measure id x}:nat:=x. Proof. Defined.
