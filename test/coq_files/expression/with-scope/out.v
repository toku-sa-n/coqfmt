Require Export Coq.Strings.String.

Check String.eqb_refl%string.
Check String.eqb_refl : forall (x : string), (x =? x)%string = true.
