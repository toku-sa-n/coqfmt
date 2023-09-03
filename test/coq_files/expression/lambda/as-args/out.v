Definition doit3times {X : Type} (f : X -> X) (n : X) : X := f (f (f n)).

Example test_anon_fun' : doit3times (fun n => n * n) 2 = 256.
Abort.
