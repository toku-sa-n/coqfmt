Require Coq.extraction.Extraction.

Extract Inductive nat => "int" ["0" "(fun x->x+1)"] "(fun zero succ n->if n=0 then zero() else succ (n-1))".
