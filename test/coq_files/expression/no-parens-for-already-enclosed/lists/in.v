From Coq Require Import List. Import ListNotations. Definition id{A}(x:A):=x. Theorem foo:id [1;2;3]=[1;2;3]. Abort.
