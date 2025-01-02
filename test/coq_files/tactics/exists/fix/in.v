Goal exists (f:nat->nat),f 0=1. exists (fix f x:=match x with O=>1|S _=>1 end). reflexivity. Qed.
