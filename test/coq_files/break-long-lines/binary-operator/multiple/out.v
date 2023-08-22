Theorem foo :
  forall (tokyo osaka sapporo kobe fukuoka nagoya : nat),
    tokyo = osako
      -> osaka = sapporo
      -> sapporo = kobe
      -> kobe = fukuoka
      -> fukuoka = nagoya
      -> nagoya = tokyo.
Proof.
Abort.
