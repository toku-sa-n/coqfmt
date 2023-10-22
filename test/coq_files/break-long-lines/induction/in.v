Theorem foo : 1 = 1.
Proof.
  Fail induction foo as [derich rosemary | ester menyanya shinobu |  | takoyaki ramen].
  Fail induction foo as
    [ | derich rosemary | ester menyanya shinobu | | dina alfred | zenyatta iris | mao marion | ].
Abort.
