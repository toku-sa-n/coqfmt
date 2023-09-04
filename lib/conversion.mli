val raw_tactic_expr_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Ltac_plugin.Tacexpr.raw_tactic_expr option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Ltac_plugin.Tacexpr.raw_tactic_expr]. *)
