val raw_tactic_expr_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Ltac_plugin.Tacexpr.raw_tactic_expr option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Ltac_plugin.Tacexpr.raw_tactic_expr]. *)

val constr_expr_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Constrexpr.constr_expr option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Constrexpr.constr_expr]. *)

val intro_pattern_list_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Ltac_plugin.Tacexpr.intro_pattern list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
    [Ltac_plugin.Tacexpr.intro_pattern list]. *)

val destruction_arg_of_raw_generic_argument :
  Genarg.raw_generic_argument ->
  Constrexpr.constr_expr Tactypes.with_bindings Tactics.destruction_arg option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Genarg.destruction_arg]. *)

val clause_expr_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Names.Id.t CAst.t Locus.clause_expr option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Names.Id.t CAst.t Locus.clause_expr]. *)

val bindings_list_of_raw_generic_argument :
  Genarg.raw_generic_argument ->
  Constrexpr.constr_expr Tactypes.bindings list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Constrexpr.constr_expr Tactypes.bindings list]. *)
