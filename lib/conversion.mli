val raw_tactic_expr_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Ltac_plugin.Tacexpr.raw_tactic_expr option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Ltac_plugin.Tacexpr.raw_tactic_expr]. *)

val tacdef_body_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Ltac_plugin.Tacexpr.tacdef_body list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Ltac_plugin.Tacexpr.tacdef_body list]. *)

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

val id_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Names.Id.t option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Names.Id.t]. *)

val hyp_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Names.lident list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Names.lident list]. *)

val nat_or_var_of_raw_generic_argument :
  Genarg.raw_generic_argument -> int Locus.or_var list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [int Locus.or_var list]. *)

val auto_using_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Constrexpr.constr_expr list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Constrexpr.constr_expr list]. *)

val ltac_use_default_of_raw_generic_argument :
  Genarg.raw_generic_argument -> bool option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [bool]. *)

val ltac_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Ltac_plugin.Tacexpr.raw_tactic_expr option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Ltac_plugin.Tacexpr.raw_tactic_expr]. *)

val hintbases_of_raw_generic_argument :
  Genarg.raw_generic_argument -> string list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [string list]. *)

val lang_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Extraction_plugin.Table.lang option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Extraction_plugin.Table.lang]. *)

val string_of_raw_generic_argument :
  Genarg.raw_generic_argument -> string option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [string]. *)

val ref_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Libnames.qualid option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Libnames.qualid]. *)

val ref_list_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Libnames.qualid list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Libnames.qualid list]. *)

val mlname_of_raw_generic_argument :
  Genarg.raw_generic_argument -> string option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [string] *)

val mlname_list_of_raw_generic_argument :
  Genarg.raw_generic_argument -> string list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [string list]. *)

val opt_string_of_raw_generic_argument :
  Genarg.raw_generic_argument -> string option option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [string option]. *)

val by_arg_tac_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Ltac_plugin.Tacexpr.raw_tactic_expr list option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Ltac_plugin.Tacexpr.raw_tactic_expr list]. *)

val clause_dft_concl_of_raw_generic_argument :
  Genarg.raw_generic_argument -> Names.variable CAst.t Locus.clause_expr option
(** Convert the given value of [Genarg.raw_generic_argument] to a value of
  [Names.variable CAst.t Locus.clause_expr]. *)

val constr_with_bindings_of_raw_generic_argument :
  Genarg.raw_generic_argument ->
  Constrexpr.constr_expr Tactypes.with_bindings option
