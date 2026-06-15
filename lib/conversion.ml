let raw_arg wit arg =
  let wit = Genarg.rawwit wit in
  if Genarg.has_type arg wit then Some (Genarg.out_gen wit arg) else None

let raw_tactic_expr_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.Tacarg.wit_tactic arg

let tacdef_body_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_list Ltac_plugin.G_ltac.wit_ltac_tacdef_body) arg

let constr_expr_of_raw_generic_argument arg =
  match raw_arg Stdarg.wit_constr arg with
  | Some _ as expr -> expr
  | None -> raw_arg Stdarg.wit_uconstr arg

let intro_pattern_list_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_list Ltac_plugin.Tacarg.wit_simple_intropattern) arg

let intro_pattern_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.Tacarg.wit_simple_intropattern arg

let destruction_arg_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.Tacarg.wit_destruction_arg arg

let clause_expr_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.Extraargs.wit_in_clause arg

let bindings_list_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_list Ltac_plugin.Tacarg.wit_bindings) arg

let id_of_raw_generic_argument arg = raw_arg Stdarg.wit_ident arg

let hyp_list_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_list Stdarg.wit_hyp) arg

let hyp_of_raw_generic_argument arg = raw_arg Stdarg.wit_hyp arg

let nat_or_var_of_raw_generic_argument arg =
  match raw_arg (Genarg.wit_opt Stdarg.wit_nat_or_var) arg with
  | Some None -> Some []
  | Some (Some var) -> Some [ var ]
  | None -> (
      match raw_arg (Genarg.wit_list Stdarg.wit_nat_or_var) arg with
      | Some _ as vars -> vars
      | None -> Option.map (fun var -> [ var ]) (raw_arg Stdarg.wit_nat_or_var arg))

let auto_using_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.G_auto.wit_auto_using arg

let ltac_use_default_of_raw_generic_argument arg =
  Option.map (fun value -> value.CAst.v) (raw_arg Ltac_plugin.G_ltac.wit_ltac_use_default arg)

let ltac_of_raw_generic_argument arg = raw_arg Ltac_plugin.Tacarg.wit_ltac arg

let hintbases_of_raw_generic_argument arg =
  Option.map
    (function None -> [] | Some hintbases -> hintbases)
    (raw_arg Ltac_plugin.G_auto.wit_hintbases arg)

let lang_of_raw_generic_argument arg =
  raw_arg Extraction_plugin.G_extraction.wit_language arg

let string_of_raw_generic_argument arg = raw_arg Stdarg.wit_string arg

let ref_of_raw_generic_argument arg = raw_arg Stdarg.wit_ref arg

let ref_list_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_list Stdarg.wit_ref) arg

let mlname_of_raw_generic_argument arg =
  raw_arg Extraction_plugin.G_extraction.wit_mlname arg

let mlname_list_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_list Extraction_plugin.G_extraction.wit_mlname) arg

let opt_string_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_opt Stdarg.wit_string) arg

let by_arg_tac_of_raw_generic_argument arg =
  Option.map
    (function None -> [] | Some tactic -> [ tactic ])
    (raw_arg Ltac_plugin.Extraargs.wit_by_arg_tac arg)

let clause_dft_concl_of_raw_generic_argument arg =
  raw_arg Stdarg.wit_clause_dft_concl arg

let constr_with_bindings_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.Tacarg.wit_constr_with_bindings arg

let rename_idents_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_list Ltac_plugin.Extraargs.wit_rename) arg

let ltac_production_item_of_raw_generic_argument arg =
  raw_arg
    (Genarg.wit_list Ltac_plugin.G_ltac.wit_ltac_production_item)
    arg

let ltac_tactic_level_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_opt Ltac_plugin.G_ltac.wit_ltac_tactic_level) arg

let test_lpar_id_colon_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.Extraargs.wit_test_lpar_id_colon arg

let lconstr_of_raw_generic_argument arg =
  raw_arg Ltac_plugin.Extraargs.wit_lconstr arg

let function_fix_definition_of_raw_generic_argument arg =
  raw_arg
    (Genarg.wit_list Funind_plugin.G_indfun.wit_function_fix_definition)
    arg

let ltac_selector_of_raw_generic_argument arg =
  raw_arg (Genarg.wit_opt Ltac_plugin.G_ltac.wit_ltac_selector) arg
