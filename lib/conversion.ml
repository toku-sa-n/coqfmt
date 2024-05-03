(* XXX: I'm not sure if these ways are correct. See
   https://coq.zulipchat.com/#narrow/stream/256331-SerAPI/topic/Parsing.20a.20value.20in.20a.20.60GenArg.60. *)

open Sexplib.Sexp

let raw_tactic_expr_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "tactic" ] ];
        rems;
      ] ->
      Some (Serlib_ltac.Ser_tacexpr.raw_tactic_expr_of_sexp rems)
  | _ -> None

let tacdef_body_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List
              [
                Atom "ListArg";
                List [ Atom "ExtraArg"; Atom "ltac_tacdef_body" ];
              ];
          ];
        List rems;
      ] ->
      Some (List.map Serlib_ltac.Ser_tacexpr.tacdef_body_of_sexp rems)
  | _ -> None

let constr_expr_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "constr" ] ];
        rems;
      ]
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "uconstr" ] ];
        rems;
      ] ->
      Some (Serlib.Ser_constrexpr.constr_expr_of_sexp rems)
  | _ -> None

let intro_pattern_list_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List
              [
                Atom "ListArg";
                List [ Atom "ExtraArg"; Atom "simple_intropattern" ];
              ];
          ];
        List rems;
      ] ->
      Some (List.map Serlib_ltac.Ser_tacexpr.intro_pattern_of_sexp rems)
  | _ -> None

let destruction_arg_of_raw_generic_argument arg =
  let open Sexplib.Sexp in
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "destruction_arg" ] ];
        rems;
      ] ->
      let arg_parser = function
        | List [ _; expr ] ->
            (Serlib.Ser_constrexpr.constr_expr_of_sexp expr, Tactypes.NoBindings)
        | _ -> failwith "TODO"
      in
      Some (Serlib.Ser_tactics.destruction_arg_of_sexp arg_parser rems)
  | _ -> None

let clause_expr_of_raw_generic_argument arg =
  let open Sexplib.Sexp in
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "in_clause" ] ];
        rems;
      ] ->
      Some
        (Serlib.Ser_locus.clause_expr_of_sexp
           (Serlib.Ser_cAst.t_of_sexp Serlib.Ser_names.Id.t_of_sexp)
           rems)
  | _ -> None

let bindings_list_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List [ Atom "ListArg"; List [ Atom "ExtraArg"; Atom "bindings" ] ];
          ];
        List rems;
      ] ->
      Some
        (List.map
           (Serlib.Ser_tactypes.bindings_of_sexp
              Serlib.Ser_constrexpr.constr_expr_of_sexp)
           rems)
  | _ -> None

let id_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "ident" ] ];
        rems;
      ] ->
      Some (Serlib.Ser_names.Id.t_of_sexp rems)
  | _ -> None

let hyp_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List [ Atom "ListArg"; List [ Atom "ExtraArg"; Atom "hyp" ] ];
          ];
        List rems;
      ] ->
      Some
        (List.map
           (Serlib.Ser_cAst.t_of_sexp Serlib.Ser_names.Id.t_of_sexp)
           rems)
  | _ -> None

let nat_or_var_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List [ Atom "OptArg"; List [ Atom "ExtraArg"; Atom "nat_or_var" ] ];
          ];
        List rems;
      ] ->
      Some
        (List.map
           (Serlib.Ser_locus.or_var_of_sexp Sexplib.Std.int_of_sexp)
           rems)
  | _ -> None

let auto_using_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "auto_using" ] ];
        List rems;
      ] ->
      Some (List.map Serlib.Ser_constrexpr.constr_expr_of_sexp rems)
  | _ -> None

let ltac_use_default_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "ltac_use_default" ] ];
        rems;
      ] ->
      Some (Sexplib.Std.bool_of_sexp rems)
  | _ -> None

let ltac_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "ltac" ] ];
        rems;
      ] ->
      Some (Serlib_ltac.Ser_tacexpr.raw_tactic_expr_of_sexp rems)
  | _ -> None

let hintbases_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "hintbases" ] ];
        List [ List rems ];
      ] ->
      Some (List.map Sexplib.Std.string_of_sexp rems)
  | _ -> None

let lang_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "language" ] ];
        rems;
      ] ->
      Some (Serlib_extraction.Ser_g_extraction.WitL.raw_of_sexp rems)
  | _ -> None

let string_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "string" ] ];
        rems;
      ] ->
      Some (Sexplib.Std.string_of_sexp rems)
  | _ -> None

let ref_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "ref" ] ];
        rems;
      ] ->
      Some (Serlib.Ser_libnames.qualid_of_sexp rems)
  | _ -> None

let ref_list_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List [ Atom "ListArg"; List [ Atom "ExtraArg"; Atom "ref" ] ];
          ];
        List rems;
      ] ->
      Some (List.map Serlib.Ser_libnames.qualid_of_sexp rems)
  | _ -> None

let mlname_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "mlname" ] ];
        rems;
      ] ->
      Some (Serlib_extraction.Ser_g_extraction.WitMN.t_of_sexp rems)
  | _ -> None

let mlname_list_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List [ Atom "ListArg"; List [ Atom "ExtraArg"; Atom "mlname" ] ];
          ];
        List rems;
      ] ->
      Some (List.map Serlib_extraction.Ser_g_extraction.WitMN.t_of_sexp rems)
  | _ -> None

let opt_string_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List [ Atom "OptArg"; List [ Atom "ExtraArg"; Atom "string" ] ];
          ];
        rems;
      ] ->
      Some (Sexplib.Std.option_of_sexp Sexplib.Std.string_of_sexp rems)
  | _ -> None

let by_arg_tac_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "by_arg_tac" ] ];
        List rems;
      ] ->
      Some (List.map Serlib_ltac.Ser_tacexpr.raw_tactic_expr_of_sexp rems)
  | _ -> None

let clause_dft_concl_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "clause_dft_concl" ] ];
        rems;
      ] ->
      Some
        (Serlib.Ser_locus.clause_expr_of_sexp
           (Serlib.Ser_cAst.t_of_sexp Serlib.Ser_names.Id.t_of_sexp)
           rems)
  | _ -> None

let constr_with_bindings_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "constr_with_bindings" ];
          ];
        rems;
      ] ->
      Some
        (Serlib.Ser_tactypes.with_bindings_of_sexp
           Serlib.Ser_constrexpr.constr_expr_of_sexp rems)
  | _ -> None

let rename_idents_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List [ Atom "ListArg"; List [ Atom "ExtraArg"; Atom "rename" ] ];
          ];
        List rems;
      ] ->
      Some
        (List.map
           (Sexplib.Conv.pair_of_sexp Serlib.Ser_names.Id.t_of_sexp
              Serlib.Ser_names.Id.t_of_sexp)
           rems)
  | _ -> None
