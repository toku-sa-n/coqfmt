open Printer
open Ltac_plugin

exception NotImplemented of string

let nop _ = ()
let pp_id id = write (Names.Id.to_string id)
let pp_lident CAst.{ v; loc = _ } = pp_id v

let pp_name = function
  | Names.Name name -> pp_id name
  | Names.Anonymous -> write "_"

let pp_lname CAst.{ v; loc = _ } = pp_name v
let pp_lstring CAst.{ v; loc = _ } = write v

let pp_definition_object_kind = function
  | Decls.Example -> write "Example"
  | Decls.Definition -> write "Definition"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_sign = function NumTok.SPlus -> nop | NumTok.SMinus -> write "-"
let pp_unsigned n = write (NumTok.Unsigned.sprint n)
let pp_signed (sign, n) = sequence [ pp_sign sign; pp_unsigned n ]

let pp_prim_token = function
  | Constrexpr.Number n -> pp_signed n
  | Constrexpr.String s -> doublequoted (write s)

let pp_qualid id = write (Libnames.string_of_qualid id)

let rec pp_cases_pattern_expr CAst.{ v; loc = _ } = pp_cases_pattern_expr_r v

and pp_cases_pattern_expr_r = function
  | Constrexpr.CPatAtom (Some id) -> pp_qualid id
  | Constrexpr.CPatAtom None -> write "_"
  (* Cstr seems to mean 'Constructor'. (e.g., `S (S O)`, `Foo 0 1`) *)
  | Constrexpr.CPatCstr (outer, None, values) ->
      let open CAst in
      let conditional_parens expr =
        match expr.v with
        | Constrexpr.CPatAtom _ -> pp_cases_pattern_expr expr
        | _ -> parens (pp_cases_pattern_expr expr)
      in
      spaced (pp_qualid outer :: List.map conditional_parens values)
  | Constrexpr.CPatNotation (scope, notation, (exprs_1, exprs_2), []) ->
      let printing_rule = Ppextend.find_notation_printing_rule scope notation in

      let rec pp unparsings exprs =
        match (unparsings, exprs) with
        | [], _ -> nop
        | Ppextend.UnpMetaVar _ :: _, [] -> failwith "Too few exprs."
        | Ppextend.UnpMetaVar _ :: t, h_exprs :: t_exprs ->
            sequence [ pp_cases_pattern_expr h_exprs; pp t t_exprs ]
        | Ppextend.UnpBinderMetaVar _ :: _, _ ->
            fun printer -> raise (NotImplemented (contents printer))
        | Ppextend.UnpListMetaVar _ :: _, [] -> failwith "Too few exprs."
        | Ppextend.UnpListMetaVar _ :: t, h_exprs :: t_exprs ->
            sequence [ pp_cases_pattern_expr h_exprs; pp t t_exprs ]
        | Ppextend.UnpBinderListMetaVar _ :: _, _ ->
            fun printer -> raise (NotImplemented (contents printer))
        | Ppextend.UnpTerminal s :: t, _ -> sequence [ write s; pp t exprs ]
        | Ppextend.UnpCut _ :: t, _ -> sequence [ space; pp t exprs ]
        | Ppextend.UnpBox (_, xs) :: t, _ -> pp (List.map snd xs @ t) exprs
      in

      pp printing_rule.notation_printing_unparsing
        (exprs_1 @ List.flatten exprs_2)
  | Constrexpr.CPatPrim token -> pp_prim_token token
  | Constrexpr.CPatOr xs -> parens (map_bard pp_cases_pattern_expr xs)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_sort_name_expr = function
  | Constrexpr.CProp -> write "Prop"
  | Constrexpr.CSet -> write "Set"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_sort_expr = function
  | Glob_term.UAnonymous { rigid = true } -> write "Type"
  | Glob_term.UAnonymous { rigid = false } ->
      fun printer -> raise (NotImplemented (contents printer))
  | Glob_term.UNamed (None, [ (sort, 0) ]) -> pp_sort_name_expr sort
  | Glob_term.UNamed _ ->
      fun printer -> raise (NotImplemented (contents printer))

let rec pp_constr_expr CAst.{ v; loc = _ } = pp_constr_expr_r v

and pp_constr_expr_r = function
  | Constrexpr.CApp
      (outer, [ ((CAst.{ v = Constrexpr.CApp _; loc = _ } as inner), None) ]) ->
      spaced [ pp_constr_expr outer; parens (pp_constr_expr inner) ]
  | Constrexpr.CApp (fn, args) ->
      let open CAst in
      let conditional_parens expr =
        match expr.v with
        | Constrexpr.CApp _ | Constrexpr.CNotation _ | Constrexpr.CLambdaN _ ->
            parens (pp_constr_expr expr)
        | Constrexpr.CAppExpl ((name, None), [ _ ])
          when Libnames.string_of_qualid name <> ".." ->
            parens (pp_constr_expr expr)
        | _ -> pp_constr_expr expr
      in

      let pp_args =
        map_sequence
          (function
            | inner, None -> sequence [ space; conditional_parens inner ]
            | _, Some _ ->
                fun printer -> raise (NotImplemented (contents printer)))
          args
      in

      sequence [ conditional_parens fn; pp_args ]
  | Constrexpr.CAppExpl ((name, None), []) ->
      sequence [ write "@"; pp_qualid name ]
  | Constrexpr.CAppExpl ((dots, None), [ expr ])
    when Libnames.string_of_qualid dots = ".." ->
      spaced [ pp_qualid dots; parens (pp_constr_expr expr); pp_qualid dots ]
  | Constrexpr.CAppExpl ((name, None), exprs) ->
      sequence
        [ write "@"; pp_qualid name; space; map_spaced pp_constr_expr exprs ]
  | Constrexpr.CCases (_, None, matchees, branches) ->
      sequence
        [
          write "match ";
          map_commad pp_case_expr matchees;
          write " with";
          newline;
          map_sequence
            (fun branch -> sequence [ pp_branch_expr branch; newline ])
            branches;
          write "end";
        ]
  | Constrexpr.CCast (v, Some DEFAULTcast, t) ->
      let parens_needed =
        match v.v with Constrexpr.CApp _ -> true | _ -> false
      in
      let conditional_parens expr =
        if parens_needed then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      sequence [ conditional_parens v; write " : "; pp_constr_expr t ]
  | Constrexpr.CDelimiters (scope, expr) ->
      let parens_needed =
        match expr.v with Constrexpr.CNotation _ -> true | _ -> false
      in
      let conditional_parens expr =
        if parens_needed then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      sequence [ conditional_parens expr; write "%"; write scope ]
  | Constrexpr.CEvar (term, []) -> sequence [ write "?"; pp_id term.v ]
  | Constrexpr.CIf (cond, (None, None), t, f) ->
      sequence
        [
          write "if ";
          pp_constr_expr cond;
          newline;
          indented
            (sequence
               [
                 write "then ";
                 pp_constr_expr t;
                 newline;
                 write "else ";
                 pp_constr_expr f;
               ]);
        ]
  | Constrexpr.CLambdaN (args, body) ->
      sequence
        [
          write "fun ";
          map_spaced pp_local_binder_expr args;
          write " => ";
          pp_constr_expr body;
        ]
  | Constrexpr.CLetIn (name, binding, None, expr) ->
      sequence
        [
          write "let ";
          pp_lname name;
          write " := ";
          pp_constr_expr binding;
          write " in";
          newline;
          pp_constr_expr expr;
        ]
  | Constrexpr.CRef (id, None) -> pp_qualid id
  | Constrexpr.CNotation
      ( scope,
        notation,
        (init_replacers_nonrec, init_replacers_rec, [], local_assums) ) as op ->
      let open Ppextend in
      let open CAst in
      let notation_info = Notgram_ops.grammar_of_notation notation |> List.hd in

      let init_replacers =
        init_replacers_nonrec @ List.flatten init_replacers_rec
      in

      let assoc = notation_info.notgram_assoc in

      let entry_keys = notation_info.notgram_typs in

      let op_level = function
        | Constrexpr.CNotation (_, notation, _) ->
            Some (Notation.level_of_notation notation)
        | _ -> None
      in

      let printing_rule = Ppextend.find_notation_printing_rule scope notation in
      let rec printers unparsings replacers local_assums entry_keys =
        match (unparsings, replacers, local_assums, entry_keys) with
        | [], [], [], [] -> nop
        | [], _ :: _, _, _ -> failwith "Too many replacers."
        | [], _, _ :: _, _ -> failwith "Too many local assumptions."
        | [], _, _, _ :: _ -> failwith "Too many entry keys."
        | Ppextend.UnpMetaVar (_, side) :: t_u, h :: t_r, _, h_keys :: t_keys ->
            (* CProdN denotes a `forall foo, ... ` value. This value needs to be
               enclosed by parentheses if it is not on the rightmost position,
               otherwise all expressions will be in the scope of the `forall
               foo`.

               For example, `(forall x, f x = x) -> (forall x, f x = x)` is
               valid, but `forall x, f x = x -> forall x, f x = x` will be
               interpreted as `forall x, (f x = x -> forall x, f x = x).` which
               is invalid. Certainly, these two have different meanings, and
               thus lhs' `forall` needs parentheses.*)
            let parens_needed =
              match (h.v, assoc, side, h_keys) with
              | Constrexpr.CLambdaN _, _, _, _ -> true
              | Constrexpr.CProdN _, _, _, _ -> t_u <> []
              | _, Some LeftA, Some Right, _ | _, Some RightA, Some Left, _ ->
                  op_level h.v >= op_level op
              | _, Some LeftA, Some Left, _ | _, Some RightA, Some Right, _ ->
                  op_level h.v > op_level op
              | ( _,
                  None,
                  None,
                  Extend.ETConstr
                    ( Constrexpr.InConstrEntry,
                      None,
                      (Extend.NumLevel _, Extend.BorderProd (Right, None)) ) )
                ->
                  op_level h.v > op_level op
              | _ -> op_level h.v >= op_level op
            in
            let conditional_parens expr =
              if parens_needed then parens (pp_constr_expr expr)
              else pp_constr_expr expr
            in

            sequence
              [ conditional_parens h; printers t_u t_r local_assums t_keys ]
        | Ppextend.UnpMetaVar _ :: _, _, _, _ -> failwith "Too few replacers."
        | Ppextend.UnpBinderMetaVar _ :: _, _, _, _ -> raise (NotImplemented "")
        | Ppextend.UnpListMetaVar (_, seps, _) :: t, elems, _, _ :: zs ->
            let get_seps = function
              | Ppextend.UnpTerminal s -> s
              | Ppextend.UnpCut (PpBrk _) -> ";"
              | _ -> failwith "TODO"
            in
            let rec loop elems seps =
              match (elems, seps) with
              | [ x ], _ ->
                  sequence [ pp_constr_expr x; printers t [] local_assums zs ]
              | [], _ -> printers t [] local_assums zs
              | xs, [] ->
                  sequence
                    [
                      map_with_seps ~sep:(write "; ") pp_constr_expr xs;
                      printers t [] local_assums zs;
                    ]
              | x :: xs, sep :: seps ->
                  sequence
                    [
                      pp_constr_expr x;
                      write (get_seps sep);
                      space;
                      loop xs seps;
                    ]
            in
            loop elems seps
        | Ppextend.UnpListMetaVar (_, _, _) :: _, _, _, [] ->
            raise (NotImplemented "")
        | Ppextend.UnpBinderListMetaVar _ :: _, _, _, [] ->
            raise (NotImplemented "Too few entry keys.")
        | ( Ppextend.UnpBinderListMetaVar (true, true, [ _ ]) :: t,
            xs,
            assums,
            _ :: keys ) ->
            sequence
              [ map_spaced pp_local_binder_expr assums; printers t xs [] keys ]
        | Ppextend.UnpBinderListMetaVar _ :: _, _, _, _ ->
            fun printer -> raise (NotImplemented (contents printer))
        | Ppextend.UnpTerminal s :: t, xs, _, keys ->
            sequence [ write s; printers t xs local_assums keys ]
        | Ppextend.UnpBox (_, xs) :: t, _, _, keys ->
            printers (List.map snd xs @ t) replacers local_assums keys
        | Ppextend.UnpCut _ :: t, xs, _, keys ->
            let hor = sequence [ space; printers t xs local_assums keys ] in
            let ver = sequence [ newline; printers t xs local_assums keys ] in
            hor <-|> ver
      in

      printers printing_rule.notation_printing_unparsing init_replacers
        (List.flatten local_assums)
        entry_keys
  | Constrexpr.CPrim prim -> pp_prim_token prim
  | Constrexpr.CProdN (xs, CAst.{ v = Constrexpr.CHole _; loc = _ }) ->
      map_spaced pp_local_binder_expr xs
  | Constrexpr.CProdN (xs, ty) ->
      let hor = sequence [ space; pp_constr_expr ty ] in
      let ver = sequence [ newline; indented (pp_constr_expr ty) ] in
      sequence
        [
          write "forall ";
          map_spaced pp_local_binder_expr xs;
          write ",";
          hor <-|> ver;
        ]
  | Constrexpr.CHole (None, IntroAnonymous) -> write "_"
  | Constrexpr.CSort expr -> pp_sort_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_case_expr = function
  | expr, None, None -> pp_constr_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_local_binder_expr = function
  | Constrexpr.CLocalAssum
      ( [ name ],
        Constrexpr.Default Explicit,
        CAst.{ v = Constrexpr.CHole (_, IntroAnonymous); loc = _ } ) ->
      pp_lname name
  | Constrexpr.CLocalAssum (names, Constrexpr.Default kind, ty) ->
      let wrapper =
        match kind with
        | Explicit -> parens
        | MaxImplicit -> braces
        | _ -> fun _ printer -> raise (NotImplemented (contents printer))
      in

      wrapper
        (sequence [ map_spaced pp_lname names; write " : "; pp_constr_expr ty ])
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_branch_expr = function
  | CAst.{ v = patterns, expr; loc = _ } ->
      let hor = sequence [ space; pp_constr_expr expr ] in
      let ver = sequence [ newline; indented (pp_constr_expr expr) ] in
      sequence
        [
          write "| ";
          map_bard (map_commad pp_cases_pattern_expr) patterns;
          write " =>";
          hor <-|> ver;
        ]

let pp_definition_expr = function
  | Vernacexpr.ProveBody (args, expr) ->
      let hor = sequence [ space; pp_constr_expr expr ] in
      let ver = sequence [ newline; indented (pp_constr_expr expr) ] in
      sequence
        [
          map_sequence
            (fun arg -> sequence [ space; pp_local_binder_expr arg ])
            args;
          write " :";
          hor <-|> ver;
        ]
  | Vernacexpr.DefineBody (args, None, def_body, return_ty) ->
      let pp_args =
        map_sequence
          (fun arg -> sequence [ space; pp_local_binder_expr arg ])
          args
      in

      let pp_return_ty =
        match return_ty with
        | None -> nop
        | Some ty ->
            let hor = sequence [ write " : "; pp_constr_expr ty ] in
            let ver =
              sequence
                [
                  newline; indented (sequence [ write ": "; pp_constr_expr ty ]);
                ]
            in

            hor <-|> ver
      in

      let pp_body =
        let hor = sequence [ space; pp_constr_expr def_body ] in
        let ver = sequence [ newline; indented (pp_constr_expr def_body) ] in

        hor <-|> ver
      in

      sequence [ pp_args; pp_return_ty; write " :="; pp_body ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_proof_end = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, Some ident) ->
      sequence [ clear_bullets; write "Save"; space; pp_lident ident; dot ]
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) ->
      sequence [ clear_bullets; write "Qed." ]
  | Vernacexpr.Proved (Vernacexpr.Transparent, Some ident) ->
      sequence [ clear_bullets; write "Defined"; space; pp_lident ident; dot ]
  | Vernacexpr.Proved (Vernacexpr.Transparent, None) ->
      sequence [ clear_bullets; write "Defined." ]
  | Vernacexpr.Admitted -> sequence [ clear_bullets; write "Admitted." ]

let pp_theorem_kind = function
  | Decls.Fact -> write "Fact"
  | Decls.Lemma -> write "Lemma"
  | Decls.Theorem -> write "Theorem"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_fixpoint_expr = function
  | Vernacexpr.
      {
        fname;
        univs = None;
        rec_order = None;
        binders;
        rtype;
        body_def = Some body_def;
        notations = [];
      } ->
      let pp_return_type =
        match rtype.v with
        | Constrexpr.CHole (None, IntroAnonymous) -> nop
        | _ -> sequence [ write " : "; pp_constr_expr rtype ]
      in
      sequence
        [
          pp_lident fname;
          space;
          map_spaced pp_local_binder_expr binders;
          pp_return_type;
          write " :=";
          newline;
          indented (sequence [ pp_constr_expr body_def; dot ]);
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_constructor_expr = function
  | ([], Vernacexpr.NoCoercion, Vernacexpr.NoInstance), (name, expr) -> (
      let open CAst in
      (* TODO: Implement this completely. An `Inductive` is an inductive prop if
         it contains a `CRef` with its name. *)
      let rec is_inductive_prop = function
        | Constrexpr.CApp _ -> true
        | Constrexpr.CRef _ -> true
        | Constrexpr.CProdN (_, x) -> is_inductive_prop x.v
        | Constrexpr.CNotation (None, _, (xs, [], [], [])) ->
            List.exists (fun x -> is_inductive_prop x.v) xs
        | _ -> false
      in

      match expr.v with
      | Constrexpr.CHole _ -> sequence [ newline; write "| "; pp_lident name ]
      | _ when is_inductive_prop expr.v ->
          sequence
            [
              newline;
              write "| ";
              pp_lident name;
              write " : ";
              pp_constr_expr expr;
            ]
      | _ ->
          sequence
            [ newline; write "| "; pp_lident name; space; pp_constr_expr expr ])
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_syntax_modifier = function
  | Vernacexpr.SetAssoc LeftA -> write "left associativity"
  | Vernacexpr.SetAssoc RightA -> write "right associativity"
  | Vernacexpr.SetAssoc NonA -> write "no associativity"
  | Vernacexpr.SetItemLevel ([ name ], None, NextLevel) ->
      sequence [ write name; write " at next level" ]
  | Vernacexpr.SetLevel level ->
      sequence [ write "at level "; write (string_of_int level) ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let rec pp_gen_tactic_arg = function
  | Tacexpr.ConstrMayEval (ConstrTerm expr) -> pp_constr_expr expr
  | Tacexpr.Reference name -> pp_qualid name
  | Tacexpr.TacCall CAst.{ v = v, []; loc = _ } -> pp_qualid v
  | Tacexpr.TacCall CAst.{ v = name, [ arg ]; loc = _ } ->
      sequence [ pp_qualid name; space; pp_gen_tactic_arg arg ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_raw_red_expr = function
  | Genredexpr.Simpl
      ( Genredexpr.
          {
            rBeta = true;
            rMatch = true;
            rFix = true;
            rCofix = true;
            rZeta = true;
            rDelta = true;
            rConst = [];
          },
        None ) ->
      write "simpl"
  | Genredexpr.Unfold
      [ (AllOccurrences, CAst.{ v = Constrexpr.AN name; loc = _ }) ] ->
      sequence [ write "unfold "; pp_qualid name ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_intro_pattern_naming_expr = function
  | Namegen.IntroIdentifier name -> pp_id name
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let rec pp_or_and_intro_pattern_expr = function
  | Tactypes.IntroOrPattern patterns ->
      let open CAst in
      let hor =
        let prefix = function 0 -> nop | _ -> write " | " in
        let pp_patterns i pattern =
          sequence
            [
              prefix i; map_spaced (fun x -> pp_intro_pattern_expr x.v) pattern;
            ]
        in
        brackets (sequence (List.mapi pp_patterns patterns))
      in
      let ver =
        let prefix = function 0 -> nop | _ -> write "|" in
        let pp_patterns i pattern =
          sequence
            [
              prefix i;
              map_sequence
                (fun x -> sequence [ space; pp_intro_pattern_expr x.v ])
                pattern;
              newline;
            ]
        in
        sequence
          [ write "["; sequence (List.mapi pp_patterns patterns); write "]" ]
      in
      hor <-|> ver
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_intro_pattern_action_expr = function
  | Tactypes.IntroOrAndPattern expr -> pp_or_and_intro_pattern_expr expr
  | Tactypes.IntroWildcard -> write "_"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_intro_pattern_expr = function
  | Tactypes.IntroAction expr -> pp_intro_pattern_action_expr expr
  | Tactypes.IntroNaming expr -> pp_intro_pattern_naming_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_core_destruction_arg = function
  | Tactics.ElimOnConstr (expr, Tactypes.NoBindings) ->
      parens (pp_constr_expr expr)
  | Tactics.ElimOnIdent ident -> pp_lident ident
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_destruction_arg = function
  | None, arg -> pp_core_destruction_arg arg
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_or_var = function
  | Locus.ArgArg CAst.{ v; loc = _ } ->
      let hor = sequence [ write " as "; pp_or_and_intro_pattern_expr v ] in
      let ver =
        sequence
          [ write " as"; newline; indented (pp_or_and_intro_pattern_expr v) ]
      in
      hor <-|> ver
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_induction_clause = function
  | arg, (eqn, as_list), None ->
      let pp_as_list = function None -> nop | Some args -> pp_or_var args in
      let pp_eqn = function
        | None -> nop
        | Some x ->
            let open CAst in
            sequence [ write " eqn:"; pp_intro_pattern_naming_expr x.v ]
      in
      sequence [ pp_destruction_arg arg; pp_as_list as_list; pp_eqn eqn ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_induction_clause_list = function
  | [ clause ], None -> pp_induction_clause clause
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_hyp_location_expr = function
  | (Locus.AllOccurrences, name), Locus.InHyp -> pp_id name.CAst.v
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_quantified_hypothesis = function
  | Tactypes.NamedHyp name -> pp_lident name
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_inversion_strength = function
  | Tacexpr.NonDepInversion (FullInversion, [], Some args) -> pp_or_var args
  | Tacexpr.NonDepInversion (FullInversion, [], None) -> nop
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_bindings = function
  | Tactypes.NoBindings -> nop
  | Tactypes.ExplicitBindings bindings ->
      let pp_one_binding CAst.{ v = replacee, replacer; loc = _ } =
        parens
          (sequence
             [
               pp_quantified_hypothesis replacee;
               write " := ";
               pp_constr_expr replacer;
             ])
      in
      sequence [ write " with "; map_spaced pp_one_binding bindings ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_raw_atomic_tactic_expr = function
  | Tacexpr.TacApply (true, false, [ (None, (expr, bindings)) ], in_clause) ->
      let pp_in_clause =
        match in_clause with
        | [] -> nop
        | [ (name, None) ] -> sequence [ write " in "; pp_id name.CAst.v ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      let parens_needed expr =
        match expr.CAst.v with Constrexpr.CApp _ -> true | _ -> false
      in
      let conditional_parens expr =
        if parens_needed expr then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      sequence
        [
          write "apply ";
          conditional_parens expr;
          pp_bindings bindings;
          pp_in_clause;
        ]
  | Tacexpr.TacAssert (false, true, Some None, Some name, expr) ->
      sequence
        [
          write "assert (";
          pp_intro_pattern_expr name.v;
          write " : ";
          pp_constr_expr expr;
          write ")";
        ]
  | Tacexpr.TacInductionDestruct (is_induction, false, clause_list) ->
      sequence
        [
          (if is_induction then write "induction " else write "destruct ");
          pp_induction_clause_list clause_list;
        ]
  | Tacexpr.TacIntroPattern
      (false, [ CAst.{ v = Tactypes.IntroForthcoming _; loc = _ } ]) ->
      write "intros"
  | Tacexpr.TacIntroPattern (false, exprs) ->
      let open CAst in
      sequence
        [
          write "intros ";
          map_spaced (fun expr -> pp_intro_pattern_expr expr.v) exprs;
        ]
  | Tacexpr.TacReduce (expr, { onhyps = Some []; concl_occs = AllOccurrences })
    ->
      pp_raw_red_expr expr
  | Tacexpr.TacReduce
      (expr, { onhyps = Some [ name ]; concl_occs = NoOccurrences }) ->
      sequence [ pp_raw_red_expr expr; write " in "; pp_hyp_location_expr name ]
  | Tacexpr.TacRewrite
      ( false,
        [ (is_left_to_right, Precisely 1, (None, (expr, with_bindings))) ],
        in_bindings,
        None ) ->
      let parens_needed =
        match expr.v with Constrexpr.CApp _ -> true | _ -> false
      in
      let conditional_parens expr =
        if parens_needed then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      let pp_in_bindings =
        let open Locus in
        match in_bindings with
        | { onhyps = Some []; concl_occs = AllOccurrences } -> nop
        | { onhyps = Some [ name ]; concl_occs = NoOccurrences } ->
            sequence [ write " in "; pp_hyp_location_expr name ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      sequence
        [
          write "rewrite ";
          (if is_left_to_right then write "-> " else write "<- ");
          conditional_parens expr;
          pp_in_bindings;
          pp_bindings with_bindings;
        ]
  | Tacexpr.TacInversion (intros, name) ->
      sequence
        [
          write "inversion ";
          pp_quantified_hypothesis name;
          pp_inversion_strength intros;
        ]
  | Tacexpr.TacLetTac
      ( false,
        replacer,
        replacee,
        { onhyps = None; concl_occs = AllOccurrences },
        false,
        None ) ->
      let parens_neded =
        match replacee.v with
        | Constrexpr.CApp _ | Constrexpr.CNotation _ -> true
        | _ -> false
      in
      let conditional_parens expr =
        if parens_neded then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      sequence
        [
          write "remember ";
          conditional_parens replacee;
          write " as ";
          pp_name replacer;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let rec pp_raw_tactic_expr (CAst.{ v; loc = _ } : Tacexpr.raw_tactic_expr) =
  pp_gen_tactic_expr_r v

and pp_gen_tactic_expr_r = function
  | Tacexpr.TacAlias (alias, init_replacers) ->
      (* FIXME: Needs refactoring. *)
      let id = Names.KerName.label alias |> Names.Label.to_string in
      let init xs = List.rev xs |> List.tl |> List.rev in

      let parens_needed expr =
        let open CAst in
        match expr.v with
        | Constrexpr.CRef _ | Constrexpr.CPrim _ -> false
        | _ -> true
      in
      let conditional_parens expr =
        if parens_needed expr then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      (* The last element is Coq's internal ID and we don't need it. *)
      let init_idents = String.split_on_char '_' id |> init in
      let rec loop idents replacers =
        match (idents, replacers) with
        | "#" :: _, [] -> failwith "Too few replacers."
        | "#" :: t_ids, Tacexpr.TacGeneric (None, args) :: t_reps -> (
            match
              ( Conversion.constr_expr_of_raw_generic_argument args,
                Conversion.destruction_arg_of_raw_generic_argument args,
                Conversion.intro_pattern_list_of_raw_generic_argument args,
                Conversion.clause_expr_of_raw_generic_argument args,
                Conversion.bindings_list_of_raw_generic_argument args,
                Conversion.id_of_raw_generic_argument args )
            with
            | None, None, None, None, None, None -> loop t_ids t_reps
            | Some h_reps, _, _, _, _, _ ->
                conditional_parens h_reps :: loop t_ids t_reps
            | _, Some h_reps, _, _, _, _ ->
                pp_destruction_arg h_reps :: loop t_ids t_reps
            | _, _, Some h_reps, _, _, _ ->
                let open CAst in
                map_spaced (fun x -> pp_intro_pattern_expr x.v) h_reps
                :: loop t_ids t_reps
            | _, _, _, Some { onhyps = Some [ name ]; concl_occs = _ }, _, _ ->
                pp_hyp_location_expr name :: loop t_ids t_reps
            | _, _, _, _, Some bindings, _ ->
                let pp_binding = function
                  | Tactypes.ImplicitBindings [ x ] -> conditional_parens x
                  | _ ->
                      fun printer -> raise (NotImplemented (contents printer))
                in

                map_commad pp_binding bindings :: loop t_ids t_reps
            | _, _, _, _, _, Some id -> pp_id id :: loop t_ids t_reps
            | _ ->
                [ (fun printer -> raise (NotImplemented (contents printer))) ])
        | "#" :: t_ids, _ :: t_reps -> loop t_ids t_reps
        | [], [] -> []
        | [], _ -> failwith "Too many replacers."
        | h_id :: t_id, _ -> write h_id :: loop t_id replacers
      in
      sequence [ loop init_idents init_replacers |> spaced ]
  | Tacexpr.TacArg arg -> pp_gen_tactic_arg arg
  | Tacexpr.TacAtom atom -> sequence [ pp_raw_atomic_tactic_expr atom ]
  | Tacexpr.TacRepeat tactic ->
      sequence [ write "repeat "; pp_raw_tactic_expr tactic ]
  | Tacexpr.TacThen (first, second) ->
      sequence
        [
          pp_raw_tactic_expr first;
          write ";";
          newline;
          pp_raw_tactic_expr second;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_ltac =
  map_sequence (fun arg ->
      match Conversion.raw_tactic_expr_of_raw_generic_argument arg with
      | None -> nop
      | Some t -> sequence [ pp_raw_tactic_expr t; dot ])

let pp_proof_bullet = function
  | Proof_bullet.Dash n -> write_before_indent (String.make n '-' ^ " ")
  | Proof_bullet.Plus n -> write_before_indent (String.make n '+' ^ " ")
  | Proof_bullet.Star n -> write_before_indent (String.make n '*' ^ " ")

let pp_import_categories { Vernacexpr.negative; import_cats } =
  let open CAst in
  sequence
    [
      space;
      (if negative then write "-" else nop);
      parens (map_commad (fun import_cat -> write import_cat.v) import_cats);
    ]

let pp_export_flag = function
  | Vernacexpr.Export -> write "Export"
  | Vernacexpr.Import -> write "Import"

let pp_export_with_cats (flag, import_categories) =
  sequence
    [
      space;
      pp_export_flag flag;
      (match import_categories with
      | None -> nop
      | Some x -> pp_import_categories x);
    ]

let pp_import_filter_expr import_filter_expr =
  match import_filter_expr with
  | Vernacexpr.ImportAll -> nop
  | Vernacexpr.ImportNames names ->
      sequence
        [
          (* FIXME: The Coq parser will raise an exception here if Export/Import
             was omitted *)
          parens
            (map_commad (fun (filter_name, _) -> pp_qualid filter_name) names);
        ]

let pp_search_item = function
  | Vernacexpr.SearchSubPattern ((Anywhere, false), expr) ->
      let parens_needed =
        match expr.v with Constrexpr.CRef _ -> false | _ -> true
      in
      let conditional_parens expr =
        if parens_needed then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      conditional_parens expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_search_request = function
  | Vernacexpr.SearchLiteral search_item -> pp_search_item search_item
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_searchable = function
  | Vernacexpr.Search [ (true, search_request) ] ->
      pp_search_request search_request
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_search_restriction = function
  | Vernacexpr.SearchInside [ range ] ->
      sequence [ write " inside "; pp_qualid range ]
  | Vernacexpr.SearchOutside [] -> nop
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_vernac_argument_status = function
  | Vernacexpr.RealArg
      { name = ty; recarg_like = false; notation_scope = []; implicit_status }
    ->
      let encloser =
        match implicit_status with
        | Explicit -> Fun.id
        | MaxImplicit -> braces
        | NonMaxImplicit -> brackets
      in

      encloser (pp_name ty)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_option_string = function
  | Vernacexpr.OptionSetString s -> doublequoted (write s)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_ident_decl = function
  | name, None -> pp_lident name
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_printable = function
  | Vernacexpr.PrintAssumptions (false, false, { v = AN name; loc = _ }) ->
      sequence [ write "Print Assumptions "; pp_qualid name; dot ]
  | Vernacexpr.PrintName (CAst.{ v = AN name; loc = _ }, None) ->
      sequence [ write "Print "; pp_qualid name; dot ]
  | Vernacexpr.PrintName (CAst.{ v = ByNotation (name, None); loc = _ }, None)
    ->
      sequence [ write "Print "; doublequoted (write name); dot ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_assumption_object_kind = function
  | Decls.Logical -> write "Axiom"
  | Decls.Conjectural -> write "Conjecture"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_notation_declaration = function
  | Vernacexpr.
      {
        ntn_decl_string;
        ntn_decl_interp;
        ntn_decl_scope = None;
        ntn_decl_modifiers = [];
      } ->
      sequence
        [
          newline;
          indented
            (sequence
               [
                 write "where";
                 newline;
                 indented
                   (sequence
                      [
                        doublequoted (pp_lstring ntn_decl_string);
                        write " := ";
                        parens (pp_constr_expr ntn_decl_interp);
                      ]);
               ]);
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_comment = function
  | Vernacexpr.CommentConstr expr -> pp_constr_expr expr
  | Vernacexpr.CommentString s -> doublequoted (write s)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_showable = function
  | Vernacexpr.ShowProof -> write "Proof"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_synterp_vernac_expr = function
  | Vernacexpr.VernacDefineModule (None, name, [], Check [], []) ->
      sequence [ write "Module "; pp_lident name; dot; increase_indent ]
  | Vernacexpr.VernacEndSegment name ->
      sequence [ decrease_indent; write "End "; pp_lident name; dot ]
  | Vernacexpr.VernacImport ((flag, None), [ (name, ImportAll) ]) ->
      sequence [ pp_export_flag flag; space; pp_qualid name; dot ]
      (* FIXME: Support other plugins, like ltac2. *)
  | Vernacexpr.VernacExtend (_, args) -> pp_ltac args
  | Vernacexpr.VernacNotation
      ( false,
        {
          ntn_decl_string = notation;
          ntn_decl_interp = expr;
          ntn_decl_scope = scope;
          ntn_decl_modifiers = modifiers;
        } ) ->
      let pp_modifiers printer =
        sequence
          [
            space;
            parens
              (map_commad
                 (fun modifier ->
                   let open CAst in
                   pp_syntax_modifier modifier.v)
                 modifiers);
          ]
          printer
      in

      (* We cannot use `Option.value` here because the `Option` module is
         overridden by `coq-core`'s one which does not have it. *)
      let pp_scope =
        match scope with
        | None -> nop
        | Some scope -> sequence [ write " : "; write scope ]
      in

      let hor =
        sequence
          [
            space;
            parens (pp_constr_expr expr);
            (if List.length modifiers > 0 then pp_modifiers else nop);
            pp_scope;
            dot;
          ]
      in

      let ver =
        sequence
          [
            newline;
            indented
              (sequence
                 [
                   parens (pp_constr_expr expr);
                   (if List.length modifiers > 0 then pp_modifiers else nop);
                   pp_scope;
                   dot;
                 ]);
          ]
      in

      sequence
        [
          write "Notation \""; pp_lstring notation; write "\" :="; hor <-|> ver;
        ]
  | Vernacexpr.VernacRequire (dirpath, export_with_cats, filtered_import) ->
      let pp_dirpath printer =
        (match dirpath with
        | None -> nop
        | Some dirpath -> sequence [ write "From "; pp_qualid dirpath; space ])
          printer
      in

      let pp_categories =
        match export_with_cats with
        | None -> nop
        | Some x -> pp_export_with_cats x
      in

      let pp_name_and_filter =
        map_sequence
          (fun (modname, import_filter_expr) ->
            sequence
              [
                space;
                pp_qualid modname;
                pp_import_filter_expr import_filter_expr;
              ])
          filtered_import
      in

      sequence
        [ pp_dirpath; write "Require"; pp_categories; pp_name_and_filter; dot ]
  | Vernacexpr.VernacReservedNotation (false, (notation, [ modifier ])) ->
      sequence
        [
          write "Reserved Notation ";
          doublequoted (pp_lstring notation);
          space;
          parens (pp_syntax_modifier modifier.v);
          write ".";
        ]
  | Vernacexpr.VernacSetOption (false, names, options) ->
      sequence
        [
          write "Set ";
          map_spaced write names;
          space;
          pp_option_string options;
          dot;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_synpure_vernac_expr = function
  | Vernacexpr.VernacAbort -> sequence [ clear_bullets; write "Abort." ]
  | Vernacexpr.VernacArguments (CAst.{ v = AN name; loc = _ }, args, [], []) ->
      sequence
        [
          write "Arguments ";
          pp_qualid name;
          space;
          map_spaced pp_vernac_argument_status args;
          dot;
        ]
  | Vernacexpr.VernacAssumption
      ((NoDischarge, kind), NoInline, [ (NoCoercion, ([ name ], expr)) ]) ->
      sequence
        [
          pp_assumption_object_kind kind;
          space;
          pp_ident_decl name;
          write " : ";
          pp_constr_expr expr;
          dot;
        ]
  | Vernacexpr.VernacCheckMayEval (check_or_compute, None, expr) ->
      let pp_name =
        match check_or_compute with
        | Some (CbvVm None) -> write "Compute"
        | None -> write "Check"
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in

      let parens_needed = function
        | Constrexpr.CRef _ | Constrexpr.CCast _ | Constrexpr.CDelimiters _ ->
            false
        | _ -> true
      in
      let pp_expr =
        if parens_needed expr.v then parens (pp_constr_expr expr)
        else pp_constr_expr expr
      in

      let hor = sequence [ pp_name; space; pp_expr; dot ] in
      let ver =
        sequence [ pp_name; newline; indented (sequence [ pp_expr; dot ]) ]
      in

      hor <-|> ver
  | Vernacexpr.VernacComments xs ->
      sequence [ write "Comments "; map_spaced pp_comment xs; dot ]
  | Vernacexpr.VernacDefinition ((NoDischarge, kind), (name, None), expr) ->
      sequence
        [
          pp_definition_object_kind kind;
          space;
          pp_lname name;
          pp_definition_expr expr;
          dot;
        ]
  | Vernacexpr.VernacFixpoint (NoDischarge, [ expr ]) ->
      sequence [ write "Fixpoint "; pp_fixpoint_expr expr ]
  | Vernacexpr.VernacLocate
      (LocateAny CAst.{ v = Constrexpr.ByNotation (name, None); loc = _ }) ->
      sequence [ write "Locate "; doublequoted (write name); dot ]
  | Vernacexpr.VernacOpenCloseScope (true, scope) ->
      sequence [ write "Open Scope "; write scope; dot ]
  | Vernacexpr.VernacPrint printable -> pp_printable printable
  | Vernacexpr.VernacSearch (searchable, None, search_restriction) ->
      sequence
        [
          write "Search ";
          pp_searchable searchable;
          pp_search_restriction search_restriction;
          dot;
        ]
  | Vernacexpr.VernacStartTheoremProof (kind, [ ((ident, None), (args, expr)) ])
    ->
      let hor = sequence [ space; pp_constr_expr expr; dot ] in
      let ver =
        sequence [ newline; indented (sequence [ pp_constr_expr expr; dot ]) ]
      in
      sequence
        [
          pp_theorem_kind kind;
          space;
          pp_lident ident;
          map_sequence
            (fun arg -> sequence [ space; pp_local_binder_expr arg ])
            args;
          write " :";
          hor <-|> ver;
        ]
  | Vernacexpr.VernacShow x -> sequence [ write "Show "; pp_showable x; dot ]
  | Vernacexpr.VernacProof (None, None) -> write "Proof."
  | Vernacexpr.VernacInductive (Inductive_kw, inductives) ->
      let pp_single_inductive = function
        | ( ( (Vernacexpr.NoCoercion, (name, None)),
              (type_params, None),
              return_type,
              Vernacexpr.Constructors constructors ),
            where_clause ) ->
            let pp_type_params =
              match type_params with
              | [] -> nop
              | _ ->
                  sequence
                    [ space; map_spaced pp_local_binder_expr type_params ]
            in
            let pp_return_type =
              match return_type with
              | Some ty -> sequence [ write " : "; pp_constr_expr ty ]
              | None -> nop
            in

            let pp_where_clause =
              match where_clause with
              | [] -> nop
              | [ notation ] -> pp_notation_declaration notation
              | _ -> fun printer -> raise (NotImplemented (contents printer))
            in

            sequence
              [
                pp_lident name;
                pp_type_params;
                pp_return_type;
                write " :=";
                indented (map_sequence pp_constructor_expr constructors);
                pp_where_clause;
              ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in
      sequence
        [
          write "Inductive ";
          map_with_seps
            ~sep:(sequence [ newline; write "with " ])
            pp_single_inductive inductives;
          dot;
        ]
  | Vernacexpr.VernacEndProof proof_end -> pp_proof_end proof_end
  | Vernacexpr.VernacBullet bullet ->
      sequence [ bullet_appears bullet; pp_proof_bullet bullet ]
  | Vernacexpr.VernacSubproof None -> sequence [ write "{"; start_subproof ]
  | Vernacexpr.VernacEndSubproof -> sequence [ end_subproof; write "}" ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_vernac_expr = function
  | Vernacexpr.VernacSynterp x -> pp_synterp_vernac_expr x
  | Vernacexpr.VernacSynPure x -> pp_synpure_vernac_expr x

let pp_control_flag = function
  | Vernacexpr.ControlFail -> write "Fail"
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_subast CAst.{ v = Vernacexpr.{ control; attrs = _; expr }; loc = _ } =
  let pp_controls = function
    | [] -> nop
    | _ -> sequence [ map_spaced pp_control_flag control; space ]
  in
  sequence [ pp_controls control; pp_vernac_expr expr ]

let separator current next =
  let open Vernacexpr in
  let open CAst in
  match (current.v.expr, next.v.expr) with
  | VernacSynPure (VernacStartTheoremProof _), VernacSynPure VernacAbort
  | VernacSynPure (VernacStartTheoremProof _), VernacSynPure (VernacEndProof _)
  | VernacSynPure (VernacStartTheoremProof _), VernacSynPure (VernacProof _)
  | VernacSynPure (VernacDefinition _), VernacSynPure VernacAbort
  | VernacSynPure (VernacDefinition _), VernacSynPure (VernacEndProof _)
  | VernacSynPure (VernacDefinition _), VernacSynPure (VernacProof _)
  | VernacSynPure (VernacProof _), VernacSynPure VernacAbort
  | VernacSynPure (VernacProof _), VernacSynPure (VernacEndProof _)
  | _, VernacSynPure (VernacProof _)
  (* `Compute` *)
  | ( VernacSynPure (VernacCheckMayEval (Some (CbvVm None), _, _)),
      VernacSynPure (VernacCheckMayEval (Some (CbvVm None), _, _)) )
  (* `Check` *)
  | ( VernacSynPure (VernacCheckMayEval (None, _, _)),
      VernacSynPure (VernacCheckMayEval (None, _, _)) )
  | VernacSynterp (VernacNotation _), VernacSynterp (VernacNotation _)
  | ( VernacSynterp (VernacReservedNotation _),
      VernacSynterp (VernacReservedNotation _) )
  | VernacSynterp (VernacDefineModule _), _
  | _, VernacSynterp (VernacEndSegment _)
  | VernacSynPure (VernacSearch _), VernacSynPure (VernacSearch _)
  (* `From ... Require ...` *)
  | ( VernacSynterp (VernacRequire (Some _, _, _)),
      VernacSynterp (VernacRequire (Some _, _, _)) )
  (* `Require ...` *)
  | ( VernacSynterp (VernacRequire (None, _, _)),
      VernacSynterp (VernacRequire (None, _, _)) )
  | VernacSynterp (VernacSetOption _), VernacSynterp (VernacSetOption _)
  | VernacSynPure (VernacLocate _), VernacSynPure (VernacLocate _)
  | VernacSynPure (VernacPrint _), VernacSynPure (VernacPrint _) ->
      newline
  | VernacSynPure (VernacDefinition (_, _, ProveBody _)), _
  | VernacSynPure (VernacProof _), _
  | VernacSynPure (VernacStartTheoremProof _), _ ->
      sequence [ newline; increase_indent ]
  | _, VernacSynPure VernacAbort | _, VernacSynPure (VernacEndProof _) ->
      sequence [ newline; decrease_indent ]
  | VernacSynPure (VernacCheckMayEval _), _
  | _, VernacSynPure (VernacCheckMayEval _)
  | VernacSynterp (VernacNotation _), _
  | _, VernacSynterp (VernacNotation _)
  | VernacSynPure (VernacDefinition _), _
  | _, VernacSynPure (VernacDefinition _)
  | VernacSynPure (VernacFixpoint _), _
  | _, VernacSynPure (VernacFixpoint _)
  | VernacSynPure (VernacInductive _), _
  | _, VernacSynPure (VernacInductive _)
  | VernacSynPure (VernacSearch _), _
  | _, VernacSynPure (VernacSearch _)
  | VernacSynterp (VernacReservedNotation _), _
  | _, VernacSynterp (VernacReservedNotation _)
  | VernacSynPure (VernacLocate _), _
  | _, VernacSynPure (VernacLocate _)
  | VernacSynterp (VernacSetOption _), _
  | _, VernacSynterp (VernacSetOption _)
  | VernacSynPure (VernacPrint _), _
  | _, VernacSynPure (VernacPrint _)
  | _, VernacSynterp (VernacDefineModule _)
  | VernacSynterp (VernacEndSegment _), _
  | VernacSynPure (VernacEndProof _), _
  | VernacSynPure VernacAbort, _
  | VernacSynterp (VernacRequire _), _ ->
      blankline
  | VernacSynPure (VernacBullet _), _ -> nop
  | _, _ -> newline

let pp_ast parser =
  (* Do not try to pp after parsing everything. After parsing everything, the
     STM will not be in nested module(s), and things that are valid inside them
     (e.g., notation declarations inside them) will be invalid, making some
     functions (e.g., `grammar_of_notation`) raise exceptions. *)
  match Astparser.next parser with
  | None -> ""
  | Some first_ast ->
      let printer = create () in

      pp_subast first_ast printer;

      let rec loop prev_ast =
        match Astparser.next parser with
        | None -> newline printer
        | Some next_ast ->
            sequence [ separator prev_ast next_ast; pp_subast next_ast ] printer;
            loop next_ast
      in

      let () = loop first_ast in
      contents printer
