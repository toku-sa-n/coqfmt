open Printer
open Ltac_plugin

exception NotImplemented of string

let nop _ = ()
let pp_id id = write (Names.Id.to_string id)
let pp_lident CAst.{ v; loc = _ } = pp_id v

let pp_name = function
  | Names.Name name -> pp_id name
  | Names.Anonymous -> fun printer -> raise (NotImplemented (contents printer))

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
  | Constrexpr.String s -> write s

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
      sequence
        [
          pp_qualid outer;
          map_sequence
            (fun value -> sequence [ space; conditional_parens value ])
            values;
        ]
  | Constrexpr.CPatNotation (None, (_, notation), (expr1, expr2), []) ->
      (* FIXME: THE CODE OF THIS BRANCH IS CORNER-CUTTING. *)
      let exprs = expr1 @ List.flatten expr2 in
      let prefix =
        String.split_on_char '_' notation |> List.hd |> String.trim
      in
      let suffix =
        String.split_on_char '_' notation |> List.rev |> List.hd |> String.trim
      in
      let separator =
        String.split_on_char '_' notation |> List.tl |> List.hd |> String.trim
      in
      sequence
        [
          write prefix;
          with_seps
            ~sep:(sequence [ write separator; space ])
            pp_cases_pattern_expr exprs;
          write suffix;
        ]
  | Constrexpr.CPatPrim token -> pp_prim_token token
  | Constrexpr.CPatOr xs -> parens (bard pp_cases_pattern_expr xs)
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_sort_expr = function
  | Glob_term.UAnonymous { rigid = true } -> write "Type"
  | Glob_term.UAnonymous { rigid = false } ->
      fun printer -> raise (NotImplemented (contents printer))
  | Glob_term.UNamed [ (Constrexpr.CSet, 0) ] -> write "Set"
  | Glob_term.UNamed _ ->
      fun printer -> raise (NotImplemented (contents printer))

let rec pp_constr_expr CAst.{ v; loc = _ } = pp_constr_expr_r v

and pp_constr_expr_r = function
  | Constrexpr.CApp
      (outer, [ ((CAst.{ v = Constrexpr.CApp _; loc = _ } as inner), None) ]) ->
      sequence [ pp_constr_expr outer; space; parens (pp_constr_expr inner) ]
  | Constrexpr.CApp (outer, inners) ->
      let open CAst in
      let conditional_parens expr =
        match expr.v with
        | Constrexpr.CApp _ -> parens (pp_constr_expr expr)
        | _ -> pp_constr_expr expr
      in
      sequence
        [
          pp_constr_expr outer;
          map_sequence
            (function
              | inner, None -> sequence [ space; conditional_parens inner ]
              | _, Some _ ->
                  fun printer -> raise (NotImplemented (contents printer)))
            inners;
        ]
  | Constrexpr.CCases (_, None, matchees, branches) ->
      sequence
        [
          write "match ";
          commad pp_case_expr matchees;
          write " with";
          newline;
          map_sequence
            (fun branch -> sequence [ pp_branch_expr branch; newline ])
            branches;
          write "end";
        ]
  | Constrexpr.CCast (v, DEFAULTcast, t) ->
      sequence [ pp_constr_expr v; write " : "; pp_constr_expr t ]
  | Constrexpr.CIf (cond, (None, None), t, f) ->
      sequence
        [
          write "if ";
          pp_constr_expr cond;
          newline;
          increase_indent;
          write "then ";
          pp_constr_expr t;
          newline;
          write "else ";
          pp_constr_expr f;
          decrease_indent;
        ]
  | Constrexpr.CRef (id, None) -> pp_qualid id
  | Constrexpr.CNotation
      (None, (InConstrEntry, init_notation), (init_replacers, [], [], [])) ->
      let rec loop notation replacers =
        match (notation, replacers) with
        | "", [] -> nop
        | "", _ -> fun _ -> failwith "Not all relpacers are consumed."
        | s, [ x ] when String.starts_with ~prefix:"_" s ->
            sequence
              [
                pp_constr_expr x; loop (String.sub s 1 (String.length s - 1)) [];
              ]
        | s, h :: t when String.starts_with ~prefix:"_" s ->
            let open CAst in
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
              match h.v with Constrexpr.CProdN _ -> true | _ -> false
            in
            let conditional_parens expr =
              if parens_needed then parens (pp_constr_expr expr)
              else pp_constr_expr expr
            in
            sequence
              [
                conditional_parens h;
                loop (String.sub s 1 (String.length s - 1)) t;
              ]
        | s, _ ->
            sequence
              [
                write (String.sub s 0 1);
                loop (String.sub s 1 (String.length s - 1)) replacers;
              ]
      in
      loop init_notation init_replacers
  | Constrexpr.CPrim prim -> pp_prim_token prim
  | Constrexpr.CProdN (xs, CAst.{ v = Constrexpr.CHole _; loc = _ }) ->
      spaced pp_local_binder_expr xs
  | Constrexpr.CProdN (xs, ty) ->
      sequence
        [
          write "forall ";
          spaced pp_local_binder_expr xs;
          write ", ";
          pp_constr_expr ty;
        ]
  | Constrexpr.CHole (None, IntroAnonymous, None) -> nop
  | Constrexpr.CSort expr -> pp_sort_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_case_expr = function
  | expr, None, None -> pp_constr_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_local_binder_expr = function
  | Constrexpr.CLocalAssum
      ( [ name ],
        Constrexpr.Default Explicit,
        CAst.{ v = Constrexpr.CHole (_, IntroAnonymous, None); loc = _ } ) ->
      pp_lname name
  | Constrexpr.CLocalAssum (names, Constrexpr.Default Explicit, ty) ->
      parens
        (sequence [ spaced pp_lname names; write " : "; pp_constr_expr ty ])
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_branch_expr = function
  | CAst.{ v = patterns, expr; loc = _ } ->
      sequence
        [
          write "| ";
          bard (commad pp_cases_pattern_expr) patterns;
          write " => ";
          pp_constr_expr expr;
        ]

let pp_definition_expr = function
  | Vernacexpr.ProveBody ([], expr) ->
      sequence [ write " : "; pp_constr_expr expr ]
  | Vernacexpr.DefineBody (args, None, def_body, return_ty) ->
      sequence
        [
          map_sequence
            (fun arg -> sequence [ space; pp_local_binder_expr arg ])
            args;
          (match return_ty with
          | None -> nop
          | Some ty -> sequence [ write " : "; pp_constr_expr ty ]);
          write " :=";
          newline;
          increase_indent;
          pp_constr_expr def_body;
          decrease_indent;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_proof_end = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) ->
      sequence [ clear_bullets; write "Qed." ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_theorem_kind = function
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
        | Constrexpr.CHole (None, IntroAnonymous, None) -> nop
        | _ -> sequence [ write " : "; pp_constr_expr rtype ]
      in
      sequence
        [
          pp_lident fname;
          space;
          spaced pp_local_binder_expr binders;
          pp_return_type;
          write " :=";
          newline;
          increase_indent;
          pp_constr_expr body_def;
          write ".";
          decrease_indent;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_construtor_expr = function
  | (Vernacexpr.NoCoercion, Vernacexpr.NoInstance), (name, expr) -> (
      let open CAst in
      match expr.v with
      | Constrexpr.CHole _ -> sequence [ newline; write "| "; pp_lident name ]
      | Constrexpr.CRef _ ->
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
  | Vernacexpr.SetLevel level ->
      sequence [ write "at level "; write (string_of_int level) ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_gen_tactic_arg = function
  | Tacexpr.TacCall ast -> sequence [ pp_qualid (fst ast.v); write "." ]
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
      write "simpl."
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_intro_pattern_naming_expr = function
  | Namegen.IntroIdentifier name -> pp_id name
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let rec pp_or_and_intro_pattern_expr = function
  | Tactypes.IntroOrPattern patterns ->
      let open CAst in
      let pp_patterns i pattern =
        match (i, pattern) with
        | 0, [] -> space
        | 0, xs -> spaced (fun x -> pp_intro_pattern_expr x.v) xs
        | _, xs ->
            sequence
              [ write "| "; spaced (fun x -> pp_intro_pattern_expr x.v) xs ]
      in
      brackets (sequence (List.mapi pp_patterns patterns))
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_intro_pattern_action_expr = function
  | Tactypes.IntroOrAndPattern expr -> pp_or_and_intro_pattern_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_intro_pattern_expr = function
  | Tactypes.IntroAction expr -> pp_intro_pattern_action_expr expr
  | Tactypes.IntroNaming expr -> pp_intro_pattern_naming_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_core_destruction_arg = function
  | Tactics.ElimOnIdent ident -> pp_lident ident
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_destruction_arg = function
  | None, arg -> pp_core_destruction_arg arg
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_induction_clause = function
  | arg, (eqn, as_list), None ->
      let pp_as_list = function
        | None -> nop
        | Some (Locus.ArgArg CAst.{ v; loc = _ }) ->
            sequence [ write " as "; pp_or_and_intro_pattern_expr v ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in
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

let pp_raw_atomic_tactic_expr = function
  | Tacexpr.TacInductionDestruct (false, false, clause_list) ->
      sequence
        [ write "destruct "; pp_induction_clause_list clause_list; write "." ]
  | Tacexpr.TacIntroPattern
      (false, [ CAst.{ v = Tactypes.IntroForthcoming _; loc = _ } ]) ->
      write "intros."
  | Tacexpr.TacIntroPattern (false, exprs) ->
      let open CAst in
      sequence
        [
          write "intros ";
          spaced (fun expr -> pp_intro_pattern_expr expr.v) exprs;
          write ".";
        ]
  | Tacexpr.TacReduce (expr, _) -> pp_raw_red_expr expr
  | Tacexpr.TacRewrite
      ( false,
        [ (is_left_to_right, Precisely 1, (None, (expr, NoBindings))) ],
        Locus.{ onhyps = Some []; concl_occs = AllOccurrences },
        None ) ->
      sequence
        [
          write "rewrite ";
          (if is_left_to_right then write "-> " else write "<- ");
          pp_constr_expr expr;
          write ".";
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_gen_tactic_expr_r = function
  | Tacexpr.TacArg arg -> pp_gen_tactic_arg arg
  | Tacexpr.TacAtom atom -> pp_raw_atomic_tactic_expr atom
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_raw_tactic_expr (CAst.{ v; loc = _ } : Tacexpr.raw_tactic_expr) =
  pp_gen_tactic_expr_r v

(* FIXME: This function should be in another module. *)
let raw_tactic_expr_of_raw_generic_argument arg : Tacexpr.raw_tactic_expr option
    =
  (* XXX: I'm not sure if this way is correct. See
     https://coq.zulipchat.com/#narrow/stream/256331-SerAPI/topic/Parsing.20a.20value.20in.20a.20.60GenArg.60. *)
  let open Sexplib.Sexp in
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "tactic" ] ];
        rems;
      ] ->
      Some (Serlib_ltac.Ser_tacexpr.raw_tactic_expr_of_sexp rems)
  | _ -> None

let pp_ltac =
  map_sequence (fun arg ->
      match raw_tactic_expr_of_raw_generic_argument arg with
      | None -> nop
      | Some t -> pp_raw_tactic_expr t)

let pp_proof_bullet = function
  | Proof_bullet.Dash 1 -> write_before_indent "- "
  | Proof_bullet.Plus 1 -> write_before_indent "+ "
  | Proof_bullet.Star 1 -> write_before_indent "* "
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_import_categories { Vernacexpr.negative; import_cats } =
  let open CAst in
  sequence
    [
      space;
      (if negative then write "-" else nop);
      parens (commad (fun import_cat -> write import_cat.v) import_cats);
    ]

let pp_export_with_cats = function
  | Vernacexpr.Export, import_categories ->
      sequence
        [
          space;
          write "Export";
          (match import_categories with
          | None -> nop
          | Some x -> pp_import_categories x);
        ]
  | Vernacexpr.Import, import_categories ->
      sequence
        [
          space;
          write "Import";
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
          parens (commad (fun (filter_name, _) -> pp_qualid filter_name) names);
        ]

let pp_subast CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ }
    =
  let open Vernacexpr in
  match expr with
  | VernacAbort -> sequence [ decrease_indent; write "Abort." ]
  | VernacCheckMayEval (check_or_compute, None, expr) ->
      sequence
        [
          (match check_or_compute with
          | Some (CbvVm None) -> write "Compute "
          | None -> write "Check "
          | _ -> fun printer -> raise (NotImplemented (contents printer)));
          (match expr.v with
          | Constrexpr.CRef _ | Constrexpr.CCast _ -> pp_constr_expr expr
          | _ -> parens (pp_constr_expr expr));
          write ".";
        ]
  | VernacDefineModule (None, name, [], Check [], []) ->
      sequence [ write "Module "; pp_lident name; write "."; increase_indent ]
  | VernacDefinition ((NoDischarge, kind), (name, None), expr) ->
      sequence
        [
          pp_definition_object_kind kind;
          space;
          pp_lname name;
          pp_definition_expr expr;
          write ".";
        ]
  | VernacEndSegment name ->
      sequence [ decrease_indent; write "End "; pp_lident name; write "." ]
  | VernacFixpoint (NoDischarge, [ expr ]) ->
      sequence [ write "Fixpoint "; pp_fixpoint_expr expr ]
  | VernacNotation (false, expr, (notation, modifiers), scope) ->
      let pp_modifiers printer =
        sequence
          [
            space;
            parens
              (commad
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
      sequence
        [
          write "Notation \"";
          pp_lstring notation;
          write "\" := ";
          parens (pp_constr_expr expr);
          (if List.length modifiers > 0 then pp_modifiers else nop);
          pp_scope;
          write ".";
        ]
  | VernacStartTheoremProof (kind, [ ((ident, None), ([], expr)) ]) ->
      sequence
        [
          pp_theorem_kind kind;
          write " ";
          pp_lident ident;
          write " : ";
          pp_constr_expr expr;
          write ".";
        ]
  | VernacProof (None, None) -> sequence [ write "Proof."; increase_indent ]
  | VernacInductive (Inductive_kw, inductives) ->
      let pp_single_inductive = function
        | ( ( (Vernacexpr.NoCoercion, (name, None)),
              ([], None),
              ty,
              Vernacexpr.Constructors constructors ),
            [] ) ->
            sequence
              [
                pp_lident name;
                (match ty with
                | Some ty -> sequence [ write " : "; pp_constr_expr ty ]
                | None -> nop);
                write " :=";
                increase_indent;
                map_sequence pp_construtor_expr constructors;
                decrease_indent;
              ]
        | _ -> fun printer -> raise (NotImplemented (contents printer))
      in
      sequence
        [
          write "Inductive ";
          with_seps
            ~sep:(sequence [ newline; write "with " ])
            pp_single_inductive inductives;
          write ".";
        ]
  (* FIXME: Support other plugins, like ltac2. *)
  | VernacExtend (_, args) -> pp_ltac args
  | VernacEndProof proof_end ->
      sequence [ decrease_indent; pp_proof_end proof_end ]
  | VernacBullet bullet ->
      sequence [ bullet_appears bullet; pp_proof_bullet bullet ]
  | VernacSubproof None -> sequence [ write "{"; increase_indent ]
  | VernacEndSubproof -> sequence [ decrease_indent; write "}" ]
  | VernacRequire (dirpath, export_with_cats, filtered_import) ->
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
        [
          pp_dirpath;
          write "Require";
          pp_categories;
          pp_name_and_filter;
          write ".";
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let separator current next =
  let open Vernacexpr in
  let open CAst in
  match (current.v.expr, next.v.expr) with
  | _, VernacProof _
  | VernacCheckMayEval _, VernacCheckMayEval _
  | VernacNotation _, VernacNotation _
  | VernacDefineModule _, _
  | _, VernacEndSegment _ ->
      newline
  | VernacCheckMayEval _, _
  | _, VernacCheckMayEval _
  | VernacNotation _, _
  | _, VernacNotation _
  | VernacDefinition _, _
  | _, VernacDefinition _
  | VernacFixpoint _, _
  | _, VernacFixpoint _
  | VernacInductive _, _
  | _, VernacInductive _
  | _, VernacDefineModule _
  | VernacEndSegment _, _
  | VernacEndProof _, _ ->
      blankline
  | VernacBullet _, _ -> nop
  | _, _ -> newline

let pp_ast ast =
  let printer = create () in
  let rec loop = function
    | [] -> ()
    | [ x ] ->
        sequence
          [
            pp_subast x;
            (* Given that codes are usually stored in files, it is better to
               append a `\n` at the end if the code is not empty. *)
            newline;
          ]
          printer
    | head :: next :: tail ->
        sequence [ pp_subast head; separator head next ] printer;
        loop (next :: tail)
  in
  loop ast;
  contents printer
