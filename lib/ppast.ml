open Printer
open Ltac_plugin

exception NotImplemented of string

(* TODO: Move this function into the appropriate module. *)
let concat xs printer = List.iter (fun x -> x printer) xs
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

let pp_sign = function
  | NumTok.SPlus -> fun _ -> ()
  | NumTok.SMinus -> write "-"

let pp_unsigned n = write (NumTok.Unsigned.sprint n)
let pp_signed (sign, n) = concat [ pp_sign sign; pp_unsigned n ]

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
      concat
        [
          pp_qualid outer;
          concat
            (List.concat
               (List.map
                  (fun value -> [ space; conditional_parens value ])
                  values));
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
      concat
        [
          write prefix;
          with_seps
            ~sep:(concat [ write separator; space ])
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
      concat [ pp_constr_expr outer; space; parens (pp_constr_expr inner) ]
  | Constrexpr.CApp (outer, inners) ->
      let open CAst in
      let conditional_parens expr =
        match expr.v with
        | Constrexpr.CApp _ -> parens (pp_constr_expr expr)
        | _ -> pp_constr_expr expr
      in
      concat
        [
          pp_constr_expr outer;
          concat
            (List.map
               (function
                 | inner, None -> concat [ space; conditional_parens inner ]
                 | _, Some _ ->
                     fun printer -> raise (NotImplemented (contents printer)))
               inners);
        ]
  | Constrexpr.CCases (_, None, matchees, branches) ->
      concat
        [
          write "match ";
          commad pp_case_expr matchees;
          write " with";
          newline;
          concat
            (List.map
               (fun branch -> concat [ pp_branch_expr branch; newline ])
               branches);
          write "end";
        ]
  | Constrexpr.CCast (v, DEFAULTcast, t) ->
      concat [ pp_constr_expr v; write " : "; pp_constr_expr t ]
  | Constrexpr.CIf (cond, (None, None), t, f) ->
      concat
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
  | Constrexpr.CRef (id, None) -> write (Libnames.string_of_qualid id)
  | Constrexpr.CNotation
      (None, (InConstrEntry, init_notation), (init_replacers, [], [], [])) ->
      let rec loop notation replacers =
        match (notation, replacers) with
        | "", [] -> fun _ -> ()
        | "", _ -> fun _ -> failwith "Not all relpacers are consumed."
        | s, [ x ] when String.starts_with ~prefix:"_" s ->
            concat
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
            concat
              [
                conditional_parens h;
                loop (String.sub s 1 (String.length s - 1)) t;
              ]
        | s, _ ->
            concat
              [
                write (String.sub s 0 1);
                loop (String.sub s 1 (String.length s - 1)) replacers;
              ]
      in
      loop init_notation init_replacers
  | Constrexpr.CPrim prim -> pp_prim_token prim
  | Constrexpr.CProdN (xs, CAst.{ v = Constrexpr.CHole _; loc = _ }) ->
      concat (List.map (fun x -> concat [ space; pp_local_binder_expr x ]) xs)
  | Constrexpr.CProdN (xs, ty) ->
      concat
        [
          write "forall ";
          spaced pp_local_binder_expr xs;
          write ", ";
          pp_constr_expr ty;
        ]
  | Constrexpr.CHole (None, IntroAnonymous, None) -> fun _ -> ()
  | Constrexpr.CSort expr -> pp_sort_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_case_expr = function
  | expr, None, None -> pp_constr_expr expr
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_local_binder_expr = function
  | Constrexpr.CLocalAssum
      ( [ name ],
        Constrexpr.Default Explicit,
        CAst.
          {
            v = Constrexpr.CHole (Some (BinderType _), IntroAnonymous, None);
            loc = _;
          } ) ->
      pp_lname name
  | Constrexpr.CLocalAssum (names, Constrexpr.Default Explicit, ty) ->
      parens (concat [ spaced pp_lname names; write " : "; pp_constr_expr ty ])
  | _ -> fun printer -> raise (NotImplemented (contents printer))

and pp_branch_expr = function
  | CAst.{ v = patterns, expr; loc = _ } ->
      concat
        [
          write "| ";
          bard (commad pp_cases_pattern_expr) patterns;
          write " => ";
          pp_constr_expr expr;
        ]

let pp_definition_expr = function
  | Vernacexpr.ProveBody ([], expr) ->
      concat [ write " : "; pp_constr_expr expr ]
  | Vernacexpr.DefineBody (args, None, def_body, return_ty) ->
      concat
        [
          concat
            (List.map
               (fun arg -> concat [ space; pp_local_binder_expr arg ])
               args);
          (match return_ty with
          | None -> fun _ -> ()
          | Some ty -> concat [ write " : "; pp_constr_expr ty ]);
          write " :=";
          newline;
          increase_indent;
          pp_constr_expr def_body;
          decrease_indent;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_proof_end = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) ->
      concat [ clear_bullets; write "Qed." ]
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
      concat
        [
          pp_lident fname;
          space;
          spaced pp_local_binder_expr binders;
          write " : ";
          pp_constr_expr rtype;
          write " :=";
          newline;
          increase_indent;
          pp_constr_expr body_def;
          write ".";
          decrease_indent;
        ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_construtor_expr (_, (name, expr)) =
  concat [ newline; write "| "; pp_lident name; pp_constr_expr expr ]

let pp_syntax_modifier = function
  | Vernacexpr.SetAssoc LeftA -> write "left associativity"
  | Vernacexpr.SetLevel level ->
      concat [ write "at level "; write (string_of_int level) ]
  | _ -> fun printer -> raise (NotImplemented (contents printer))

let pp_gen_tactic_arg = function
  | Tacexpr.TacCall ast -> concat [ pp_qualid (fst ast.v); write "." ]
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

let pp_intro_pattern_naming_expr printer = function
  | Namegen.IntroIdentifier name -> pp_id name printer
  | _ -> raise (NotImplemented (contents printer))

let rec pp_or_and_intro_pattern_expr printer = function
  | Tactypes.IntroOrPattern patterns ->
      let open CAst in
      brackets
        (fun printer ->
          List.iteri
            (fun i pattern ->
              match (i, pattern) with
              | 0, [] -> space printer
              | 0, xs ->
                  spaced
                    (fun x printer -> pp_intro_pattern_expr printer x.v)
                    xs printer
              | _, xs ->
                  write "| " printer;
                  spaced
                    (fun x printer -> pp_intro_pattern_expr printer x.v)
                    xs printer)
            patterns)
        printer
  | _ -> raise (NotImplemented (contents printer))

and pp_intro_pattern_action_expr printer = function
  | Tactypes.IntroOrAndPattern expr -> pp_or_and_intro_pattern_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

and pp_intro_pattern_expr printer = function
  | Tactypes.IntroAction expr -> pp_intro_pattern_action_expr printer expr
  | Tactypes.IntroNaming expr -> pp_intro_pattern_naming_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

let pp_core_destruction_arg printer = function
  | Tactics.ElimOnIdent ident -> pp_lident ident printer
  | _ -> raise (NotImplemented (contents printer))

let pp_destruction_arg printer = function
  | None, arg -> pp_core_destruction_arg printer arg
  | _ -> raise (NotImplemented (contents printer))

let pp_induction_clause printer = function
  | arg, (eqn, as_list), None ->
      let pp_as_list = function
        | None -> ()
        | Some (Locus.ArgArg CAst.{ v; loc = _ }) ->
            write " as " printer;
            pp_or_and_intro_pattern_expr printer v
        | _ -> raise (NotImplemented (contents printer))
      in
      let pp_eqn = function
        | None -> ()
        | Some x ->
            let open CAst in
            write " eqn:" printer;
            pp_intro_pattern_naming_expr printer x.v
      in
      pp_destruction_arg printer arg;
      pp_as_list as_list;
      pp_eqn eqn
  | _ -> raise (NotImplemented (contents printer))

let pp_induction_clause_list printer = function
  | [ clause ], None -> pp_induction_clause printer clause
  | _ -> raise (NotImplemented (contents printer))

let pp_raw_atomic_tactic_expr printer (expr : Tacexpr.raw_atomic_tactic_expr) =
  let open CAst in
  match expr with
  | Tacexpr.TacInductionDestruct (false, false, clause_list) ->
      write "destruct " printer;
      pp_induction_clause_list printer clause_list;
      write "." printer
  | Tacexpr.TacIntroPattern
      (false, [ CAst.{ v = Tactypes.IntroForthcoming _; loc = _ } ]) ->
      write "intros." printer
  | Tacexpr.TacIntroPattern (false, exprs) ->
      write "intros " printer;
      spaced
        (fun expr printer -> pp_intro_pattern_expr printer expr.v)
        exprs printer;
      write "." printer
  | Tacexpr.TacReduce (expr, _) -> pp_raw_red_expr expr printer
  | Tacexpr.TacRewrite
      ( false,
        [ (is_left_to_right, Precisely 1, (None, (expr, NoBindings))) ],
        Locus.{ onhyps = Some []; concl_occs = AllOccurrences },
        None ) ->
      write "rewrite " printer;
      if is_left_to_right then write "-> " printer else write "<- " printer;
      pp_constr_expr expr printer;
      write "." printer
  | _ -> raise (NotImplemented (contents printer))

let pp_gen_tactic_expr_r (expr : Tacexpr.r_dispatch Tacexpr.gen_tactic_expr_r)
    printer =
  match expr with
  | Tacexpr.TacArg arg -> pp_gen_tactic_arg arg printer
  | Tacexpr.TacAtom atom -> pp_raw_atomic_tactic_expr printer atom
  | _ -> raise (NotImplemented (contents printer))

let pp_raw_tactic_expr printer (CAst.{ v; loc = _ } : Tacexpr.raw_tactic_expr) =
  pp_gen_tactic_expr_r v printer

let raw_tactic_expr_of_raw_generic_argument arg : Tacexpr.raw_tactic_expr option
    =
  (* XXX: I'm not sure if this way is correct. See
          https://coq.zulipchat.com/#narrow/stream/256331-SerAPI/topic/Parsing.20a.20value.20in.20a.20.60GenArg.60.
  *)
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

let pp_ltac printer args =
  List.iter
    (fun arg ->
      match raw_tactic_expr_of_raw_generic_argument arg with
      | None -> ()
      | Some t -> pp_raw_tactic_expr printer t)
    args

let pp_proof_bullet printer = function
  | Proof_bullet.Dash 1 -> write_before_indent "- " printer
  | Proof_bullet.Plus 1 -> write_before_indent "+ " printer
  | Proof_bullet.Star 1 -> write_before_indent "* " printer
  | _ -> raise (NotImplemented (contents printer))

let pp_subast printer
    CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
  | VernacAbort ->
      decrease_indent printer;
      write "Abort." printer
  | VernacCheckMayEval (check_or_compute, None, expr) ->
      ((match check_or_compute with
       | Some (CbvVm None) -> write "Compute " printer
       | None -> write "Check " printer
       | _ -> raise (NotImplemented (contents printer)));
       match expr.v with
       | Constrexpr.CRef _ | Constrexpr.CCast _ -> pp_constr_expr expr printer
       | _ -> parens (fun printer -> pp_constr_expr expr printer) printer);
      write "." printer
  | VernacDefineModule (None, name, [], Check [], []) ->
      write "Module " printer;
      pp_lident name printer;
      write "." printer;
      increase_indent printer
  | VernacDefinition ((NoDischarge, kind), (name, None), expr) ->
      pp_definition_object_kind kind printer;
      space printer;
      pp_lname name printer;
      pp_definition_expr expr printer;
      write "." printer
  | VernacEndSegment name ->
      decrease_indent printer;
      write "End " printer;
      pp_lident name printer;
      write "." printer
  | VernacFixpoint (NoDischarge, [ expr ]) ->
      write "Fixpoint " printer;
      pp_fixpoint_expr expr printer
  | VernacNotation (false, expr, (notation, modifiers), scope) ->
      let pp_modifiers () =
        space printer;
        parens
          (fun printer ->
            commad
              (fun modifier printer ->
                let open CAst in
                pp_syntax_modifier modifier.v printer)
              modifiers printer)
          printer
      in
      let pp_scope () =
        match scope with
        | None -> ()
        | Some scope ->
            write " : " printer;
            write scope printer
      in
      write "Notation \"" printer;
      pp_lstring notation printer;
      write "\" := " printer;
      parens (fun printer -> pp_constr_expr expr printer) printer;
      if List.length modifiers > 0 then pp_modifiers ();
      pp_scope ();
      write "." printer
  | VernacStartTheoremProof (kind, [ ((ident, None), ([], expr)) ]) ->
      pp_theorem_kind kind printer;
      write " " printer;
      pp_lident ident printer;
      write " : " printer;
      pp_constr_expr expr printer;
      write "." printer
  | VernacProof (None, None) ->
      write "Proof." printer;
      increase_indent printer
  | VernacInductive (Inductive_kw, inductives) ->
      let pp_single_inductive = function
        | ( ( (Vernacexpr.NoCoercion, (name, None)),
              ([], None),
              ty,
              Vernacexpr.Constructors constructors ),
            [] ) ->
            pp_lident name printer;
            (match ty with
            | Some ty ->
                write " : " printer;
                pp_constr_expr ty printer
            | None -> ());
            write " :=" printer;
            increase_indent printer;
            List.iter (fun x -> pp_construtor_expr x printer) constructors;
            decrease_indent printer
        | _ -> raise (NotImplemented (contents printer))
      in
      write "Inductive " printer;
      with_seps
        ~sep:(fun printer ->
          newline printer;
          write "with " printer)
        (fun inductive _ -> pp_single_inductive inductive)
        inductives printer;
      write "." printer
  (* FIXME: Support other plugins, like ltac2. *)
  | VernacExtend (_, args) -> pp_ltac printer args
  | VernacEndProof proof_end ->
      decrease_indent printer;
      pp_proof_end proof_end printer
  | VernacBullet bullet ->
      bullet_appears bullet printer;
      pp_proof_bullet printer bullet
  | VernacSubproof None ->
      write "{" printer;
      increase_indent printer
  | VernacEndSubproof ->
      decrease_indent printer;
      write "}" printer
  | _ -> raise (NotImplemented (contents printer))

let separator printer current next =
  let open Vernacexpr in
  let open CAst in
  match (current.v.expr, next.v.expr) with
  | _, VernacProof _
  | VernacCheckMayEval _, VernacCheckMayEval _
  | VernacNotation _, VernacNotation _
  | VernacDefineModule _, _
  | _, VernacEndSegment _ ->
      newline printer
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
      blankline printer
  | VernacBullet _, _ -> ()
  | _, _ -> newline printer

let pp_ast ast =
  let printer = create () in
  let rec loop = function
    | [] -> ()
    | [ x ] ->
        pp_subast printer x;
        (* Given that codes are usually stored in files, it is better to append
           a `\n` at the end if the code is not empty. *)
        newline printer
    | head :: next :: tail ->
        pp_subast printer head;
        separator printer head next;
        loop (next :: tail)
  in
  loop ast;
  contents printer
