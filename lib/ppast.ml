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

and pp_cases_pattern_expr_r expr printer =
  match expr with
  | Constrexpr.CPatAtom (Some id) -> pp_qualid id printer
  | Constrexpr.CPatAtom None -> write "_" printer
  (* Cstr seems to mean 'Constructor'. (e.g., `S (S O)`, `Foo 0 1`) *)
  | Constrexpr.CPatCstr (outer, None, values) ->
      let open CAst in
      let conditional_parens expr =
        match expr.v with
        | Constrexpr.CPatAtom _ -> pp_cases_pattern_expr expr
        | _ -> parens (fun () -> pp_cases_pattern_expr expr printer)
      in
      pp_qualid outer printer;
      concat
        (List.concat
           (List.map (fun value -> [ space; conditional_parens value ]) values))
        printer
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
      write prefix printer;
      with_seps
        ~sep:(fun printer ->
          write separator printer;
          space printer)
        (fun x -> pp_cases_pattern_expr x printer)
        exprs printer;
      write suffix printer
  | Constrexpr.CPatPrim token -> pp_prim_token token printer
  | Constrexpr.CPatOr xs ->
      parens
        (fun () -> bard (fun x -> pp_cases_pattern_expr x printer) xs printer)
        printer
  | _ -> raise (NotImplemented (contents printer))

let pp_sort_expr printer = function
  | Glob_term.UAnonymous { rigid = true } -> write "Type" printer
  | Glob_term.UAnonymous { rigid = false } ->
      raise (NotImplemented (contents printer))
  | Glob_term.UNamed [ (Constrexpr.CSet, 0) ] -> write "Set" printer
  | Glob_term.UNamed _ -> raise (NotImplemented (contents printer))

let rec pp_constr_expr printer CAst.{ v; loc = _ } = pp_constr_expr_r printer v

and pp_constr_expr_r printer = function
  | Constrexpr.CApp
      (outer, [ ((CAst.{ v = Constrexpr.CApp _; loc = _ } as inner), None) ]) ->
      pp_constr_expr printer outer;
      space printer;
      parens (fun () -> pp_constr_expr printer inner) printer
  | Constrexpr.CApp (outer, inners) ->
      let open CAst in
      let conditional_parens expr =
        match expr.v with
        | Constrexpr.CApp _ ->
            parens (fun () -> pp_constr_expr printer expr) printer
        | _ -> pp_constr_expr printer expr
      in
      pp_constr_expr printer outer;
      List.iter
        (function
          | inner, None ->
              space printer;
              conditional_parens inner
          | _, Some _ -> raise (NotImplemented (contents printer)))
        inners
  | Constrexpr.CCases (_, None, matchees, branches) ->
      write "match " printer;
      commad (pp_case_expr printer) matchees printer;
      write " with" printer;
      newline printer;
      List.iter
        (fun branch ->
          pp_branch_expr printer branch;
          newline printer)
        branches;
      write "end" printer
  | Constrexpr.CCast (v, DEFAULTcast, t) ->
      pp_constr_expr printer v;
      write " : " printer;
      pp_constr_expr printer t
  | Constrexpr.CIf (cond, (None, None), t, f) ->
      write "if " printer;
      pp_constr_expr printer cond;
      newline printer;
      increase_indent printer;
      write "then " printer;
      pp_constr_expr printer t;
      newline printer;
      write "else " printer;
      pp_constr_expr printer f;
      decrease_indent printer
  | Constrexpr.CRef (id, None) -> write (Libnames.string_of_qualid id) printer
  | Constrexpr.CNotation
      (None, (InConstrEntry, init_notation), (init_replacers, [], [], [])) ->
      let rec loop notation replacers =
        match (notation, replacers) with
        | "", [] -> ()
        | "", _ -> failwith "Not all relpacers are consumed."
        | s, [ x ] when String.starts_with ~prefix:"_" s ->
            pp_constr_expr printer x;
            loop (String.sub s 1 (String.length s - 1)) []
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
              if parens_needed then
                parens (fun () -> pp_constr_expr printer expr) printer
              else pp_constr_expr printer expr
            in
            conditional_parens h;
            loop (String.sub s 1 (String.length s - 1)) t
        | s, _ ->
            write (String.sub s 0 1) printer;
            loop (String.sub s 1 (String.length s - 1)) replacers
      in
      loop init_notation init_replacers
  | Constrexpr.CPrim prim -> pp_prim_token prim printer
  | Constrexpr.CProdN (xs, CAst.{ v = Constrexpr.CHole _; loc = _ }) ->
      List.iter
        (fun x ->
          space printer;
          pp_local_binder_expr printer x)
        xs
  | Constrexpr.CProdN (xs, ty) ->
      write "forall " printer;
      spaced (pp_local_binder_expr printer) xs printer;
      write ", " printer;
      pp_constr_expr printer ty
  | Constrexpr.CHole (None, IntroAnonymous, None) -> ()
  | Constrexpr.CSort expr -> pp_sort_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

and pp_case_expr printer = function
  | expr, None, None -> pp_constr_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

and pp_local_binder_expr printer = function
  | Constrexpr.CLocalAssum
      ( [ name ],
        Constrexpr.Default Explicit,
        CAst.
          {
            v = Constrexpr.CHole (Some (BinderType _), IntroAnonymous, None);
            loc = _;
          } ) ->
      pp_lname name printer
  | Constrexpr.CLocalAssum (names, Constrexpr.Default Explicit, ty) ->
      parens
        (fun () ->
          spaced (fun x -> pp_lname x printer) names printer;
          write " : " printer;
          pp_constr_expr printer ty)
        printer
  | _ -> raise (NotImplemented (contents printer))

and pp_branch_expr printer = function
  | CAst.{ v = patterns, expr; loc = _ } ->
      write "| " printer;
      bard
        (fun xs -> commad (fun x -> pp_cases_pattern_expr x printer) xs printer)
        patterns printer;
      write " => " printer;
      pp_constr_expr printer expr

let pp_definition_expr printer = function
  | Vernacexpr.ProveBody ([], expr) ->
      write " : " printer;
      pp_constr_expr printer expr
  | Vernacexpr.DefineBody (args, None, def_body, return_ty) ->
      List.iter
        (fun arg ->
          space printer;
          pp_local_binder_expr printer arg)
        args;
      (match return_ty with
      | None -> ()
      | Some ty ->
          write " : " printer;
          pp_constr_expr printer ty);
      write " :=" printer;
      newline printer;
      increase_indent printer;
      pp_constr_expr printer def_body;
      decrease_indent printer
  | _ -> raise (NotImplemented (contents printer))

let pp_proof_end printer = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) ->
      clear_bullets printer;
      write "Qed." printer
  | _ -> raise (NotImplemented (contents printer))

let pp_theorem_kind printer = function
  | Decls.Theorem -> write "Theorem" printer
  | _ -> raise (NotImplemented (contents printer))

let pp_fixpoint_expr printer = function
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
      pp_lident fname printer;
      space printer;
      spaced (pp_local_binder_expr printer) binders printer;
      write " : " printer;
      pp_constr_expr printer rtype;
      write " :=" printer;
      newline printer;
      increase_indent printer;
      pp_constr_expr printer body_def;
      write "." printer;
      decrease_indent printer
  | _ -> raise (NotImplemented (contents printer))

let pp_construtor_expr printer (_, (name, expr)) =
  newline printer;
  write "| " printer;
  pp_lident name printer;
  pp_constr_expr printer expr

let pp_syntax_modifier printer = function
  | Vernacexpr.SetAssoc LeftA -> write "left associativity" printer
  | Vernacexpr.SetLevel level ->
      write "at level " printer;
      write (string_of_int level) printer
  | _ -> raise (NotImplemented (contents printer))

let pp_gen_tactic_arg printer (expr : Tacexpr.raw_tactic_arg) =
  match expr with
  | Tacexpr.TacCall ast ->
      pp_qualid (fst ast.v) printer;
      write "." printer
  | _ -> raise (NotImplemented (contents printer))

let pp_raw_red_expr printer (expr : Tacexpr.raw_red_expr) =
  match expr with
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
      write "simpl." printer
  | _ -> raise (NotImplemented (contents printer))

let pp_intro_pattern_naming_expr printer = function
  | Namegen.IntroIdentifier name -> pp_id name printer
  | _ -> raise (NotImplemented (contents printer))

let rec pp_or_and_intro_pattern_expr printer = function
  | Tactypes.IntroOrPattern patterns ->
      let open CAst in
      brackets
        (fun () ->
          List.iteri
            (fun i pattern ->
              match (i, pattern) with
              | 0, [] -> space printer
              | 0, xs ->
                  spaced (fun x -> pp_intro_pattern_expr printer x.v) xs printer
              | _, xs ->
                  write "| " printer;
                  spaced (fun x -> pp_intro_pattern_expr printer x.v) xs printer)
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
      spaced (fun expr -> pp_intro_pattern_expr printer expr.v) exprs printer;
      write "." printer
  | Tacexpr.TacReduce (expr, _) -> pp_raw_red_expr printer expr
  | Tacexpr.TacRewrite
      ( false,
        [ (is_left_to_right, Precisely 1, (None, (expr, NoBindings))) ],
        Locus.{ onhyps = Some []; concl_occs = AllOccurrences },
        None ) ->
      write "rewrite " printer;
      if is_left_to_right then write "-> " printer else write "<- " printer;
      pp_constr_expr printer expr;
      write "." printer
  | _ -> raise (NotImplemented (contents printer))

let pp_gen_tactic_expr_r printer
    (expr : Tacexpr.r_dispatch Tacexpr.gen_tactic_expr_r) =
  match expr with
  | Tacexpr.TacArg arg -> pp_gen_tactic_arg printer arg
  | Tacexpr.TacAtom atom -> pp_raw_atomic_tactic_expr printer atom
  | _ -> raise (NotImplemented (contents printer))

let pp_raw_tactic_expr printer (CAst.{ v; loc = _ } : Tacexpr.raw_tactic_expr) =
  pp_gen_tactic_expr_r printer v

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
       | Constrexpr.CRef _ | Constrexpr.CCast _ -> pp_constr_expr printer expr
       | _ -> parens (fun () -> pp_constr_expr printer expr) printer);
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
      pp_definition_expr printer expr;
      write "." printer
  | VernacEndSegment name ->
      decrease_indent printer;
      write "End " printer;
      pp_lident name printer;
      write "." printer
  | VernacFixpoint (NoDischarge, [ expr ]) ->
      write "Fixpoint " printer;
      pp_fixpoint_expr printer expr
  | VernacNotation (false, expr, (notation, modifiers), scope) ->
      let pp_modifiers () =
        space printer;
        parens
          (fun () ->
            commad
              (fun modifier ->
                let open CAst in
                pp_syntax_modifier printer modifier.v)
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
      parens (fun () -> pp_constr_expr printer expr) printer;
      if List.length modifiers > 0 then pp_modifiers ();
      pp_scope ();
      write "." printer
  | VernacStartTheoremProof (kind, [ ((ident, None), ([], expr)) ]) ->
      pp_theorem_kind printer kind;
      write " " printer;
      pp_lident ident printer;
      write " : " printer;
      pp_constr_expr printer expr;
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
                pp_constr_expr printer ty
            | None -> ());
            write " :=" printer;
            increase_indent printer;
            List.iter (pp_construtor_expr printer) constructors;
            decrease_indent printer
        | _ -> raise (NotImplemented (contents printer))
      in
      write "Inductive " printer;
      with_seps
        ~sep:(fun printer ->
          newline printer;
          write "with " printer)
        pp_single_inductive inductives printer;
      write "." printer
  (* FIXME: Support other plugins, like ltac2. *)
  | VernacExtend (_, args) -> pp_ltac printer args
  | VernacEndProof proof_end ->
      decrease_indent printer;
      pp_proof_end printer proof_end
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
