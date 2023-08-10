open Printer
open Ltac_plugin

exception NotImplemented of string

let pp_id printer id = Names.Id.to_string id |> write printer
let pp_lident printer CAst.{ v; loc = _ } = pp_id printer v

let pp_name printer = function
  | Names.Name name -> pp_id printer name
  | Names.Anonymous -> raise (NotImplemented (contents printer))

let pp_lname printer CAst.{ v; loc = _ } = pp_name printer v
let pp_lstring printer CAst.{ v; loc = _ } = write printer v

let pp_definition_object_kind printer = function
  | Decls.Example -> write printer "Example"
  | Decls.Definition -> write printer "Definition"
  | _ -> raise (NotImplemented (contents printer))

let pp_sign printer = function
  | NumTok.SPlus -> ()
  | NumTok.SMinus -> write printer "-"

let pp_unsigned printer n = write printer (NumTok.Unsigned.sprint n)

let pp_signed printer (sign, n) =
  pp_sign printer sign;
  pp_unsigned printer n

let pp_prim_token printer = function
  | Constrexpr.Number n -> pp_signed printer n
  | Constrexpr.String s -> write printer s

let pp_qualid printer id = Libnames.string_of_qualid id |> write printer

let rec pp_cases_pattern_expr printer CAst.{ v; loc = _ } =
  pp_cases_pattern_expr_r printer v

and pp_cases_pattern_expr_r printer = function
  | Constrexpr.CPatAtom (Some id) -> pp_qualid printer id
  | Constrexpr.CPatAtom None -> write printer "_"
  | Constrexpr.CPatCstr (outer, None, values) ->
      pp_qualid printer outer;
      List.iter
        (fun value ->
          space printer;
          pp_cases_pattern_expr printer value)
        values
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
      write printer prefix;
      with_seps
        ~sep:(fun () ->
          write printer separator;
          space printer)
        (pp_cases_pattern_expr printer)
        exprs;
      write printer suffix
  | _ -> raise (NotImplemented (contents printer))

let rec pp_constr_expr printer CAst.{ v; loc = _ } = pp_constr_expr_r printer v

and pp_constr_expr_r printer = function
  | Constrexpr.CApp
      (outer, [ ((CAst.{ v = Constrexpr.CApp _; loc = _ } as inner), None) ]) ->
      pp_constr_expr printer outer;
      space printer;
      parens printer (fun () -> pp_constr_expr printer inner)
  | Constrexpr.CApp (outer, inners) ->
      pp_constr_expr printer outer;
      List.iter
        (function
          | inner, None ->
              space printer;
              pp_constr_expr printer inner
          | _, Some _ -> raise (NotImplemented (contents printer)))
        inners
  | Constrexpr.CCases (_, None, matchees, branches) ->
      write printer "match ";
      commad printer (pp_case_expr printer) matchees;
      write printer " with";
      newline printer;
      List.iter
        (fun branch ->
          pp_branch_expr printer branch;
          newline printer)
        branches;
      write printer "end"
  | Constrexpr.CCast (v, DEFAULTcast, t) ->
      pp_constr_expr printer v;
      write printer " : ";
      pp_constr_expr printer t
  | Constrexpr.CIf (cond, (None, None), t, f) ->
      write printer "if ";
      pp_constr_expr printer cond;
      newline printer;
      increase_indent printer;
      write printer "then ";
      pp_constr_expr printer t;
      newline printer;
      write printer "else ";
      pp_constr_expr printer f;
      decrease_indent printer
  | Constrexpr.CRef (id, None) -> Libnames.string_of_qualid id |> write printer
  | Constrexpr.CNotation
      (None, (InConstrEntry, init_notation), (init_replacers, [], [], [])) ->
      let rec loop notation replacers =
        match (notation, replacers) with
        | "", [] -> ()
        | "", _ -> failwith "Not all relpacers are consumed."
        | s, h :: t when String.starts_with ~prefix:"_" s ->
            pp_constr_expr printer h;
            loop (String.sub s 1 (String.length s - 1)) t
        | s, _ ->
            write printer (String.sub s 0 1);
            loop (String.sub s 1 (String.length s - 1)) replacers
      in
      loop init_notation init_replacers
  | Constrexpr.CPrim prim -> pp_prim_token printer prim
  | Constrexpr.CProdN (xs, CAst.{ v = Constrexpr.CHole _; loc = _ }) ->
      List.iter
        (fun x ->
          space printer;
          pp_local_binder_expr printer x)
        xs
  | Constrexpr.CProdN (xs, ty) ->
      write printer "forall ";
      spaced printer (pp_local_binder_expr printer) xs;
      write printer ", ";
      pp_constr_expr printer ty
  | Constrexpr.CHole (None, IntroAnonymous, None) -> ()
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
      pp_lname printer name
  | Constrexpr.CLocalAssum (names, Constrexpr.Default Explicit, ty) ->
      parens printer (fun () ->
          spaced printer (pp_lname printer) names;
          write printer " : ";
          pp_constr_expr printer ty)
  | _ -> raise (NotImplemented (contents printer))

and pp_branch_expr printer = function
  | CAst.{ v = [ patterns ], expr; loc = _ } ->
      write printer "| ";
      commad printer (pp_cases_pattern_expr printer) patterns;
      write printer " => ";
      pp_constr_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

let pp_definition_expr printer = function
  | Vernacexpr.ProveBody ([], expr) ->
      write printer " : ";
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
          write printer " : ";
          pp_constr_expr printer ty);
      write printer " :=";
      newline printer;
      increase_indent printer;
      pp_constr_expr printer def_body;
      decrease_indent printer
  | _ -> raise (NotImplemented (contents printer))

let pp_proof_end printer = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) -> write printer "Qed."
  | _ -> raise (NotImplemented (contents printer))

let pp_theorem_kind printer = function
  | Decls.Theorem -> write printer "Theorem"
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
      pp_lident printer fname;
      space printer;
      List.iter (fun binder -> pp_local_binder_expr printer binder) binders;
      write printer " : ";
      pp_constr_expr printer rtype;
      write printer " :=";
      newline printer;
      increase_indent printer;
      pp_constr_expr printer body_def;
      write printer ".";
      decrease_indent printer
  | _ -> raise (NotImplemented (contents printer))

let pp_construtor_expr printer (_, (name, expr)) =
  newline printer;
  write printer "| ";
  pp_lident printer name;
  pp_constr_expr printer expr

let pp_syntax_modifier printer = function
  | Vernacexpr.SetAssoc LeftA -> write printer "left associativity"
  | Vernacexpr.SetLevel level ->
      write printer "at level ";
      write printer (string_of_int level)
  | _ -> raise (NotImplemented (contents printer))

let pp_gen_tactic_arg printer (expr : Tacexpr.raw_tactic_arg) =
  match expr with
  | Tacexpr.TacCall ast ->
      pp_qualid printer (fst ast.v);
      write printer "."
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
      write printer "simpl."
  | _ -> raise (NotImplemented (contents printer))

let pp_intro_pattern_naming_expr printer = function
  | Namegen.IntroIdentifier name -> pp_id printer name
  | _ -> raise (NotImplemented (contents printer))

let pp_intro_pattern_expr printer = function
  | Tactypes.IntroNaming expr -> pp_intro_pattern_naming_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

let pp_raw_atomic_tactic_expr printer (expr : Tacexpr.raw_atomic_tactic_expr) =
  let open CAst in
  match expr with
  | Tacexpr.TacInductionDestruct
      (false, false, ([ ((None, _), (None, None), None) ], None)) ->
      write printer "destruct n."
  | Tacexpr.TacIntroPattern (false, exprs) ->
      write printer "intros ";
      spaced printer (fun expr -> pp_intro_pattern_expr printer expr.v) exprs;
      write printer "."
  | Tacexpr.TacReduce (expr, _) -> pp_raw_red_expr printer expr
  | Tacexpr.TacRewrite
      ( false,
        [ (is_left_to_right, Precisely 1, (None, (expr, NoBindings))) ],
        Locus.{ onhyps = Some []; concl_occs = AllOccurrences },
        None ) ->
      write printer "rewrite ";
      if is_left_to_right then write printer "-> " else write printer "<- ";
      pp_constr_expr printer expr;
      write printer "."
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

let pp_subast printer
    CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
  | VernacAbort ->
      decrease_indent printer;
      write printer "Abort."
  | VernacCheckMayEval (check_or_compute, None, expr) ->
      ((match check_or_compute with
       | Some (CbvVm None) -> write printer "Compute "
       | None -> write printer "Check "
       | _ -> raise (NotImplemented (contents printer)));
       match expr.v with
       | Constrexpr.CRef _ | Constrexpr.CCast _ -> pp_constr_expr printer expr
       | _ -> parens printer (fun () -> pp_constr_expr printer expr));
      write printer "."
  | VernacDefineModule (None, name, [], Check [], []) ->
      write printer "Module ";
      pp_lident printer name;
      write printer ".";
      increase_indent printer
  | VernacDefinition ((NoDischarge, kind), (name, None), expr) ->
      pp_definition_object_kind printer kind;
      space printer;
      pp_lname printer name;
      pp_definition_expr printer expr;
      write printer "."
  | VernacEndSegment name ->
      decrease_indent printer;
      write printer "End ";
      pp_lident printer name;
      write printer "."
  | VernacFixpoint (NoDischarge, [ expr ]) ->
      write printer "Fixpoint ";
      pp_fixpoint_expr printer expr
  | VernacNotation (false, expr, (notation, modifiers), scope) ->
      let pp_modifiers () =
        space printer;
        parens printer (fun () ->
            commad printer
              (fun modifier ->
                let open CAst in
                pp_syntax_modifier printer modifier.v)
              modifiers)
      in
      let pp_scope () =
        match scope with
        | None -> ()
        | Some scope ->
            write printer " : ";
            write printer scope
      in
      write printer "Notation \"";
      pp_lstring printer notation;
      write printer "\" := ";
      parens printer (fun () -> pp_constr_expr printer expr);
      if List.length modifiers > 0 then pp_modifiers ();
      pp_scope ();
      write printer "."
  | VernacStartTheoremProof (kind, [ ((ident, None), ([], expr)) ]) ->
      pp_theorem_kind printer kind;
      write printer " ";
      pp_lident printer ident;
      write printer " : ";
      pp_constr_expr printer expr;
      write printer "."
  | VernacProof (None, None) ->
      write printer "Proof.";
      increase_indent printer
  | VernacInductive (Inductive_kw, inductives) ->
      let pp_single_inductive = function
        | ( ( (Vernacexpr.NoCoercion, (name, None)),
              ([], None),
              ty,
              Vernacexpr.Constructors constructors ),
            [] ) ->
            pp_lident printer name;
            if Option.has_some ty then write printer " : Type";
            write printer " :=";
            increase_indent printer;
            List.iter (pp_construtor_expr printer) constructors;
            decrease_indent printer
        | _ -> raise (NotImplemented (contents printer))
      in
      write printer "Inductive ";
      with_seps
        ~sep:(fun () ->
          newline printer;
          write printer "with ")
        pp_single_inductive inductives;
      write printer "."
  (* FIXME: Support other plugins, like ltac2. *)
  | VernacExtend (_, args) -> pp_ltac printer args
  | VernacEndProof proof_end ->
      decrease_indent printer;
      pp_proof_end printer proof_end
  | _ -> raise (NotImplemented (contents printer))

let separator printer current next =
  let open Vernacexpr in
  let open CAst in
  match (current.v.expr, next.v.expr) with
  | VernacDefinition _, VernacDefinition _
  | VernacInductive _, VernacDefinition _ ->
      blankline printer
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
