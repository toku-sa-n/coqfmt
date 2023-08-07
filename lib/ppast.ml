open Printer

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
  | Constrexpr.CPatCstr (outer, None, [ expr ]) ->
      pp_qualid printer outer;
      space printer;
      pp_cases_pattern_expr printer expr
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
      List.iteri
        (fun i expr ->
          match i with
          | 0 -> pp_cases_pattern_expr printer expr
          | _ ->
              write printer separator;
              space printer;
              pp_cases_pattern_expr printer expr)
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
  | Constrexpr.CCases (_, None, [ matchee ], branches) ->
      write printer "match ";
      pp_case_expr printer matchee;
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
  | Constrexpr.CProdN (_, _) -> write printer " (x: foo) (y: foo)"
  | Constrexpr.CHole (_, _, _) -> ()
  | _ -> raise (NotImplemented (contents printer))

and pp_case_expr printer = function
  | expr, None, None -> pp_constr_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

and pp_branch_expr printer = function
  | CAst.{ v = [ [ pattern ] ], expr; loc = _ } ->
      write printer "| ";
      pp_cases_pattern_expr printer pattern;
      write printer " => ";
      pp_constr_expr printer expr
  | _ -> raise (NotImplemented (contents printer))

let pp_local_binder_expr printer = function
  | Constrexpr.CLocalAssum ([ name ], Constrexpr.Default Explicit, ty) ->
      parens printer (fun () ->
          pp_lname printer name;
          write printer ": ";
          pp_constr_expr printer ty)
  | _ -> raise (NotImplemented (contents printer))

let pp_definition_expr printer = function
  | Vernacexpr.ProveBody ([], expr) ->
      write printer ": ";
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

let pp_subast printer
    CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
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
  | VernacNotation (false, expr, (notation, []), None) ->
      write printer "Notation \"";
      pp_lstring printer notation;
      write printer "\" := (";
      pp_constr_expr printer expr;
      write printer ")."
  | VernacStartTheoremProof (kind, [ ((ident, None), ([], expr)) ]) ->
      pp_theorem_kind printer kind;
      write printer " ";
      pp_lident printer ident;
      write printer ": ";
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
            if Option.has_some ty then write printer ": Type";
            write printer " :=";
            increase_indent printer;
            List.iter
              (fun (_, (name, expr)) ->
                newline printer;
                write printer "| ";
                pp_lident printer name;
                pp_constr_expr printer expr)
              constructors;
            decrease_indent printer
        | _ -> raise (NotImplemented (contents printer))
      in
      List.iteri
        (fun i inductive ->
          match i with
          | 0 ->
              write printer "Inductive ";
              pp_single_inductive inductive
          | _ ->
              newline printer;
              write printer "with ";
              pp_single_inductive inductive)
        inductives;
      write printer "."
  (* FIXME: I have no idea how to extract the complete information of a `VernacExtend`.
     See https://stackoverflow.com/questions/76792174/how-to-extract-the-exact-information-of-genarg. *)
  | VernacExtend _ ->
      Ppvernac.pr_vernac_expr expr |> Pp.string_of_ppcmds |> write printer;
      write printer "."
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
