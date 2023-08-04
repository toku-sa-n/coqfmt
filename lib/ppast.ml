open Printer

exception NotImplemented

let pp_id printer id = Names.Id.to_string id |> write printer
let pp_lident printer CAst.{ v; loc = _ } = pp_id printer v

let pp_name printer = function
  | Names.Name name -> pp_id printer name
  | Names.Anonymous -> raise NotImplemented

let pp_lname printer CAst.{ v; loc = _ } = pp_name printer v

let pp_definition_object_kind printer = function
  | Decls.Example -> write printer "Example"
  | Decls.Definition -> write printer "Definition"
  | _ -> raise NotImplemented

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

let rec pp_constr_expr printer CAst.{ v; loc = _ } = pp_constr_expr_r printer v

and pp_constr_expr_r printer = function
  | Constrexpr.CCases _ ->
      write printer "match n with";
      newline printer;
      write printer "| O => S O";
      newline printer;
      write printer "| S n' => S (inc n')";
      newline printer;
      write printer "end"
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
  | _ -> raise NotImplemented

let pp_local_binder_expr printer = function
  | Constrexpr.CLocalAssum ([ name ], Constrexpr.Default Explicit, ty) ->
      write printer "(";
      pp_lname printer name;
      write printer ": ";
      pp_constr_expr printer ty;
      write printer ")"
  | _ -> raise NotImplemented

let pp_definition_expr printer = function
  | Vernacexpr.ProveBody ([], expr) ->
      write printer ": ";
      pp_constr_expr printer expr
  | Vernacexpr.DefineBody ([ arg ], None, def_body, Some return_ty) ->
      space printer;
      pp_local_binder_expr printer arg;
      write printer " : ";
      pp_constr_expr printer return_ty;
      write printer " :=";
      newline printer;
      increase_indent printer;
      pp_constr_expr printer def_body;
      decrease_indent printer
  | _ -> raise NotImplemented

let pp_proof_end printer = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) -> write printer "Qed."
  | _ -> raise NotImplemented

let pp_theorem_kind printer = function
  | Decls.Theorem -> write printer "Theorem"
  | _ -> raise NotImplemented

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
  | _ -> raise NotImplemented

let pp_subast printer
    CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
  | VernacDefinition ((NoDischarge, kind), (name, None), expr) ->
      pp_definition_object_kind printer kind;
      space printer;
      pp_lname printer name;
      pp_definition_expr printer expr;
      write printer "."
  | VernacFixpoint (NoDischarge, [ expr ]) ->
      write printer "Fixpoint ";
      pp_fixpoint_expr printer expr
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
              (fun (_, (name, _)) ->
                newline printer;
                write printer "| ";
                pp_lident printer name)
              constructors;
            decrease_indent printer
        | _ -> raise NotImplemented
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
  | _ -> raise NotImplemented

(* Given that codes are usually stored in files, it is better to append a `\n` at the end if the code is not empty. *)
let pp_ast ast =
  let printer = create () in
  List.iter
    (fun subast ->
      pp_subast printer subast;
      newline printer)
    ast;
  contents printer
