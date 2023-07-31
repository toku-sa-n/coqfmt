exception NotImplemented

let pp_id printer id = Names.Id.to_string id |> Printer.write printer

let pp_lident printer CAst.{ v; loc = _ } =
  Names.Id.to_string v |> Printer.write printer

let pp_lname printer CAst.{ v; loc = _ } =
  match v with
  | Names.Name name -> pp_id printer name
  | Names.Anonymous -> raise NotImplemented

let pp_definition_object_kind printer = function
  | Decls.Example -> Printer.write printer "Example"
  | _ -> raise NotImplemented

let pp_sign printer = function
  | NumTok.SPlus -> ()
  | NumTok.SMinus -> Printer.write printer "-"

let pp_unsigned printer n = Printer.write printer (NumTok.Unsigned.sprint n)

let pp_signed printer (sign, n) =
  pp_sign printer sign;
  pp_unsigned printer n

let pp_prim_token printer = function
  | Constrexpr.Number n -> pp_signed printer n
  | Constrexpr.String s -> Printer.write printer s

let rec pp_constr_expr printer CAst.{ v; loc = _ } =
  match v with
  | Constrexpr.CRef (_, None) -> raise NotImplemented
  | Constrexpr.CFix _ -> raise NotImplemented
  | Constrexpr.CCoFix _ -> raise NotImplemented
  | Constrexpr.CProdN _ -> raise NotImplemented
  | Constrexpr.CLambdaN _ -> raise NotImplemented
  | Constrexpr.CLetIn _ -> raise NotImplemented
  | Constrexpr.CAppExpl _ -> raise NotImplemented
  | Constrexpr.CApp _ -> raise NotImplemented
  | Constrexpr.CProj _ -> raise NotImplemented
  | Constrexpr.CRecord _ -> raise NotImplemented
  | Constrexpr.CCases _ -> raise NotImplemented
  | Constrexpr.CLetTuple _ -> raise NotImplemented
  | Constrexpr.CIf _ -> raise NotImplemented
  | Constrexpr.CHole _ -> raise NotImplemented
  | Constrexpr.CPatVar _ -> raise NotImplemented
  | Constrexpr.CEvar _ -> raise NotImplemented
  | Constrexpr.CSort _ -> raise NotImplemented
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
            Printer.write printer (String.sub s 0 1);
            loop (String.sub s 1 (String.length s - 1)) replacers
      in
      loop init_notation init_replacers
  | Constrexpr.CPrim prim -> pp_prim_token printer prim
  | _ -> raise NotImplemented

let pp_definition_expr printer = function
  | Vernacexpr.ProveBody ([], expr) -> pp_constr_expr printer expr
  | _ -> raise NotImplemented

let pp_proof_end printer = function
  | Vernacexpr.Proved (Vernacexpr.Opaque, None) -> Printer.write printer "Qed."
  | _ -> raise NotImplemented

let pp_subast printer
    CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
  | VernacDefinition ((NoDischarge, kind), (name, None), expr) ->
      pp_definition_object_kind printer kind;
      Printer.space printer;
      pp_lname printer name;
      Printer.write printer ": ";
      pp_definition_expr printer expr;
      Printer.write printer "."
  | VernacProof (None, None) ->
      Printer.write printer "Proof.";
      Printer.increase_indent printer
  | VernacInductive
      ( Inductive_kw,
        [
          (((NoCoercion, (name, None)), ([], None), Some _, Constructors _), []);
        ] ) ->
      Printer.write printer "Inductive ";
      pp_lident printer name;
      Printer.write printer ": Type :=";
      Printer.newline printer;
      Printer.increase_indent printer;
      Printer.write printer "| foo";
      Printer.newline printer;
      Printer.write printer "| bar.";
      Printer.decrease_indent printer
  (* FIXME: I have no idea how to extract the complete information of a `VernacExtend`.
     See https://stackoverflow.com/questions/76792174/how-to-extract-the-exact-information-of-genarg. *)
  | VernacExtend _ ->
      Ppvernac.pr_vernac_expr expr
      |> Pp.string_of_ppcmds |> Printer.write printer;
      Printer.write printer "."
  | VernacEndProof proof_end ->
      Printer.decrease_indent printer;
      pp_proof_end printer proof_end
  | _ -> raise NotImplemented

(* Given that codes are usually stored in files, it is better to append a `\n` at the end if the code is not empty. *)
let pp_ast ast =
  let printer = Printer.create () in
  List.iter
    (fun subast ->
      pp_subast printer subast;
      Printer.newline printer)
    ast;
  Printer.contents printer
