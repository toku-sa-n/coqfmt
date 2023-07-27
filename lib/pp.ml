exception NotImplemented

let pp_id printer id = Names.Id.to_string id |> Printer.write printer

let pp_lname printer CAst.{ v; loc = _ } =
  match v with
  | Names.Name name -> pp_id printer name
  | Names.Anonymous -> raise NotImplemented

let pp_definition_object_kind printer = function
  | Decls.Example -> Printer.write printer "Example"
  | _ -> raise NotImplemented

let pp_definition_expr printer = function
  | Vernacexpr.ProveBody _ -> Printer.write printer "1 = 1"
  | Vernacexpr.DefineBody _ -> raise NotImplemented

let pp_subast printer
    CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
  | VernacDefinition ((_, kind), (name, _), expr) ->
      pp_definition_object_kind printer kind;
      Printer.space printer;
      pp_lname printer name;
      Printer.write printer ": ";
      pp_definition_expr printer expr;
      Printer.write printer "."
  | VernacProof _ -> Printer.write printer "Proof."
  | VernacExtend _ ->
      Printer.increase_indent printer;
      Printer.write printer "reflexivity."
  | VernacEndProof _ ->
      Printer.decrease_indent printer;
      Printer.write printer "Qed."
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
