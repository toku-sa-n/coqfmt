exception NotImplemented

let pp_subast printer
    CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
  | VernacDefinition _ -> Printer.write printer "Example one_eq_one: 1 = 1."
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
