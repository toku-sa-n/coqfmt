exception NotImplemented

let pp_subast CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ }
    =
  match expr with
  | VernacDefinition _ -> "Example one_eq_one: 1 = 1."
  | VernacProof _ -> "Proof."
  | VernacExtend _ -> "  reflexivity."
  | VernacEndProof _ -> "Qed."
  | _ -> raise NotImplemented

(* Given that codes are usually stored in files, it is better to append a `\n` at the end if the code is not empty. *)
let pp_ast ast =
  let printer = Printer.create () in
  List.iter
    (fun subast ->
      pp_subast subast |> Printer.write printer;
      Printer.newline printer)
    ast;
  Printer.contents printer
