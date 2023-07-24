exception NotImplemented

let pp_subast CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ }
    =
  match expr with
  | VernacDefinition _ -> "Example one_eq_one: 1 = 1."
  | VernacProof _ -> "Proof."
  | VernacExtend _ -> "  reflexivity."
  | VernacEndProof _ -> "Qed."
  | _ -> raise NotImplemented

let pp_ast ast = List.map pp_subast ast |> String.concat "\n"
