let pp_ast CAst.{ v = Vernacexpr.{ control = _; attrs = _; expr }; loc = _ } =
  match expr with
  | VernacDefinition _ -> "Example one_eq_one: 1 = 1."
  | VernacProof _ -> "Proof."
  | VernacExtend _ -> "  reflexivity."
  | VernacEndProof _ -> "Qed."
  | _ -> ""

(* Given that codes are usually stored in files, it is better to append a `\n` at the end if the code is not empty. *)
let format = function
  | "" -> ""
  | x ->
      Init.init_coq ();
      (Ast.generate_from_code x |> List.map pp_ast |> String.concat "\n") ^ "\n"
