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
  let buffer = Buffer.create 16 in
  let () =
    List.iter
      (fun subast ->
        let () = pp_subast subast |> Buffer.add_string buffer in
        Buffer.add_char buffer '\n')
      ast
  in
  Buffer.contents buffer
