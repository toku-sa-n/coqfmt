let generate_ast code =
  let mode = Ltac_plugin.G_ltac.classic_proof_mode in
  let entry = Pvernac.main_entry (Some mode) in
  let init_parser = Gramlib.Stream.of_string code |> Pcoq.Parsable.make in
  let rec f parser =
    match Pcoq.Entry.parse entry parser with
    | None -> []
    | Some ast -> ast :: f parser
  in
  f init_parser

let pp_ast
    {
      CAst.v = { Vernacexpr.control = _; Vernacexpr.attrs = _; Vernacexpr.expr };
      CAst.loc = _;
    } =
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
      (generate_ast x |> List.map pp_ast |> String.concat "\n") ^ "\n"
