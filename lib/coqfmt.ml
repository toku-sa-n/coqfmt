let init_coq () =
  let args, () =
    Coqinit.parse_arguments
      ~parse_extra:(fun _ -> ((), []))
      ~usage:
        Boot.Usage.
          { executable_name = "coqfmt"; extra_args = ""; extra_options = "" }
      ()
  in
  let cmds = Coqinit.init_runtime args in
  Coqinit.start_library ~top:Names.DirPath.initial cmds

let generate_ast code =
  let mode = Ltac_plugin.G_ltac.classic_proof_mode in
  let entry = Pvernac.main_entry (Some mode) in
  let code_stream = Gramlib.Stream.of_string code in
  let init_parser = Pcoq.Parsable.make code_stream in
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
;;

init_coq ()

let format x = generate_ast x |> List.map pp_ast |> String.concat "\n"
