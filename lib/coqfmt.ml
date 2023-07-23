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
;;

init_coq ()

let format x =
  generate_ast x
  |> List.map Ppvernac.pr_vernac
  |> List.map Pp.string_of_ppcmds
  |> String.concat "\n"
