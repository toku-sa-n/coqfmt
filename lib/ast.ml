let generate_from_code code =
  let mode = Ltac_plugin.G_ltac.classic_proof_mode in
  let entry = Pvernac.main_entry (Some mode) in
  let init_parser = Gramlib.Stream.of_string code |> Pcoq.Parsable.make in
  let rec f parser =
    match Pcoq.Entry.parse entry parser with
    | None -> []
    | Some ast -> ast :: f parser
  in
  f init_parser
