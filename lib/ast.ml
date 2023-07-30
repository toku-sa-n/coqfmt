type t = Vernacexpr.vernac_control list

let generate_from_code code =
  let mode = Ltac_plugin.G_ltac.classic_proof_mode in
  let entry = Pvernac.main_entry (Some mode) in
  let parser = Gramlib.Stream.of_string code |> Pcoq.Parsable.make in
  let rec f acc =
    match Pcoq.Entry.parse entry parser with
    | None -> List.rev acc
    | Some ast -> f (ast :: acc)
  in
  f []
