type t = Vernacexpr.vernac_control list

let generate_from_code code =
  (* XXX: I don't know what is the correct value of `doc_type`. *)
  let stm_init_options =
    Stm.
      {
        doc_type = Interactive (TopLogical Names.DirPath.initial);
        injections =
          [ Coqargs.RequireInjection ("Prelude", Some "Coq", Some Import) ];
      }
  in

  let init_doc, init_state = Stm.new_doc stm_init_options in

  let parser = Gramlib.Stream.of_string code |> Pcoq.Parsable.make in
  let rec f doc state acc =
    match Stm.parse_sentence ~doc ~entry:Pvernac.main_entry state parser with
    | None -> List.rev acc
    | Some ast ->
        let next_doc, next_state, _ = Stm.add ~doc ~ontop:state false ast in
        f next_doc next_state (ast :: acc)
  in
  f init_doc init_state []
