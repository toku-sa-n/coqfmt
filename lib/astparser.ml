type t = {
  mutable doc : Stm.doc;
  mutable state : Stateid.t;
  code : Pcoq.Parsable.t;
}

let make code =
  (* XXX: I don't know what is the correct value of `doc_type`. *)
  let stm_init_options =
    Stm.
      {
        doc_type = Interactive (TopLogical Coqargs.default_toplevel);
        injections =
          [
            Coqargs.RequireInjection
              { lib = "Prelude"; prefix = Some "Coq"; export = Some Import };
          ];
      }
  in

  let doc, state = Stm.new_doc stm_init_options in

  let code = Gramlib.Stream.of_string code |> Pcoq.Parsable.make in

  { doc; state; code }

let add_ast t ast =
  let next_doc, next_state, _ = Stm.add ~doc:t.doc ~ontop:t.state false ast in

  t.doc <- next_doc;
  t.state <- next_state

let rec next t =
  let prev_ast =
    if t.state = Stateid.initial then None else Stm.get_ast ~doc:t.doc t.state
  in
  let next_ast =
    Stm.parse_sentence ~doc:t.doc ~entry:Pvernac.main_entry t.state t.code
  in

  match (prev_ast, next_ast) with
  | _, None -> None
  | None, Some ast ->
      let () = add_ast t ast in
      Some ast
  | Some prev_ast, Some next_ast ->
      let () = add_ast t next_ast in

      let skip_this_ast =
        let open CAst in
        let open Vernacexpr in
        match (prev_ast.v.expr, next_ast.v.expr) with
        | ( VernacSynPure (VernacStartTheoremProof _),
            VernacSynPure (VernacProof _) )
        | VernacSynPure (VernacDefinition _), VernacSynPure (VernacProof _) ->
            false
        | _, VernacSynPure (VernacProof _) -> true
        | _, _ -> false
      in

      if skip_this_ast then next t else Some next_ast
