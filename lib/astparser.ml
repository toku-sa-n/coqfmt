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
        doc_type = Interactive (TopLogical Names.DirPath.initial);
        injections =
          [ Coqargs.RequireInjection ("Prelude", Some "Coq", Some Import) ];
      }
  in

  let doc, state = Stm.new_doc stm_init_options in

  let code = Gramlib.Stream.of_string code |> Pcoq.Parsable.make in

  { doc; state; code }

let rec next t =
  match
    Stm.parse_sentence ~doc:t.doc ~entry:Pvernac.main_entry t.state t.code
  with
  | None -> None
  | Some ast ->
      let next_doc, next_state, _ =
        Stm.add ~doc:t.doc ~ontop:t.state false ast
      in
      let prev_ast =
        if t.state = Stateid.initial then None
        else
          match Stm.get_ast ~doc:t.doc t.state with
          | None -> None
          | Some ast -> Some ast.v.expr
      in

      t.doc <- next_doc;
      t.state <- next_state;

      let skip_this_ast =
        let open CAst in
        let open Vernacexpr in
        match (prev_ast, ast.v.expr) with
        | Some (VernacStartTheoremProof _), VernacProof _
        | Some (VernacDefinition _), VernacProof _ ->
            false
        | _, VernacProof _ -> true
        | _, _ -> false
      in

      if skip_this_ast then next t else Some ast
