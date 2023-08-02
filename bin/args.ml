open Cmdliner

let cmd handler =
  let doc = "Format the given Coq source code in a uniform style" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The $(b,coqfmt) utility formats the given Coq source code in a \
         uniform style. It reads from the standard input and writes to the \
         standard output.";
    ]
  in
  let version =
    match Build_info.V1.version () with
    | None -> failwith "No version found"
    | Some v -> Build_info.V1.Version.to_string v
  in
  let info = Cmd.info "coqfmt" ~version ~doc ~man in
  let term = Cmdliner.Term.(const handler $ const ()) in
  Cmd.v info term
