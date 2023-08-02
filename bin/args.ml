open Cmdliner

let version =
  match Build_info.V1.version () with
  | None -> failwith "No version found"
  | Some v -> Build_info.V1.Version.to_string v

let doc = "Format the given Coq source code in a uniform style"

let man =
  [
    `S Manpage.s_description;
    `P
      "The $(b,coqfmt) utility formats the given Coq source code in a uniform \
       style. It reads from the standard input and writes to the standard \
       output.";
  ]

let info = Cmd.info "coqfmt" ~version ~doc ~man
let term handler = Cmdliner.Term.(const handler $ const ())
let cmd handler = Cmd.v info (term handler)
