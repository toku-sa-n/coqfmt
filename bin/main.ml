open Cmdliner

let format () =
  let input () =
    let rec loop acc =
      match read_line () with
      | line -> loop (line :: acc)
      | exception End_of_file -> List.rev acc |> String.concat "\n"
    in
    loop []
  in
  let () = Coqfmt.Init.init_coq () in
  input () |> Coqfmt.Format.format |> print_string

let format_t = Cmdliner.Term.(const format $ const ())

let cmd =
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
  Cmd.v info format_t

let () = exit (Cmd.eval cmd)
