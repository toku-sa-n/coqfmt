let format () =
  let input () =
    let rec loop acc =
      match read_line () with
      | line -> loop (line :: acc)
      | exception End_of_file -> List.rev acc |> String.concat "\n"
    in
    loop []
  in
  (* Do not call `Coqfmt.Init.init_coq` before `Cmd.eval`, otherwise everything
     (including help page, etc.) will be overwritten by Coq's ones. *)
  let () = Coqfmt.Init.init_coq () in
  input () |> Coqfmt.Format.format |> print_string

let () = exit (Cmdliner.Cmd.eval (Args.cmd format))
