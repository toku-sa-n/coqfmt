(* This function follows the instructions written in
   https://coq.github.io/doc/master/api/coq-core/Coqinit/index.html except
   `start_library` as it is called from `Stm.new_doc`. *)
let init () =
  let usage =
    Boot.Usage.
      { executable_name = "rocqfmt"; extra_args = ""; extra_options = "" }
  in

  let initial_args =
    let args_from_coqproject = Coqproject.traverse_and_read () in

    Coqargs.parse_args ~init:Coqargs.default args_from_coqproject |> fst
  in

  Coqinit.init_ocaml ();
  let opts, _ =
    Coqinit.parse_arguments
      ~parse_extra:(fun _ args -> ((), args))
      ~initial_args (List.tl (Array.to_list Sys.argv))
  in
  Coqinit.init_runtime ~usage opts;
  Coqinit.init_document opts;
  Stm.init_core ();
  Stm.init_process (Stm.AsyncOpts.default_opts ~spawn_args:[])
