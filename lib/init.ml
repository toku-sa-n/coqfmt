(* This function follows the instructions written in
   https://coq.github.io/doc/master/api/coq-core/Coqinit/index.html except
   `start_library` as it is called from `Stm.new_doc`. *)
let init_coq () =
  let _ =
    Coqinit.init_ocaml ();
    Coqinit.parse_arguments
      ~parse_extra:(fun _ -> ((), []))
      ~usage:
        Boot.Usage.
          { executable_name = "coqfmt"; extra_args = ""; extra_options = "" }
      ()
    |> fst |> Coqinit.init_runtime
  in
  Stm.init_core ();
  Stm.init_process Stm.AsyncOpts.default_opts
