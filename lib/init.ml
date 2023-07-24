(* This function follows the instructions written in
   https://coq.github.io/doc/master/api/coq-core/Coqinit/index.html. *)
let init_coq () =
  let args, () =
    Coqinit.parse_arguments
      ~parse_extra:(fun _ -> ((), []))
      ~usage:
        Boot.Usage.
          { executable_name = "coqfmt"; extra_args = ""; extra_options = "" }
      ()
  in
  let cmds = Coqinit.init_runtime args in
  Coqinit.start_library ~top:Names.DirPath.initial cmds
