(* This function follows the instructions written in
   https://coq.github.io/doc/master/api/coq-core/Coqinit/index.html. *)
let init_coq () =
  Coqinit.parse_arguments
    ~parse_extra:(fun _ -> ((), []))
    ~usage:
      Boot.Usage.
        { executable_name = "coqfmt"; extra_args = ""; extra_options = "" }
    ()
  |> fst |> Coqinit.init_runtime
  |> Coqinit.start_library ~top:Names.DirPath.initial
