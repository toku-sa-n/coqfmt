val init_coq : unit -> unit
(** Initialize the Coq environment. This function must be called once and only
    once, and in the very early stage. Note, however, that the handling of
    command-line arguments will be overridden by Coq's one after calling it
    (e.g., passing `--help` as a command-line argument will show Coq's help
    page.) *)
