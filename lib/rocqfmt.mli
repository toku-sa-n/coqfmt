(** {1 Initialization} *)

val init : unit -> unit
(** Initialize the Coq environment and the handler of command line arguments.

    This function must be called once and only once, and in the very early
    stage. *)

(** {1 Format} *)

val format : string -> string
(** [format] formats the given Coq source code in a uniform style. The formatted
    code has a [\n] at the end if the input is not empty. *)
