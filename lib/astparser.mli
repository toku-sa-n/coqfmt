type t
(** AST parser type. *)

val make : string -> t * ((int * int) * string) list
(** Create a new parser from the given Coq source code. *)

val next : t -> Vernacexpr.vernac_control option
(** Get the next AST node from the parser. *)
