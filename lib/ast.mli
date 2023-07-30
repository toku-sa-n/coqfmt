type t = Vernacexpr.vernac_control list
(** The type of AST. *)

val generate_from_code : string -> t
(** Generates the AST of the given source code. *)
