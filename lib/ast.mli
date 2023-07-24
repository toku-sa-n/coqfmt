type t = Vernacexpr.vernac_control list
(** The type of AST. *)

val generate_from_code : string -> Vernacexpr.vernac_control list
(** Generates the AST of the given source code. Unlike normal ASTs like Haskell's
   one, the generated AST is composed of sub-ASTs, and each element corresponds
   to a sentence separated by a dot (`.`).*)
