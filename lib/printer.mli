type t
(** Print abstruct type. *)

val create : unit -> t
(** Create a new printer. *)

val write : t -> string -> unit
(** Write out the given string. *)

val space : t -> unit
(** Write out a space. *)

val newline : t -> unit
(** Write out a newline. *)

val increase_indent : t -> unit
(** Increase the indent level. *)

val decrease_indent : t -> unit
(** Decrease the indent level. *)

val parens : t -> (unit -> unit) -> unit
(** Write out parentheses around the given function. *)

val contents : t -> string
(** Get the contents of the printer. *)
