type t
(** Print abstruct type. *)

val create : unit -> t
(** Create a new printer. *)

val sequence : (t -> unit) list -> t -> unit
(** Sequencially apply printers from the head of the list. *)

val map_sequence : ('a -> t -> unit) -> 'a list -> t -> unit
(** Map a function to a list and apply printers sequencially. *)

val write : string -> t -> unit
(** Write out the given string. *)

val space : t -> unit
(** Write out a space. *)

val newline : t -> unit
(** Write out a newline. *)

val blankline : t -> unit
(** Write out a blank line. *)

val increase_indent : t -> unit
(** Increase the indent level. *)

val decrease_indent : t -> unit
(** Decrease the indent level. *)

val indented : (t -> unit) -> t -> unit
(** Write out the given function with increased indent level. *)

val write_before_indent : string -> t -> unit
(** Write out the given string before the indent spaces. Mostly for writing bullets. *)

val bullet_appears : Proof_bullet.t -> t -> unit
(** Call this function when a bullet appears. *)

val clear_bullets : t -> unit
(** Clear all the bullets. *)

val parens : (t -> unit) -> t -> unit
(** Write out parentheses around the given function. *)

val brackets : (t -> unit) -> t -> unit
(** Write out brackets around the given function. *)

val map_with_seps : sep:(t -> unit) -> ('a -> t -> unit) -> 'a list -> t -> unit
(** Map the elements of the given list to printers and run them with [~sep] as
  the delimiter. *)

val map_commad : ('a -> t -> unit) -> 'a list -> t -> unit
(** Map the elements of the given list to printers and run them
  comma-separatedly. *)

val map_spaced : ('a -> t -> unit) -> 'a list -> t -> unit
(** Map the elements of the given list to printers and run them
  space-separatedly. *)

val map_bard : ('a -> t -> unit) -> 'a list -> t -> unit
(** Map the elements of the given list to printers and run them
  bar-separatedly. *)

val ( <-|> ) : (t -> unit) -> (t -> unit) -> t -> unit
(** Try running the first printer. If the result fits in the columns limit, use
  the result, and if not, runs the second printer. *)

val contents : t -> string
(** Get the contents of the printer. *)
