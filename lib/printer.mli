type t
(** Print abstruct type. *)

val create : unit -> t
(** Create a new printer. *)

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

val write_before_indent : string -> t -> unit
(** Write out the given string before the indent spaces. Mostly for writing bullets. *)

val bullet_appears : Proof_bullet.t -> t -> unit
(** Call this function when a bullet appears. *)

val clear_bullets : t -> unit
(** Clear all the bullets. *)

val parens : (unit -> unit) -> t -> unit
(** Write out parentheses around the given function. *)

val brackets : (unit -> unit) -> t -> unit
(** Write out brackets around the given function. *)

val with_seps : sep:(unit -> unit) -> ('a -> unit) -> 'a list -> unit
(** Arrange a series of elements with [~sep] as the delimiter  *)

val commad : ('a -> unit) -> 'a list -> t -> unit
(** Write out a comma-separated list of elements. *)

val spaced : ('a -> unit) -> 'a list -> t -> unit
(** Write out a space-separated list of elements. *)

val bard : ('a -> unit) -> 'a list -> t -> unit
(** Write out a bar-separated list of elements. *)

val contents : t -> string
(** Get the contents of the printer. *)
