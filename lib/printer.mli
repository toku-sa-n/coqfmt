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

val blankline : t -> unit
(** Write out a blank line. *)

val increase_indent : t -> unit
(** Increase the indent level. *)

val decrease_indent : t -> unit
(** Decrease the indent level. *)

val bullet_appears : t -> Proof_bullet.t -> unit
(** Call this function when a bullet appears. *)

val clear_bullets : t -> unit
(** Clear all the bullets. *)

val parens : t -> (unit -> unit) -> unit
(** Write out parentheses around the given function. *)

val brackets : t -> (unit -> unit) -> unit
(** Write out brackets around the given function. *)

val with_seps : sep:(unit -> unit) -> ('a -> unit) -> 'a list -> unit
(** Arrange a series of elements with [~sep] as the delimiter  *)

val commad : t -> ('a -> unit) -> 'a list -> unit
(** Write out a comma-separated list of elements. *)

val spaced : t -> ('a -> unit) -> 'a list -> unit
(** Write out a space-separated list of elements. *)

val contents : t -> string
(** Get the contents of the printer. *)
