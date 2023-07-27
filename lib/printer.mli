type t
(** Print abstruct type. *)

val create : unit -> t
(** Create a new printer. *)

val write : t -> string -> unit
(** Write out the given string. *)

val contents : t -> string
(** Get the contents of the printer. *)
