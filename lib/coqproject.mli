val traverse_and_read : unit -> string list
(** [traverse_and_read] traverses the current directory and its parents to find
    the [_CoqProject] file and returns its content as a list of strings.

    This function returns an empty list if the [_CoqProject] file is not found. *)
