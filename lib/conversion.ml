(* XXX: I'm not sure if these ways are correct. See
   https://coq.zulipchat.com/#narrow/stream/256331-SerAPI/topic/Parsing.20a.20value.20in.20a.20.60GenArg.60. *)

let raw_tactic_expr_of_raw_generic_argument arg =
  let open Sexplib.Sexp in
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "tactic" ] ];
        rems;
      ] ->
      Some (Serlib_ltac.Ser_tacexpr.raw_tactic_expr_of_sexp rems)
  | _ -> None
