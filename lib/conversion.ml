(* XXX: I'm not sure if these ways are correct. See
   https://coq.zulipchat.com/#narrow/stream/256331-SerAPI/topic/Parsing.20a.20value.20in.20a.20.60GenArg.60. *)

open Sexplib.Sexp

let raw_tactic_expr_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "tactic" ] ];
        rems;
      ] ->
      Some (Serlib_ltac.Ser_tacexpr.raw_tactic_expr_of_sexp rems)
  | _ -> None

let constr_expr_of_raw_generic_argument arg =
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "constr" ] ];
        rems;
      ]
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "uconstr" ] ];
        rems;
      ] ->
      Some (Serlib.Ser_constrexpr.constr_expr_of_sexp rems)
  | _ -> None
