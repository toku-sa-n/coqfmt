open Printer

(* exception NotImplemented of string *)

let pp_raw_tactic_expr printer _ = write printer "reflexivity."

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

let pp_ltac printer args =
  List.iter
    (fun arg ->
      match raw_tactic_expr_of_raw_generic_argument arg with
      | None -> ()
      | Some t -> pp_raw_tactic_expr printer t)
    args
