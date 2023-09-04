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

let constr_expr_of_raw_generic_argument arg : Constrexpr.constr_expr option =
  let open Sexplib.Sexp in
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

let intro_pattern_list_of_raw_generic_argument arg =
  let open Sexplib.Sexp in
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List
          [
            Atom "Rawwit";
            List
              [
                Atom "ListArg";
                List [ Atom "ExtraArg"; Atom "simple_intropattern" ];
              ];
          ];
        List rems;
      ] ->
      Some (List.map Serlib_ltac.Ser_tacexpr.intro_pattern_of_sexp rems)
  | _ -> None

let destruction_arg_of_raw_generic_argument arg =
  let open Sexplib.Sexp in
  match Serlib.Ser_genarg.sexp_of_raw_generic_argument arg with
  | List
      [
        Atom "GenArg";
        List [ Atom "Rawwit"; List [ Atom "ExtraArg"; Atom "destruction_arg" ] ];
        rems;
      ] ->
      let arg_parser = function
        | List [ _; expr ] ->
            (Serlib.Ser_constrexpr.constr_expr_of_sexp expr, Tactypes.NoBindings)
        | _ -> failwith "TODO"
      in
      Some (Serlib.Ser_tactics.destruction_arg_of_sexp arg_parser rems)
  | _ -> None
