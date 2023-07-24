(* Do not put `Init.init_coq ()` inside `format`, otherwise an internal error in
   Coq will happen

   TODO: Is it correct to put this here? *)
Init.init_coq ()

let format code = Ast.generate_from_code code |> Pp.pp_ast
