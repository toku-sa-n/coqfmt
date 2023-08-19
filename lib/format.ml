let format code =
  Ast.generate_from_code code
  |> Astpreprocessing.modify_ast_for_pp |> Ppast.pp_ast
