let format code = Ast.generate_from_code code |> Pp.pp_ast
