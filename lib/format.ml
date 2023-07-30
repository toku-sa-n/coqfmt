let format code = Ast.generate_from_code code |> Ppast.pp_ast
