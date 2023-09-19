let format code =
  let ast, _ = Astparser.make code in
  Ppast.pp_ast ast
