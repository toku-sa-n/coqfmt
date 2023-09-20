let format code =
  let ast, comments = Astparser.make code in
  Ppast.pp_ast ast comments
