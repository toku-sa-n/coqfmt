(* Given that codes are usually stored in files, it is better to append a `\n` at the end if the code is not empty. *)
let format = function
  | "" -> ""
  | x ->
      Init.init_coq ();
      (Ast.generate_from_code x |> List.map Pp.pp_ast |> String.concat "\n")
      ^ "\n"
