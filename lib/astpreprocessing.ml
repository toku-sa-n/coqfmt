let rec filter_proof = function
  | ([] | [ _ ]) as l -> l
  | h1 :: h2 :: xs -> (
      let open CAst in
      let open Vernacexpr in
      match (h1.v.expr, h2.v.expr) with
      | VernacStartTheoremProof _, VernacProof _
      | VernacDefinition _, VernacProof _ ->
          h1 :: filter_proof (h2 :: xs)
      | _, VernacProof _ -> filter_proof (h1 :: xs)
      | _ -> h1 :: filter_proof (h2 :: xs))

let modify_ast_for_pp = filter_proof
