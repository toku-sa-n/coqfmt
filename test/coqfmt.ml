let test_empty_case () =
  Alcotest.(check string) "same string" "" (Coqfmt.format "")

let () =
  Alcotest.run "Coqfmt"
    [ ("format", [ Alcotest.test_case "Empty string" `Quick test_empty_case ]) ]
