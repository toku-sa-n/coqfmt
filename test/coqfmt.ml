let test_empty_case () =
  Alcotest.(check string) "same string" "" (Coqfmt.format "")

let test_example () =
  Alcotest.(check string)
    "same string" "Example one_eq_one:1=1. Proof. reflexivity. Qed."
    (Coqfmt.format {|Exampme one_eq_one: 1 = 1.
Proof.
  reflexivity.
Qed.|})

let () =
  Alcotest.run "Coqfmt"
    [
      ( "format",
        [
          Alcotest.test_case "Empty string" `Quick test_empty_case;
          Alcotest.test_case "Example" `Quick test_example;
        ] );
    ]
