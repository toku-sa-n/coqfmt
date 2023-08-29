Theorem foo :
  forall (my_first_parameter my_second_parameter : nat),
    my_first_parameter = my_second_parameter ->
    my_first_parameter = my_second_parameter.
Proof.
  intros my_first_parameter my_second_parameter H.
  rewrite -> H.
  reflexivity.
Qed.
