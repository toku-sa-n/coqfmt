Definition foo :=
  fun x => 
    match x with
    | O => O
    | S n => n
    end.
