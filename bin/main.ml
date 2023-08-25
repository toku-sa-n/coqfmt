let input () =
  let rec loop acc =
    match read_line () with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc |> String.concat "\n"
  in
  loop []
;;

Coqfmt.init ();
input () |> Coqfmt.format |> print_string
