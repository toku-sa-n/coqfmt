let input () =
  let rec loop acc =
    match read_line () with
    | line -> loop (line :: acc)
    | exception End_of_file -> List.rev acc |> String.concat "\n"
  in
  loop []
;;

Rocqfmt.init ();
input () |> Rocqfmt.format |> print_string
