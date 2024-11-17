let file_exists_in_dir dir filename =
  Sys.file_exists (Filename.concat dir filename)

let read_file file_path =
  let ic = open_in file_path in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in

  read_lines []

let rec find_coqproject dir =
  let coqproject_filename = "_CoqProject" in

  if file_exists_in_dir dir coqproject_filename then
    read_file (Filename.concat dir coqproject_filename)
  else if dir = "/" then []
  else find_coqproject (Filename.dirname dir)

let traverse_and_read () =
  let current_dir = Sys.getcwd () in
  let lines = find_coqproject current_dir in
  let single_line = String.concat " " lines in

  String.split_on_char ' ' single_line
