let read_file path =
  let channel = Stdlib.open_in path in
  let content = In_channel.input_all channel in
  let () = In_channel.close channel in
  content

let test_cases =
  let read_in_out_files dir =
    ( Filename.concat dir "in.v" |> read_file,
      Filename.concat dir "out.v" |> read_file )
  in
  Sys.readdir "coq_files" |> Array.to_list
  |> List.map (Filename.concat "coq_files")
  |> List.filter Sys.is_directory
  |> List.map (fun name ->
         let in_content, out_content = read_in_out_files name in
         (name, in_content, out_content))
  |> List.map (fun (name, in_content, out_content) ->
         Alcotest.test_case name `Quick (fun () ->
             Alcotest.(check string)
               "same string" out_content (Coqfmt.format in_content)))

let () = Alcotest.run "Coqfmt" [ ("format", test_cases) ]
