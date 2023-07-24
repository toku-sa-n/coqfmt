let test_cases =
  let read_in_out_files dir =
    let in_file = Filename.concat dir "in.v" in
    let out_file = Filename.concat dir "out.v" in
    let in_file_channel = Stdlib.open_in in_file in
    let out_file_channel = Stdlib.open_in out_file in
    let in_file_content = In_channel.input_all in_file_channel in
    let out_file_content = In_channel.input_all out_file_channel in
    let () = In_channel.close in_file_channel in
    let () = In_channel.close out_file_channel in
    (in_file_content, out_file_content)
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
