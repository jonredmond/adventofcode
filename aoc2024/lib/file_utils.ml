let read_lines filename =
  let file = open_in filename in
  let rec read_lines' acc =
    try read_lines' (input_line file :: acc)
    with End_of_file ->
      close_in file;
      List.rev acc
  in
  read_lines' []
