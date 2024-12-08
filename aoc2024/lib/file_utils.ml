let read_lines filename =
  let file = open_in filename in
  let rec read_lines' acc =
    try
      let line = input_line file in
      read_lines' (line :: acc)
    with End_of_file ->
      close_in file;
      List.rev acc
  in
  read_lines' []
