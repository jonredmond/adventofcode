open Aoc2024

let parse_input lines =
  lines |> List.map (fun line -> String.to_seq line |> List.of_seq)

let () =
  let input = File_utils.read_lines "test_data/day_04.dat" in
  input |> parse_input |> fun parsed_input ->
  Printf.printf "Number of words found: %i\n"
    (Day_04.count_valid_paths parsed_input);
  Printf.printf "Number of X Mas': %i\n"
    (Day_04.count_valid_x_mas_paths parsed_input)
