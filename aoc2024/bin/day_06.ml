open Aoc2024

let parse_input =
  List.map (fun line -> List.init (String.length line) (String.get line))

let grid_to_array grid = Array.of_list (List.map Array.of_list grid)

let () =
  let input = File_utils.read_lines "test_data/day_06.dat" in
  input |> parse_input |> grid_to_array |> fun parsed_input ->
  Printf.printf "Number of unique positions: %i\n"
    (Day_06.get_number_unique_positions parsed_input);
  (* Printf.printf "Debugging... %b\n" (Day_06.debug 56 55 parsed_input) *)
  Printf.printf "Number of looping paths: %i\n"
    (Day_06.count_number_of_looping_paths parsed_input)
