open Aoc2024

let parse_input =
  List.map (fun line -> List.init (String.length line) (String.get line))

let grid_to_array grid = Array.of_list (List.map Array.of_list grid)

let () =
  let input = File_utils.read_lines "test_data/day_08.dat" in
  input |> parse_input |> grid_to_array |> fun grid ->
  Printf.printf "Number of distinct antinodes: %i\n"
    (Day_08.count_all_distinct_antinodes grid)
