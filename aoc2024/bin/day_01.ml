open Aoc2024

let parse_input lines =
  lines
  |> List.map (fun line ->
         let split_line =
           String.split_on_char ' ' line
           |> List.filter (fun s -> s <> "")
           |> List.map int_of_string
         in
         (List.nth split_line 0, List.nth split_line 1))
  |> List.split

let () =
  let input = File_utils.read_lines "test_data/day_01.dat" in
  input |> parse_input |> Day_01.sum_diff |> Printf.printf "Difference: %i\n";
  input |> parse_input |> Day_01.similarity_score
  |> Printf.printf "Similarity score: %i\n"
