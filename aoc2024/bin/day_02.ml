open Aoc2024

let parse_input lines =
  lines
  |> List.map (fun line ->
         String.split_on_char ' ' line
         |> List.filter (fun s -> s <> "")
         |> List.map int_of_string)

let () =
  let input = File_utils.read_lines "test_data/day_02.dat" in
  input |> parse_input |> fun parsed_input ->
  Printf.printf "Number of safe lines: %i\n"
    (Day_02.count_safe_lines parsed_input);
  Printf.printf "Number of safe lines with problem dampener: %i\n"
    (Day_02.count_safe_lines_problem_dampener parsed_input);
  Printf.printf "Number of safe lines with brute force: %i\n"
    (Day_02.count_brute_force_safe_lines_problem_dampener parsed_input);
  Printf.printf "Different lines: %s\n"
    (String.concat "\n"
       (List.map
          (fun x -> String.concat ", " (List.map string_of_int x))
          (Day_02.compare_brute_force_to_problem_dampener parsed_input)))
