open Aoc2024

let () =
  let input = File_utils.read_lines "test_data/day_03.dat" in
  let combined_input = String.concat "" input in
  Printf.printf "Raw result: %d\n" (Day_03.mul_groups combined_input);
  Printf.printf "Removing disabled commands: %d\n"
    (Day_03.mul_groups_ignore_disabled combined_input)
