open Aoc2024

let parse_input lines =
  let result_constants_split = String.split_on_char ':' lines in
  let result = int_of_string (List.nth result_constants_split 0) in
  let equation_constants =
    List.map int_of_string
      (List.filter
         (fun x -> x <> "")
         (String.split_on_char ' ' (List.nth result_constants_split 1)))
  in
  (result, equation_constants)

let () =
  let input = File_utils.read_lines "test_data/day_07.dat" in
  input |> List.map parse_input |> fun parsed_input ->
  Printf.printf "Sum of valid equation combinations: %i\n"
    (Day_07.sum_valid_equation_combinations parsed_input)
