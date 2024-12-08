open Aoc2024

let parse_rule line =
  let split_line =
    String.split_on_char '|' line
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
  in
  (List.nth split_line 0, List.nth split_line 1)

let parse_page_update line =
  String.split_on_char ',' line
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

let parse_input lines =
  let rec parse_input' rules page_updates is_page_update = function
    | [] -> (rules, page_updates)
    | "" :: xs -> parse_input' rules page_updates true xs
    | line :: xs -> (
        match is_page_update with
        | true ->
            parse_input' rules (parse_page_update line :: page_updates) true xs
        | false -> parse_input' (parse_rule line :: rules) page_updates false xs
        )
  in
  parse_input' [] [] false lines

let () =
  let input = File_utils.read_lines "test_data/day_05.dat" in
  input |> parse_input |> fun (rules, page_updates) ->
  let rule_graph = Day_05.generate_rule_graph rules in
  let valid_page_updates =
    List.filter
      (fun page_update -> Day_05.validate_page_update rule_graph page_update)
      page_updates
  in
  let invalid_page_updates =
    List.filter
      (fun page_update ->
        not (Day_05.validate_page_update rule_graph page_update))
      page_updates
  in
  let ordered_page_updates =
    List.map (Day_05.order_page_update rule_graph) invalid_page_updates
  in
  Printf.printf "Number of valid page updates: %i\n"
    (List.length valid_page_updates);
  Printf.printf "Sum of middle elements: %i\n"
    (Day_05.sum_middle valid_page_updates);
  Printf.printf "Sum of middle elements after sorting: %i\n"
    (Day_05.sum_middle ordered_page_updates)
