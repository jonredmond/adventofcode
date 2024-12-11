module CharMap = Map.Make (Char)

type position = { x : int; y : int }

let pair_up_elements_in_list list =
  let rec pair_up_elements_in_list' acc = function
    | [] -> acc
    | x :: xs ->
        List.map (fun y -> (x, y)) xs @ pair_up_elements_in_list' acc xs
  in
  pair_up_elements_in_list' [] list

let find_antinodes_of_two_antennas ({ x; y }, { x = x'; y = y' }) max_width
    max_height =
  let rec imp acc n =
    let antinode_position =
      { x = x + (n * (x' - x)); y = y + (n * (y' - y)) }
    in
    if antinode_position.x < 0 || antinode_position.x >= max_width then acc
    else if antinode_position.y < 0 || antinode_position.y >= max_height then
      acc
    else imp (antinode_position :: acc) (n + 1)
  in
  let rec imp2 acc n =
    let antinode_position =
      { x = x' + (n * (x - x')); y = y' + (n * (y - y')) }
    in
    if antinode_position.x < 0 || antinode_position.x >= max_width then acc
    else if antinode_position.y < 0 || antinode_position.y >= max_height then
      acc
    else imp2 (antinode_position :: acc) (n + 1)
  in
  imp [] 1 @ imp2 [] 1

let find_all_antinodes_in_frequency position_list max_width max_height =
  let rec find_all_antinodes_in_frequency' acc = function
    | [] -> acc
    | x :: xs ->
        find_all_antinodes_in_frequency'
          (acc @ find_antinodes_of_two_antennas x max_width max_height)
          xs
  in
  find_all_antinodes_in_frequency' [] (pair_up_elements_in_list position_list)

let find_antennas_in_grid grid =
  let rec find_antennas_in_grid' x y antenna_map =
    if y >= Array.length grid then antenna_map
    else if x >= Array.length grid.(y) then
      find_antennas_in_grid' 0 (y + 1) antenna_map
    else
      match grid.(y).(x) with
      | '.' -> find_antennas_in_grid' (x + 1) y antenna_map
      | frequency ->
          let antenna_map' =
            CharMap.update frequency
              (function
                | None -> Some [ { x; y } ]
                | Some positions -> Some ({ x; y } :: positions))
              antenna_map
          in
          find_antennas_in_grid' (x + 1) y antenna_map'
  in
  find_antennas_in_grid' 0 0 CharMap.empty

let print_antenna_map antenna_map =
  CharMap.iter
    (fun k v ->
      Printf.printf "Frequency: %c\n" k;
      List.iter (fun { x; y } -> Printf.printf "Position: %d %d\n" x y) v)
    antenna_map

let print_antinode_positions positions =
  print_endline "Antinode positions:";
  List.iter (fun { x; y } -> Printf.printf "Position: %d %d\n" x y) positions

let print_grid_with_antinodes grid antinode_positions =
  let grid_height, grid_width = (Array.length grid, Array.length grid.(0)) in
  let grid_with_antinodes =
    Array.init grid_height (fun y ->
        Array.init grid_width (fun x ->
            if List.mem { x; y } antinode_positions && grid.(y).(x) = '.' then
              '#'
            else grid.(y).(x)))
  in
  Array.iter
    (fun row -> Printf.printf "%s\n" (String.of_seq (Array.to_seq row)))
    grid_with_antinodes

let count_all_distinct_antinodes grid =
  let antenna_map = find_antennas_in_grid grid in
  let grid_height, grid_width = (Array.length grid, Array.length grid.(0)) in
  let all_antinode_positions =
    CharMap.fold
      (fun _ v acc ->
        acc @ find_all_antinodes_in_frequency v grid_width grid_height)
      antenna_map []
  in
  let filtered_positions =
    List.filter
      (fun { x; y } -> x >= 0 && x < grid_width && y >= 0 && y < grid_height)
      all_antinode_positions
  in
  let unique_positions = List.sort_uniq compare filtered_positions in
  print_grid_with_antinodes grid unique_positions;
  List.length unique_positions
