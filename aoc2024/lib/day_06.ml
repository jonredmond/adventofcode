type direction = Up | Down | Left | Right
type position = { x : int; y : int }
type grid = char array array

let get_guard_starting_position grid =
  let rec get_guard_starting_position' x y =
    match grid.(y).(x) with
    | '^' -> ({ x; y }, Up)
    | 'v' -> ({ x; y }, Down)
    | '<' -> ({ x; y }, Left)
    | '>' -> ({ x; y }, Right)
    | _ ->
        if x < Array.length grid.(y) - 1 then
          get_guard_starting_position' (x + 1) y
        else get_guard_starting_position' 0 (y + 1)
  in
  get_guard_starting_position' 0 0

let get_next_position { x; y } = function
  | Up -> { x; y = y - 1 }
  | Down -> { x; y = y + 1 }
  | Left -> { x = x - 1; y }
  | Right -> { x = x + 1; y }

let turn_right = function
  | Up -> Right
  | Down -> Left
  | Left -> Up
  | Right -> Down

let rec check_next_position grid position direction =
  let { x; y } = get_next_position position direction in
  if y < 0 || y >= Array.length grid then None
  else if x < 0 || x >= Array.length grid.(y) then None
  else
    match grid.(y).(x) with
    | '#' -> check_next_position grid position (turn_right direction)
    | _ -> Some ({ x; y }, direction)

let rec get_path grid position direction =
  match check_next_position grid position direction with
  | None -> []
  | Some (next_position, next_direction) ->
      next_position :: get_path grid next_position next_direction

let get_number_unique_positions grid =
  let starting_position, starting_direction =
    get_guard_starting_position grid
  in
  let path =
    starting_position :: get_path grid starting_position starting_direction
  in
  List.length (List.sort_uniq compare path)

let find_loop path last_position =
  let rec find_loop' path' =
    match path' with
    | [] -> []
    | position :: rest ->
        if position = last_position then rest else position :: find_loop' rest
  in
  find_loop' path

let does_path_loop grid =
  let starting_position, starting_direction =
    get_guard_starting_position grid
  in
  let rec does_path_loop' visited_nodes path position direction =
    match check_next_position grid position direction with
    | None -> false
    | Some (next_position, next_direction) ->
        if List.mem position visited_nodes && next_direction <> direction then
          true
        else
          let visited_nodes' =
            if next_direction = direction then visited_nodes
            else position :: visited_nodes
          in
          does_path_loop' visited_nodes' (next_position :: path) next_position
            next_direction
  in
  does_path_loop' [] [] starting_position starting_direction

let count_number_of_looping_paths grid =
  let rec count_number_of_looping_paths' x y count =
    if y >= Array.length grid then count
    else if x >= Array.length grid.(y) then
      count_number_of_looping_paths' 0 (y + 1) count
    else if List.mem grid.(y).(x) [ '#'; '^'; '>'; 'v'; '<' ] then
      count_number_of_looping_paths' (x + 1) y count
    else
      let grid' = Array.copy grid in
      let original_cell = grid'.(y).(x) in
      grid'.(y).(x) <- '#';
      let count' = if does_path_loop grid' then count + 1 else count in
      grid'.(y).(x) <- original_cell;
      count_number_of_looping_paths' (x + 1) y count'
  in
  count_number_of_looping_paths' 0 0 0

let debug x y grid =
  print_char grid.(x).(y);
  flush stdout;
  let grid' = Array.copy grid in
  grid'.(y).(x) <- '#';
  does_path_loop grid'
