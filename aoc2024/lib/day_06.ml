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
