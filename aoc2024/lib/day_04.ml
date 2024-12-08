let xmas = "XMAS"

let paths (x, y) : (int * int) list list =
  [
    [ (x, y); (x + 1, y); (x + 2, y); (x + 3, y) ];
    [ (x, y); (x - 1, y); (x - 2, y); (x - 3, y) ];
    [ (x, y); (x + 1, y + 1); (x + 2, y + 2); (x + 3, y + 3) ];
    [ (x, y); (x - 1, y - 1); (x - 2, y - 2); (x - 3, y - 3) ];
    [ (x, y); (x + 1, y - 1); (x + 2, y - 2); (x + 3, y - 3) ];
    [ (x, y); (x - 1, y + 1); (x - 2, y + 2); (x - 3, y + 3) ];
    [ (x, y); (x, y + 1); (x, y + 2); (x, y + 3) ];
    [ (x, y); (x, y - 1); (x, y - 2); (x, y - 3) ];
  ]

let x_mas = "MASMS"

let x_mas_paths (x, y) =
  [
    [ (x, y); (x + 1, y + 1); (x + 2, y + 2); (x, y + 2); (x + 2, y) ];
    [ (x, y); (x + 1, y + 1); (x + 2, y + 2); (x + 2, y); (x, y + 2) ];
    [ (x, y); (x - 1, y - 1); (x - 2, y - 2); (x, y - 2); (x - 2, y) ];
    [ (x, y); (x - 1, y - 1); (x - 2, y - 2); (x - 2, y); (x, y - 2) ];
  ]

let validate_path path grid =
  let rec validate_path' = function
    | [] -> true
    | (x, y) :: xs -> (
        let index = 4 - (List.length xs + 1) in
        match List.nth (List.nth grid y) x with
        | c when c = String.get xmas index -> validate_path' xs
        | exception Failure _ -> false
        | exception Invalid_argument _ -> false
        | _ -> false)
  in
  validate_path' path

let count_valid_paths_starting_from (x, y) grid =
  let rec count_valid_paths_starting_from' acc = function
    | [] -> acc
    | path :: paths ->
        count_valid_paths_starting_from'
          (if validate_path path grid then acc + 1 else acc)
          paths
  in
  count_valid_paths_starting_from' 0 (paths (x, y))

let count_valid_paths grid =
  List.mapi
    (fun y row ->
      List.mapi (fun x _ -> count_valid_paths_starting_from (x, y) grid) row)
    grid
  |> List.flatten |> List.fold_left ( + ) 0

let validate_x_mas_path path grid =
  let rec validate_path' = function
    | [] -> true
    | (x, y) :: xs -> (
        let index = 5 - (List.length xs + 1) in
        match List.nth (List.nth grid y) x with
        | c when c = String.get x_mas index -> validate_path' xs
        | exception Failure _ -> false
        | exception Invalid_argument _ -> false
        | _ -> false)
  in
  validate_path' path

let count_valid_paths_x_mas_starting_from (x, y) grid =
  let rec count_valid_paths_x_mas_starting_from' acc = function
    | [] -> acc
    | path :: paths ->
        count_valid_paths_x_mas_starting_from'
          (if validate_x_mas_path path grid then acc + 1 else acc)
          paths
  in
  count_valid_paths_x_mas_starting_from' 0 (x_mas_paths (x, y))

let count_valid_x_mas_paths grid =
  List.mapi
    (fun y row ->
      List.mapi
        (fun x _ -> count_valid_paths_x_mas_starting_from (x, y) grid)
        row)
    grid
  |> List.flatten |> List.fold_left ( + ) 0
