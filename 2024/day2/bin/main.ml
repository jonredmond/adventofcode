type increasing_or_decreasing = Increasing | Decreasing | None

let read_lines filename =
  let file = open_in filename in
  let rec imp acc = 
    try imp (input_line file :: acc)
    with End_of_file -> close_in file; List.rev acc
  in
  imp []
;;

let parse_input lines =
  lines |> List.map (fun line -> 
    String.split_on_char ' ' line 
    |> List.filter (fun s -> s <> "") 
    |> List.map int_of_string
  )
;;

let check_safety line = 
    let rec check_safety' increasing_or_decreasing = function
      | [] -> true
      | _::[] -> true
      | x::y::xs ->
        if abs (x-y) > 3 then false
        else
        match increasing_or_decreasing with
        | None -> if x < y then check_safety' Increasing (y::xs) else if x > y then check_safety' Decreasing (y::xs) else false
        | Increasing -> if x < y then check_safety' Increasing (y::xs) else false
        | Decreasing -> if x > y then check_safety' Decreasing (y::xs) else false
    in
    check_safety' None line

let rec count_safe_lines acc = function
  | [] -> acc
  | x::xs -> count_safe_lines (if check_safety x then acc + 1 else acc) xs

let () = 
  let input = read_lines "input.dat" in
  let parsed_input = parse_input input in
  Printf.printf "Number of safe lines: %i\n" (count_safe_lines 0 parsed_input)