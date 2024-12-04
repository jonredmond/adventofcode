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

let count_safe_lines input = 
  let rec count_safe_lines' acc = function
    | [] -> acc
    | x::xs -> count_safe_lines' (if check_safety x then acc + 1 else acc) xs
in count_safe_lines' 0 input

let determine_trend = function
  | x1::x2::x3::x4::_ -> if (x1 > x2) && (x2 > x3) && (x3 > x4) then Decreasing
    else if x1 < x2 && x2 < x3 && x3 < x4 then Increasing
    else if (x1 > x2 && x2 > x3) || (x1 > x2 && x3 > x4) || (x2 > x3 && x3 > x4) then Decreasing
    else if (x1 < x2 && x2 < x3) || (x1 < x2 && x3 < x4) || (x2 < x3 && x3 < x4) then Increasing
    else None
  | x1::x2::x3::_ -> if x1 > x2 && x2 > x3 then Decreasing
    else if x1 < x2 && x2 < x3 then Increasing
    else None
  | x1::x2::_ -> if x1 > x2 then Decreasing
    else if x1 < x2 then Increasing
    else None
  | _ -> None


let check_safety_problem_dampener line =
  let rec check_safety_problem_dampener' increasing_or_decreasing num_unsafe_levels = function
    | [] -> num_unsafe_levels <= 1
    | _::[] -> num_unsafe_levels <= 1
    | x1::x2::xs ->
      if num_unsafe_levels > 1 then false
      else 
        begin
          if abs(x1-x2) > 3 then check_safety_problem_dampener' increasing_or_decreasing (num_unsafe_levels + 1) (x2::xs)
          else
          match increasing_or_decreasing with
          | None -> let trend = determine_trend (x1::x2::xs) in 
                    if trend = None then false          
                    else check_safety_problem_dampener' trend num_unsafe_levels (x1::x2::xs)
          | Increasing -> if x1 < x2 then check_safety_problem_dampener' Increasing num_unsafe_levels (x2::xs)
            else check_safety_problem_dampener' Increasing (num_unsafe_levels + 1) (x1::xs)
          | Decreasing -> if x1 > x2 then check_safety_problem_dampener' Decreasing num_unsafe_levels (x2::xs)
            else check_safety_problem_dampener' Decreasing (num_unsafe_levels + 1) (x1::xs)
        end
    in check_safety_problem_dampener' None 0 line

let count_safe_lines_problem_dampener input = 
  let rec count_safe_lines_problem_dampener' acc = function
    | [] -> acc
    | x::xs -> count_safe_lines_problem_dampener' (if check_safety_problem_dampener x then acc + 1 else acc) xs
in count_safe_lines_problem_dampener' 0 input

let rec remove_at n lst =
  match lst with
  | [] -> []
  | _::xs when n = 0 -> xs
  | x::xs -> x :: remove_at (n-1) xs

let generate_sublists lst =
  let rec aux i acc =
    if i >= List.length lst then List.rev acc
    else aux (i + 1) ((remove_at i lst) :: acc)
  in
  aux 0 []

let brute_force_check_safety_problem_dampener line =
  let sublists = generate_sublists line in
  let rec brute_force_check_safety_problem_dampener' = function
    | [] -> false
    | x::xs -> if check_safety x then true else brute_force_check_safety_problem_dampener' xs
  in
  brute_force_check_safety_problem_dampener' sublists

let count_brute_force_safe_lines_problem_dampener input = 
  let rec count_brute_force_safe_lines_problem_dampener' acc = function
    | [] -> acc
    | x::xs -> count_brute_force_safe_lines_problem_dampener' (if brute_force_check_safety_problem_dampener x then acc + 1 else acc) xs
in count_brute_force_safe_lines_problem_dampener' 0 input

let compare_brute_force_to_problem_dampener input = 
  let rec compare_brute_force_to_problem_dampener' acc = function
    | [] -> acc
    | x::xs -> compare_brute_force_to_problem_dampener' (if check_safety_problem_dampener x = brute_force_check_safety_problem_dampener x then acc else x::acc) xs
in compare_brute_force_to_problem_dampener' [] input


let () = 
  let input = read_lines "input.dat" in
  let parsed_input = parse_input input in
  Printf.printf "Number of safe lines: %i\n" (count_safe_lines parsed_input);
  Printf.printf "Number of safe lines with problem dampener: %i\n" (count_safe_lines_problem_dampener parsed_input);
  Printf.printf "Number of safe lines with brute force: %i\n" (count_brute_force_safe_lines_problem_dampener parsed_input);
  Printf.printf "Different lines: %s\n" (String.concat "\n" (List.map (fun x -> String.concat ", " (List.map string_of_int x)) (compare_brute_force_to_problem_dampener parsed_input)))