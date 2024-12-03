module FrequencyMap = Map.Make(Int)

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
    let split_line = String.split_on_char ' ' line 
    |> List.filter (fun s -> s <> "") 
    |> List.map int_of_string in
      List.nth split_line 0, List.nth split_line 1
  ) |> List.split
;;

let find_diff (list1, list2) = 
  let list1_sorted = List.sort compare list1 in
  let list2_sorted = List.sort compare list2 in
  let rec sum_diff acc = function
    | [], [] -> acc
    | x::xs, y::ys -> sum_diff (acc + abs (x - y)) (xs, ys)
    | _ -> failwith "Lists are not the same length"
  in
  sum_diff 0 (list1_sorted, list2_sorted)

let similarity_score (list1, list2) =
  let frequency_map = List.fold_left (fun acc x -> 
    let count = try FrequencyMap.find x acc with Not_found -> 0 in
    FrequencyMap.add x (count + 1) acc
  ) FrequencyMap.empty list2 in
  let rec calc_sim_score acc = function
    | [] -> acc
    | x::xs -> calc_sim_score (acc + (x * (try FrequencyMap.find x frequency_map with Not_found -> 0))) xs
  in
  calc_sim_score 0 list1
  

let () =
  let input = read_lines "input.dat" in
    input |> parse_input |> find_diff |> Printf.printf "Difference: %i\n";
    input |> parse_input |> similarity_score |> Printf.printf "Similarity score: %i\n"