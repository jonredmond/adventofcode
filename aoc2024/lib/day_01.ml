let sum_diff (list1, list2) =
  let list1_sorted = List.sort compare list1 in
  let list2_sorted = List.sort compare list2 in
  let rec sum_diff acc = function
    | [], [] -> acc
    | x :: xs, y :: ys -> sum_diff (acc + abs (x - y)) (xs, ys)
    | _ -> failwith "Lists are not the same length"
  in
  sum_diff 0 (list1_sorted, list2_sorted)

module FrequencyMap = Map.Make (Int)

let similarity_score (list1, list2) =
  let frequency_map =
    List.fold_left
      (fun acc x ->
        let count = try FrequencyMap.find x acc with Not_found -> 0 in
        FrequencyMap.add x (count + 1) acc)
      FrequencyMap.empty list2
  in
  let rec calc_sim_score acc = function
    | [] -> acc
    | x :: xs ->
        calc_sim_score
          (acc
          + (x * try FrequencyMap.find x frequency_map with Not_found -> 0))
          xs
  in
  calc_sim_score 0 list1
