let rec find_equation_combinations = function
  | [] -> []
  | x :: [] -> [ x ]
  | x :: xs ->
      List.map (fun y -> x + y) (find_equation_combinations xs)
      @ List.map (fun y -> x * y) (find_equation_combinations xs)

let rec find_equation_combinations_including_concat = function
  | [] -> []
  | x :: [] -> [ x ]
  | x :: xs ->
      List.map (fun y -> x + y) (find_equation_combinations_including_concat xs)
      @ List.map
          (fun y -> x * y)
          (find_equation_combinations_including_concat xs)
      @ List.map
          (fun y -> int_of_string (string_of_int y ^ string_of_int x))
          (find_equation_combinations_including_concat xs)

let debug inputs =
  Printf.printf "Equation combinations: %s"
    (String.concat "\n"
       (List.map
          (fun (_, equation_constants) ->
            String.concat " "
              (List.map string_of_int
                 (find_equation_combinations_including_concat
                    (List.rev equation_constants))))
          inputs))

let is_valid_equation_combination result equation_constants =
  List.mem result
    (find_equation_combinations_including_concat (List.rev equation_constants))

let sum_valid_equation_combinations (inputs : (int * int list) list) =
  List.fold_left
    (fun acc (result, _) -> acc + result)
    0
    (List.filter
       (fun (result, equation_constants) ->
         is_valid_equation_combination result equation_constants)
       inputs)
