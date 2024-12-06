let read_lines filename =
  let file = open_in filename in
  let rec imp acc =
    try imp (input_line file :: acc)
    with End_of_file ->
      close_in file;
      List.rev acc
  in
  imp []

let mul_groups input =
  let regex =
    Re.compile
      Re.(
        seq
          [
            str "mul("; group (rep1 digit); str ","; group (rep1 digit); str ")";
          ])
  in
  let matches = Re.all regex input in
  List.fold_left
    (fun acc group ->
      acc
      + int_of_string (Re.Group.get group 1)
        * int_of_string (Re.Group.get group 2))
    0 matches

let mul_groups_ignore_disabled input =
  let regex =
    Re.compile
      Re.(
        alt
          [
            seq
              [
                str "mul(";
                group (rep1 digit);
                str ",";
                group (rep1 digit);
                str ")";
              ];
            str "don't()";
            str "do()";
          ])
  in
  let matches = Re.all regex input in
  let rec mul_groups_ignore_disabled' acc is_enabled = function
    | [] -> acc
    | group :: xs -> (
        match Re.Group.get group 0 with
        | "don't()" -> mul_groups_ignore_disabled' acc false xs
        | "do()" -> mul_groups_ignore_disabled' acc true xs
        | _ ->
            if is_enabled then
              mul_groups_ignore_disabled'
                (acc
                + int_of_string (Re.Group.get group 1)
                  * int_of_string (Re.Group.get group 2))
                true xs
            else mul_groups_ignore_disabled' acc false xs)
  in
  mul_groups_ignore_disabled' 0 true matches

let () =
  let input = read_lines "input.dat" in
  let combined_input = String.concat "" input in
  Printf.printf "Raw result: %d\n" (mul_groups combined_input);
  Printf.printf "Removing disabled commands: %d\n"
    (mul_groups_ignore_disabled combined_input)