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
