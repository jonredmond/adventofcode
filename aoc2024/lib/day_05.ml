type rule_graph = { nodes : int list; edges : (int * int) list }

let sum_middle page_updates =
  List.fold_left
    (fun acc (update : int list) ->
      let middle = List.length update / 2 in
      acc + List.nth update middle)
    0 page_updates

let generate_rule_graph rule_list =
  let rec generate_rule_graph' acc = function
    | [] -> acc
    | (before, after) :: rules ->
        let acc' =
          {
            nodes = [ before; after ] @ acc.nodes;
            edges = (before, after) :: acc.edges;
          }
        in
        generate_rule_graph' acc' rules
  in
  generate_rule_graph' { nodes = []; edges = [] } rule_list

let check_reachable_nodes_not_visited visited_nodes rule_graph node =
  List.filter (fun (x, _) -> x = node) rule_graph.edges
  |> List.fold_left
       (fun acc (_, y) ->
         match List.mem y visited_nodes with true -> false | false -> acc)
       true

let validate_page_update (rule_graph : rule_graph) (page_update : int list) =
  let rec validate_page_update' visited_nodes = function
    | [] -> true
    | x :: xs -> (
        let visited_nodes' = x :: visited_nodes in
        match List.mem x rule_graph.nodes with
        | false -> validate_page_update' visited_nodes' xs
        | true -> (
            match
              check_reachable_nodes_not_visited visited_nodes' rule_graph x
            with
            | false -> false
            | true -> validate_page_update' visited_nodes' xs))
  in
  validate_page_update' [] page_update

let remove_edges edges node =
  let rec remove_edges' acc_edges nodes_with_no_entry = function
    | [] -> (acc_edges, nodes_with_no_entry)
    | (x, y) :: xs when x = node ->
        if List.exists (fun (_, z) -> z = y) (acc_edges @ xs) then
          remove_edges' acc_edges nodes_with_no_entry xs
        else remove_edges' acc_edges (y :: nodes_with_no_entry) xs
    | (x, y) :: xs -> remove_edges' ((x, y) :: acc_edges) nodes_with_no_entry xs
  in
  remove_edges' [] [] edges

let order_page_update (rule_graph : rule_graph) (page_update : int list) =
  let edges =
    List.filter
      (fun (x, y) -> List.mem x page_update && List.mem y page_update)
      rule_graph.edges
  in
  let nodes_with_no_entry =
    List.filter
      (fun node -> not (List.exists (fun (_, y) -> y = node) edges))
      page_update
  in
  let rec order_page_update' ordered_list edges = function
    | [] -> ordered_list
    | x :: xs ->
        let ordered_list' = x :: ordered_list in
        let edges', nodes_with_no_entry' = remove_edges edges x in
        order_page_update' ordered_list' edges' (nodes_with_no_entry' @ xs)
  in
  order_page_update' [] edges nodes_with_no_entry
