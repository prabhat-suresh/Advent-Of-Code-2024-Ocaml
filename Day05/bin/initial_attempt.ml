(* Input *)
let input = open_in "input.txt"
let graph = Array.make_matrix 101 101 false

let add_edge = function
  | [ u; v ] -> graph.(u).(v) <- true
  | _ -> failwith "Invalid input"

let () =
  let rec input_edges () =
    try
      input_line input |> String.split_on_char '|' |> List.map int_of_string
      |> add_edge |> input_edges
    with _ -> ()
  in
  input_edges ()

let list_of_updates =
  let rec input_helper acc =
    try
      input_line input |> String.split_on_char ',' |> List.map int_of_string
      |> fun l -> input_helper (l :: acc)
    with _ -> acc
  in
  input_helper []

(* Part 1 *)
let dfs g =
  let vertex_values = Array.make 101 0 in
  let visited = Array.make 101 false in
  let is_visited u = visited.(u) in
  let children u = g.(u) in

  let rec dfs_visit u =
    visited.(u) <- true;
    vertex_values.(u) <-
      1
      + (children u
        |> List.map vertex_value_after_dfs_visit
        |> List.fold_left Int.max (-1))
  and vertex_value_after_dfs_visit u =
    if is_visited u then vertex_values.(u)
    else (
      dfs_visit u;
      vertex_values.(u))
  in

  let () =
    for i = 1 to 100 do
      if not (is_visited i) then dfs_visit i
    done
  in
  vertex_values

let middle_of_list l = List.nth l (List.length l / 2)

let rec check_list_is_decreasing = function
  | x :: (y :: _ as xs) -> x > y && check_list_is_decreasing xs
  | _ -> true

let subgraph l =
  let subg = Array.make 101 [] in
  let rec helper = function
    | [] -> subg
    | u :: vs ->
        List.iter
          (fun v ->
            if graph.(u).(v) then subg.(u) <- v :: subg.(u);
            if graph.(v).(u) then subg.(v) <- u :: subg.(v))
          vs;
        helper vs
  in
  helper l

type 'a valid = Valid | Invalid of 'a

let check_valid_update l =
  let vertex_values = dfs (subgraph l) in
  l |> List.map (fun i -> vertex_values.(i)) |> check_list_is_decreasing
  |> function
  | true -> Valid
  | false -> Invalid vertex_values

let validate_update vertex_values =
  List.sort (fun a b -> vertex_values.(b) - vertex_values.(a))

let answer1, answer2 =
  let l1, l2 =
    List.partition_map
      (fun l ->
        match check_valid_update l with
        | Valid -> Left (middle_of_list l)
        | Invalid vertex_values ->
            Right (middle_of_list @@ validate_update vertex_values l))
      list_of_updates
  in
  (List.fold_left ( + ) 0 l1, List.fold_left ( + ) 0 l2)

let () = Printf.printf "Part 1: %d\n Part 2: %d\n" answer1 answer2
