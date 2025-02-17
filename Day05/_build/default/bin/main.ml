(* Input *)
let input = open_in "input.txt"
let adj_matrix = Array.make_matrix 101 101 false

let add_edge = function
  | [ u; v ] -> adj_matrix.(u).(v) <- true
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

let middle_of_list l = List.nth l (List.length l / 2)

(* Part 1 *)
let validate_update =
  List.sort (fun u v ->
      if adj_matrix.(u).(v) then -1 else if adj_matrix.(v).(u) then 1 else 0)

let answer_part1, answer_part2 =
  let ls1, ls2 =
    list_of_updates
    |> List.partition_map (fun l ->
           let val_l = validate_update l in
           if l = val_l then Left (middle_of_list l)
           else Right (middle_of_list val_l))
  in
  (List.fold_left ( + ) 0 ls1, List.fold_left ( + ) 0 ls2)

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" answer_part1 answer_part2

(* let rec check_valid_update = function *)
(*   | [] -> true *)
(*   | u :: vs -> *)
(*       List.for_all (fun v -> not adj_matrix.(v).(u)) vs && check_valid_update vs *)
(* let check_valid_update_using_sort l = l = validate_update l *)
(* open Core_bench *)
(* [Bench.Test.create ~name:"check_valid_update" (fun () -> check_valid_update @@ List.hd list_of_updates ) Bench.Test.create ~name:"check_valid_update_using_sort" (fun () -> check_valid_update_using_sort @@ List.hd list_of_updates ) ] |> Bench.bench *)
