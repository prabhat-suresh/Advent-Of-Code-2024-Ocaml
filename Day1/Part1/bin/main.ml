(* take input from a file input.txt *)
let input = Scanf.Scanning.from_file "input.txt"

(* read the first line of input *)
let rec input_helper (xs, ys) =
  try Scanf.bscanf input "%d %d\n" (fun x y -> input_helper (x :: xs, y :: ys))
  with End_of_file -> (xs, ys)

let list1, list2 = input_helper ([], [])

let sorted_list1, sorted_list2 =
  (List.sort compare list1, List.sort compare list2)

let rec calc_dist (ls1, ls2) =
  match (ls1, ls2) with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let dist = abs (x - y) in
      dist + calc_dist (xs, ys)
  | _ -> failwith "lists are not the same length"

let () = Printf.printf "%d\n" (calc_dist (sorted_list1, sorted_list2))
