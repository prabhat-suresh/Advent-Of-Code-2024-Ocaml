let input = open_in "input.txt"

let rec diffs = function
  | x1 :: (x2 :: _ as xs) -> (x2 - x1) :: diffs xs
  | _ -> []

(* Part 1 *)
(* let check_part1 ls = *)
(*   let diff_ls = diffs ls in *)
(*   List.for_all (fun x -> x <= 3 && x > 0) diff_ls *)
(*   || List.for_all (fun x -> x >= -3 && x < 0) diff_ls *)

(* Part 2 *)
(* O(n) solution *)
let predicate check_increasing x =
  if check_increasing then x <= 3 && x > 0 else x >= -3 && x < 0

let rec check diff_ls allow_one_removal check_increasing =
  match diff_ls with
  | x :: xs ->
      if predicate check_increasing x then
        check xs allow_one_removal check_increasing
      else if allow_one_removal then
        match xs with
        | y :: ys -> check ((x + y) :: ys) false check_increasing
        | [] -> true
      else false
  | [] -> true

let check_part2 ls =
  let diff_ls = diffs ls in
  check diff_ls true true || check diff_ls true false
  || check (List.tl diff_ls) false true
  || check (List.tl diff_ls) false false

let rec input_helper count =
  try
    let ints = String.split_on_char ' ' (input_line input) in
    let ls = List.map int_of_string ints in
    input_helper (if check_part2 ls then count + 1 else count)
  with End_of_file -> count

let () = Printf.printf "%d\n" (input_helper 0)
