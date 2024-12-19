let input = open_in "input.txt"

type status = Decreasing | Increasing | Neutral

let rec check ls curr_status =
  match ls with
  | prev :: x :: xs -> (
      match curr_status with
      | Decreasing ->
          if x >= prev || prev - x > 3 then false
          else check (x :: xs) curr_status
      | Increasing ->
          if x <= prev || x - prev > 3 then false
          else check (x :: xs) curr_status
      | Neutral ->
          if x = prev || abs (x - prev) > 3 then false
          else check (x :: xs) (if x > prev then Increasing else Decreasing))
  | _ -> true

let rec check_with_removal ls prev_ls =
  match ls with
  | x :: xs ->
      check (List.rev prev_ls @ xs) Neutral
      || check_with_removal xs (x :: prev_ls)
  | [] -> check prev_ls Neutral

let rec input_helper count =
  try
    let ints = String.split_on_char ' ' (input_line input) in
    let ls = List.map int_of_string ints in
    input_helper (if check_with_removal ls [] then count + 1 else count)
  with End_of_file -> count

let () = Printf.printf "%d\n" (input_helper 0)