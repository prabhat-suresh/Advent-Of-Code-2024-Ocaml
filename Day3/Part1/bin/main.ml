let input = open_in "input.txt"

let rec sum_of_multiplications str sum_acc = match str with
  | 'm'::'u'::'l'::'('::rest ->
  | _ :: rest -> sum_of_multiplications rest sum_acc
  | _ -> sum_acc

let rec input_helper sum =
  try
    input_helper (sum_of_multiplications (input_line input) + sum)
  with End_of_file -> sum

let () = Printf.printf "%d\n" (input_helper 0)
