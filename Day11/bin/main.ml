let input = open_in "input.txt"

let stones =
  input_line input |> String.split_on_char ' ' |> List.map int_of_string

let num_digits stone = stone |> string_of_int |> String.length

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let mid = pow a (n / 2) in
      mid * mid * if n mod 2 = 0 then 1 else a

let memo = Hashtbl.create 100000

let rec transform ntimes stone =
  if ntimes = 0 then 1
  else
    match Hashtbl.find_opt memo (stone, ntimes) with
    | Some num_stones -> num_stones
    | None ->
        let num_stones =
          let len = num_digits stone in
          if stone = 0 then transform (ntimes - 1) 1
          else if len mod 2 = 1 then transform (ntimes - 1) (stone * 2024)
          else
            let divisor = pow 10 (len / 2) in
            transform (ntimes - 1) (stone / divisor)
            + transform (ntimes - 1) (stone mod divisor)
        in
        Hashtbl.add memo (stone, ntimes) num_stones;
        num_stones

let answer = stones |> List.map (transform 25) |> List.fold_left ( + ) 0
let () = Printf.printf "Part 1: %d\n" answer
