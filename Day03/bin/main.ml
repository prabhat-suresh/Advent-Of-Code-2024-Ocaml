let input = open_in "input.txt"
let mul_regex = Str.regexp "mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)"
let num_regex = Str.regexp "[0-9][0-9]?[0-9]?"

(* Part 1 *)
(* let rec search_for_muls str search_index curr_sum = *)
(*   try *)
(*     let _ = Str.search_forward mul_regex str search_index in *)
(*     let matched_str = Str.matched_string str in *)
(*     let end_pos = Str.match_end () in *)
(*     let _ = Str.search_forward num_regex matched_str 1 in *)
(*     let left_num = int_of_string (Str.matched_string matched_str) in *)
(*     let _ = Str.search_forward num_regex matched_str (Str.match_end ()) in *)
(*     let right_num = int_of_string (Str.matched_string matched_str) in *)
(*     search_for_muls str end_pos (curr_sum + (left_num * right_num)) *)
(*   with Not_found -> curr_sum *)
(* let rec input_helper curr_sum = *)
(*   try *)
(*     let input_string = input_line input in *)
(*     input_helper (search_for_muls input_string 0 curr_sum) *)
(*   with End_of_file -> curr_sum *)
(* let () = Printf.printf "%d\n" (input_helper 0) *)

(* Part 2 *)
let do_regex = Str.regexp "do()"
let dont_regex = Str.regexp "don't()"

type token = Do | Dont | Mul of int * int

let list_of_tokens s =
  let rec helper acc i =
    if i + 2 >= String.length s then List.rev acc
    else
      match (s.[i], s.[i + 1], s.[i + 2]) with
      | 'd', 'o', '(' -> (
          try
            let index = Str.search_forward do_regex s i in
            if index = i then helper (Do :: acc) (i + 4) else helper acc (i + 1)
          with Not_found -> helper acc (i + 1))
      | 'd', 'o', 'n' -> (
          try
            let index = Str.search_forward dont_regex s i in
            if index = i then helper (Dont :: acc) (i + 6)
            else helper acc (i + 1)
          with Not_found -> helper acc (i + 1))
      | 'm', 'u', 'l' -> (
          try
            let index = Str.search_forward mul_regex s i in
            if index = i then
              let _ = Str.search_forward num_regex s (i + 4) in
              let num1 = int_of_string (Str.matched_string s) in
              let end_pos = Str.match_end () in
              let _ = Str.search_forward num_regex s end_pos in
              let num2 = int_of_string (Str.matched_string s) in
              let end_pos = Str.match_end () in
              helper (Mul (num1, num2) :: acc) end_pos
            else helper acc (i + 1)
          with Not_found -> helper acc (i + 1))
      | _ -> helper acc (i + 1)
  in
  helper [] 0

let rec input_handler curr_sum enable =
  try
    let new_sum, new_enable =
      input_line input |> list_of_tokens
      |> List.fold_left
           (fun (sum, enable) token ->
             match token with
             | Do -> (sum, true)
             | Dont -> (sum, false)
             | Mul (a, b) -> ((sum + if enable then a * b else 0), enable))
           (curr_sum, enable)
    in
    input_handler new_sum new_enable
  with End_of_file -> curr_sum

let () = Printf.printf "%d\n" (input_handler 0 true)
