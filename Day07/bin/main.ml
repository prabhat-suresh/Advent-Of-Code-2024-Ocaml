let input = open_in "input.txt"

let inp_lists =
  In_channel.input_lines input
  |> List.map (fun s ->
         s |> String.split_on_char ' ' |> function
         | test_val :: test_list ->
             let test_val' =
               String.sub test_val 0 (String.length test_val - 1)
               |> int_of_string
             in
             test_val' :: List.map int_of_string test_list
         | _ -> failwith "Invalid input")

let list_of_possible_test_vals is_part2 = function
  | hd :: tl ->
      List.fold_left
        (fun acc x ->
          List.map (( + ) x) acc
          @ List.map (( * ) x) acc
          @
          if is_part2 then
            List.map
              (fun y -> string_of_int y ^ string_of_int x |> int_of_string)
              acc
          else [])
        [ hd ] tl
  | _ -> failwith "Invalid input"

let valid_test is_part2 = function
  | hd :: tl -> List.mem hd (list_of_possible_test_vals is_part2 tl)
  | [] -> failwith "Invalid input"

let answer1, answer2 =
  ( List.fold_left
      (fun sum test -> sum + if valid_test false test then List.hd test else 0)
      0 inp_lists,
    List.fold_left
      (fun sum test -> sum + if valid_test true test then List.hd test else 0)
      0 inp_lists )

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" answer1 answer2
