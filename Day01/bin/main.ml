let input = open_in "input.txt"

let pair s =
  s |> String.split_on_char ' ' |> List.filter_map int_of_string_opt |> function
  | [ a; b ] -> (a, b)
  | _ -> failwith "Invalid input"

let list1, list2 = In_channel.input_lines input |> List.map pair |> List.split

(* Part 1 *)
let () =
  Printf.printf "%d\n"
    (List.combine (List.sort compare list1) (List.sort compare list2)
    |> List.map (fun (x, y) -> abs (x - y))
    |> List.fold_left ( + ) 0)

(* Part 2 *)
let hash_table = Hashtbl.create 1000

let () =
  List.iter
    (fun y ->
      match Hashtbl.find_opt hash_table y with
      | None -> Hashtbl.add hash_table y 1
      | Some count -> Hashtbl.replace hash_table y (count + 1))
    list2

let score =
  List.fold_left
    (fun acc p ->
      let freq =
        match Hashtbl.find_opt hash_table p with
        | Some count -> count
        | None -> 0
      in
      acc + (p * freq))
    0 list1

let () = Printf.printf "%d\n" score
