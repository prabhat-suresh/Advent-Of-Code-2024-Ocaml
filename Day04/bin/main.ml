let input = open_in "input.txt"

(* let explode s = let rec exp i dynarr = *)
(*     if i < 0 then dynarr *)
(*     else ( *)
(*       Dynarray.add_last dynarr s.[i]; *)
(*       exp (i - 1) dynarr) *)
(*   in *)
(*   exp (String.length s - 1) (Dynarray.create ()) *)
(**)
(* let rec inp_handler matrix = *)
(*   try *)
(*     let row = input_line input in *)
(*     Dynarray.add_last matrix (explode row); *)
(*     inp_handler matrix *)
(*   with End_of_file -> matrix *)

let matrix = In_channel.input_lines input |> Array.of_list

(* Part 1 *)
let cardinal_and_ordinal_directions =
  [ (1, 0); (0, 1); (-1, 0); (0, -1); (1, 1); (-1, 1); (1, -1); (-1, -1) ]

let check_xmas_in_direction (x, y) (i, j) =
  try
    [
      matrix.(x + i).[y + j];
      matrix.(x + (2 * i)).[y + (2 * j)];
      matrix.(x + (3 * i)).[y + (3 * j)];
    ]
    = [ 'M'; 'A'; 'S' ]
  with _ -> false

let () =
  let count = ref 0 in
  for i = 0 to Array.length matrix - 1 do
    for j = 0 to String.length matrix.(i) - 1 do
      if matrix.(i).[j] = 'X' then
        count :=
          !count
          + List.fold_left
              (fun acc direction ->
                if check_xmas_in_direction (i, j) direction then acc + 1
                else acc)
              0 cardinal_and_ordinal_directions
    done
  done;
  Printf.printf "Part 1: %d\n" !count

(* Part 2 *)
let check_mas_in_x_shape (i, j) =
  try
    matrix.(i).[j] = 'A'
    && (match (matrix.(i + 1).[j + 1], matrix.(i - 1).[j - 1]) with
       | 'M', 'S' -> true
       | 'S', 'M' -> true
       | _ -> false)
    &&
    match (matrix.(i + 1).[j - 1], matrix.(i - 1).[j + 1]) with
    | 'M', 'S' -> true
    | 'S', 'M' -> true
    | _ -> false
  with _ -> false

let () =
  let count = ref 0 in
  Array.iteri
    (fun i row ->
      String.iteri
        (fun j _ -> if check_mas_in_x_shape (i, j) then count := !count + 1)
        row)
    matrix;
  Printf.printf "Part 2: %d\n" !count
