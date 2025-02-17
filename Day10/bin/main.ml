let input = open_in "input.txt"

let map =
  In_channel.input_lines input
  |> List.map (fun s ->
         Array.init (String.length s) (fun i -> Char.code s.[i] - Char.code '0'))
  |> Array.of_list

let len = Array.length map
let node (i, j) = map.(i).(j)
let not_out_of_bounds (i, j) = i >= 0 && i < len && j >= 0 && j < len

let neighbours (i, j) =
  List.filter
    (fun coords -> not_out_of_bounds coords && node coords - node (i, j) = 1)
    [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]

let score_and_rating trailhead =
  let rec dfs_visit coords =
    if node coords = 9 then [ coords ]
    else neighbours coords |> List.map dfs_visit |> List.flatten
  in
  let peak_coords = dfs_visit trailhead in
  (List.length @@ List.sort_uniq compare peak_coords, List.length peak_coords)

let add_pair (a, b) (c, d) = (a + c, b + d)

let answer1, answer2 =
  let sum = ref (0, 0) in
  map
  |> Array.iteri (fun i row ->
         row
         |> Array.iteri (fun j cell ->
                if cell = 0 then sum := add_pair !sum @@ score_and_rating (i, j)));
  !sum

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" answer1 answer2
