let input = open_in "input.txt"

let garden =
  In_channel.input_lines input |> List.map Bytes.of_string |> Array.of_list

let length = Array.length garden
let is_not_out_of_bounds (i, j) = i >= 0 && i < length && j >= 0 && j < length
let matrix (i, j) = Bytes.get garden.(i) j
let visit (i, j) = Bytes.set garden.(i) j '.'
let is_visited (i, j) = Bytes.get garden.(i) j = '.'

let neighbours (i, j) =
  [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]
  |> List.filter (fun cell ->
         is_not_out_of_bounds cell && matrix cell = matrix (i, j))

let add_pair (a, b) (c, d) = (a + c, b + d)

let cost coord =
  let rec dfs_visit coord =
    let neighbors = neighbours coord in
    visit coord;
    neighbors
    |> List.filter_map (fun coord ->
           if is_visited coord then None else Some (dfs_visit coord))
    |> List.fold_left add_pair (0, 0)
    |> add_pair (1, List.length neighbors * 2)
  in
  let area, perimeter_to_be_avoided = dfs_visit coord in
  area * ((4 * area) - perimeter_to_be_avoided)

let answer1 =
  let fence_cost = ref 0 in
  Array.iteri
    (fun i byts ->
      Bytes.iteri
        (fun j c ->
          if not (c = '.') then fence_cost := !fence_cost + cost (i, j))
        byts)
    garden;
  !fence_cost

let () = Printf.printf "Part 1: %d\n" answer1
