let input = open_in "input.txt"

let map =
  In_channel.input_lines input |> List.map Bytes.of_string |> Array.of_list

let num_rows_minus1 = Array.length map - 1
let num_cols_minus1 = Bytes.length map.(0) - 1
let matrix (i, j) = Bytes.get map.(i) j

let out_of_bounds (i, j) =
  i < 0 || i > num_rows_minus1 || j < 0 || j > num_cols_minus1

let not_out_of_bounds coords = not (out_of_bounds coords)

let valid_antinodes (i1, j1) (i2, j2) =
  [ (i1 + i1 - i2, j1 + j1 - j2); (i2 + i2 - i1, j2 + j2 - j1) ]
  |> List.filter not_out_of_bounds

let scalev (i1, j1) k = (i1 * k, j1 * k)
let addv (i1, j1) (i2, j2) = (i1 + i2, j1 + j2)

let valid_antinodes_part2 v1 v2 =
  let vector = scalev v1 (-1) |> addv v2 in
  let m = Int.max num_rows_minus1 num_cols_minus1 in
  List.init ((2 * m) + 1) (fun i -> i - m)
  |> List.map (fun k -> scalev vector k |> addv v1)
  |> List.filter not_out_of_bounds

let antennae_coords =
  let arr = Array.make 256 [] in
  for i = 0 to num_rows_minus1 do
    for j = 0 to num_cols_minus1 do
      let pos = (i, j) in
      if not (matrix pos = '.') then
        let index = Char.code @@ matrix pos in
        arr.(index) <- pos :: arr.(index)
    done
  done;
  arr

module IntPair = struct
  type t = int * int

  let compare = compare
end

module IntPairSet = Set.Make (IntPair)

let add_antinodes coord_set is_part2 =
  let rec helper antinodes = function
    | [] -> antinodes
    | x :: xs ->
        xs
        |> helper
           @@ List.fold_left
                (fun acc y ->
                  IntPairSet.add_seq
                    ((if is_part2 then valid_antinodes_part2
                      else valid_antinodes)
                       x y
                    |> List.to_seq)
                    acc)
                antinodes xs
  in
  helper coord_set

let answer is_part2 =
  let antinode_coords =
    let coord_set = IntPairSet.empty in
    Array.fold_left
      (fun acc l -> add_antinodes acc is_part2 l)
      coord_set antennae_coords
  in
  antinode_coords |> IntPairSet.cardinal

let answer1, answer2 = (answer false, answer true)
let () = Printf.printf "Part 1: %d\nPart 2: %d\n" answer1 answer2
