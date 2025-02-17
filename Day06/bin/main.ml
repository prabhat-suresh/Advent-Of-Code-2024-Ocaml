let input = open_in "input.txt"

let map =
  In_channel.input_lines input |> List.map Bytes.of_string |> Array.of_list

let num_rows_minus1 = Array.length map - 1
let num_cols_minus1 = Bytes.length map.(0) - 1
let matrix (i, j) = Bytes.get map.(i) j
let put_matrix (i, j) c = Bytes.set map.(i) j c

let guard_starting_pos =
  let pos = ref (0, 0) in
  for i = 0 to num_rows_minus1 do
    for j = 0 to num_cols_minus1 do
      if matrix (i, j) = '^' then pos := (i, j)
    done
  done;
  !pos

type direction = Left | Right | Up | Down

let out_of_bounds (i, j) =
  i < 0 || i > num_rows_minus1 || j < 0 || j > num_cols_minus1

let right_of = function
  | Left -> Up
  | Up -> Right
  | Right -> Down
  | Down -> Left

let new_pos (i, j) = function
  | Left -> (i, j - 1)
  | Up -> (i - 1, j)
  | Right -> (i, j + 1)
  | Down -> (i + 1, j)

module IntPair = struct
  type t = int * int

  let compare = compare
end

module IntPairSet = Set.Make (IntPair)

let answer1 =
  let rec move direction pos visited_cells =
    let pos' = new_pos pos direction in
    if out_of_bounds pos' then IntPairSet.add pos visited_cells
    else if matrix pos' = '#' then move (right_of direction) pos visited_cells
    else move direction pos' (IntPairSet.add pos visited_cells)
  in
  move Up guard_starting_pos IntPairSet.empty |> IntPairSet.cardinal

let () = Printf.printf "Part 1: %d\n" answer1

module IntPairAndDir = struct
  type t = int * int * direction

  let compare = compare
end

module IntPairAndDirSet = Set.Make (IntPairAndDir)

let rec check_guard_loops direction ((i, j) as pos) visited_states =
  let curr_state = (i, j, direction) in
  match IntPairAndDirSet.find_opt curr_state visited_states with
  | Some _ -> true
  | None ->
      let pos' = new_pos pos direction in
      let updated_visited_states =
        IntPairAndDirSet.add curr_state visited_states
      in
      if out_of_bounds pos' then false
      else if matrix pos' = '#' then
        check_guard_loops (right_of direction) pos updated_visited_states
      else check_guard_loops direction pos' updated_visited_states

let answer2 =
  let rec obstructions_that_loops direction ((i, j) as pos) visited_states count
      =
    let curr_state = (i, j, direction) in
    let ((i', j') as pos') = new_pos pos direction in
    let updated_visited_states =
      IntPairAndDirSet.add curr_state visited_states
    in
    if out_of_bounds pos' then count
    else
      match matrix pos' with
      | '.' -> (
          match
            List.filter_map
              (fun dir ->
                IntPairAndDirSet.find_opt (i', j', dir) visited_states)
              [ Left; Up; Right; Down ]
          with
          | [] ->
              let () = put_matrix pos' '#' in
              let is_guard_looping =
                check_guard_loops direction pos visited_states
              in
              let () = put_matrix pos' '.' in
              obstructions_that_loops direction pos' updated_visited_states
                (if is_guard_looping then 1 + count else count)
          | _ ->
              obstructions_that_loops direction pos' updated_visited_states
                count)
      | '#' ->
          obstructions_that_loops (right_of direction) pos
            updated_visited_states count
      | _ -> obstructions_that_loops direction pos' updated_visited_states count
  in
  obstructions_that_loops Up guard_starting_pos IntPairAndDirSet.empty 0

let () = Printf.printf "Part 2: %d\n" answer2
