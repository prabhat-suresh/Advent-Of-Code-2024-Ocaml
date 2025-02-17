let input = open_in "input.txt"
let char_to_int c = Char.code c - Char.code '0'

let disk_map1, disk_map2, length =
  let line = input_line input in
  ( Array.init (String.length line) (fun i -> line.[i] |> char_to_int),
    Array.init (String.length line) (fun i -> line.[i] |> char_to_int),
    String.length line )

let num_file_blocks =
  let num = ref 0 in
  Array.iteri (fun i x -> if i mod 2 = 0 then num := !num + x) disk_map1;
  !num

let id index = index / 2

let check_sum1 =
  let rec helper sum i j curr_index =
    if curr_index >= num_file_blocks then sum
    else if i mod 2 = 0 then (
      match disk_map1.(i) with
      | 0 -> helper sum (i + 1) j curr_index
      | n ->
          disk_map1.(i) <- n - 1;
          let sum' = sum + (curr_index * id i) in
          helper sum' i j (curr_index + 1))
    else
      match (disk_map1.(i), disk_map1.(j)) with
      | 0, _ -> helper sum (i + 1) j curr_index
      | _, 0 -> helper sum i (j - 2) curr_index
      | n, m ->
          disk_map1.(i) <- n - 1;
          disk_map1.(j) <- m - 1;
          let sum' = sum + (curr_index * id j) in
          helper sum' i j (curr_index + 1)
  in
  helper 0 0 (length - 1) 0

let check_sum2 =
  let slices = Array.make length [] in
  let find_slice size before =
    Array.find_mapi
      (fun i num_blocks ->
        if i mod 2 = 1 && i < before && num_blocks >= size then Some i else None)
      disk_map2
  in
  for j = length - 1 downto 0 do
    if j mod 2 = 0 then
      let size = disk_map2.(j) in
      match find_slice size j with
      | None ->
          disk_map2.(j) <- 0;
          slices.(j) <- (id j, size) :: slices.(j)
      | Some i ->
          disk_map2.(i) <- disk_map2.(i) - size;
          slices.(i) <- (id j, size) :: slices.(i)
  done;
  let sum = ref 0 in
  let index = ref 0 in
  for i = 0 to length - 1 do
    List.rev slices.(i)
    |> List.iter (fun (id, size) ->
           for j = 0 to size - 1 do
             sum := !sum + (id * (!index + j))
           done;
           index := !index + size);
    index := !index + disk_map2.(i)
  done;
  !sum

let () = Printf.printf "Part 1: %d\nPart 2: %d\n" check_sum1 check_sum2
