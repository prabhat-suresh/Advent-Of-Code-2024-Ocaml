(* take input from a file input.txt *)

let input = Scanf.Scanning.from_file "input.txt"

(* read the first line of input *)
let hash_table = Hashtbl.create 1000

let rec input_helper ls =
  try
    let x, y = Scanf.bscanf input "%d %d\n" (fun a b -> (a, b)) in
    let _ =
      match Hashtbl.find_opt hash_table y with
      | Some count -> Hashtbl.replace hash_table y (count + 1)
      | None -> Hashtbl.add hash_table y 1
    in
    input_helper (x :: ls)
  with End_of_file -> ls

let input_list = input_helper []

let score =
  List.fold_left
    (fun acc p ->
      let freq =
        match Hashtbl.find_opt hash_table p with
        | Some count -> count
        | None -> 0
      in

      acc + (p * freq))
    0 input_list

let () = Printf.printf "%d\n" score
