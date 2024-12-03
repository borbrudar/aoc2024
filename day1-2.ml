let read_file filename =
  let ic = open_in filename in
  let rec read_lines (acc : int list) acc2 =
    try
      let line = input_line ic in
      let splitted = String.split_on_char ' ' line in
      (*print_endline (List.nth splitted 3);*)
      read_lines ((int_of_string (List.nth splitted 0)) :: acc) ((int_of_string (List.nth splitted 3)) :: acc2)   
    with
    | End_of_file -> close_in ic; (List.rev acc , List.rev acc2)
    | e -> close_in ic; raise e 
  in
  read_lines [] []

let () =
  let l1,l2 = read_file "input1.txt" in
  let s1 = List.sort compare l1 in 
  let s2 = List.sort compare l2 in
  let count_occurrences lst target = List.fold_left (fun count x -> if x = target then count + 1 else count) 0 lst
  in
  let ans = List.fold_left (fun acc x -> acc + x * (count_occurrences s1 x)) 0 s2 in
  print_int ans;;  (* Print all lines *)