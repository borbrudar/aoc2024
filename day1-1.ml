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
  let l1,l2 = read_file "day_1.in" in
  let s1 = List.sort compare l1 in 
  let s2 = List.sort compare l2 in
  let ans = List.fold_left2 (fun acc x y -> acc + abs (x - y) ) 0 s1 s2 in
  print_int ans;;  (* Print all lines *)