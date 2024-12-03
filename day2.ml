let naloga1 vsebina_datoteke =
  let reports = String.split_on_char '\n' vsebina_datoteke in
  let rec aux cnt reports = 
    match reports with
    | [] -> cnt
    | x :: xs -> 
      let parts = String.split_on_char ' ' x in
      let rec aux2 parts = 
        match parts with 
        | [] -> true
        | f :: [] -> true
        | f :: s :: t :: xs -> 
          let ff = int_of_string f in
          let ss = int_of_string s in
          let tt = int_of_string t in
          let dif1 = abs (ff - ss) in
          let dif2 = abs (ss - tt) in
          if not ((ff < ss && ss < tt) || (ff > ss && ss > tt)) then false
          else if dif1 > 3 || dif1 < 1 || dif2 > 3 || dif2 < 1 then false
          else aux2 (s :: t :: xs)
          | f :: s :: [] -> let a = abs ((int_of_string f) - (int_of_string s)) in a <= 3 && a >= 1 
        in 
        if aux2 parts then aux (cnt+1) xs
        else aux cnt xs
      in string_of_int (aux 0 reports)


let naloga2 vsebina_datoteke =
  let reports = String.split_on_char '\n' vsebina_datoteke in
  let remove_one lst =
    let rec remove_at idx lst =
      match lst with
      | [] -> []  (* Shouldn't happen if the index is valid *)
      | x :: xs -> if idx = 0 then xs else x :: remove_at (idx - 1) xs
    in
  
    (* Generate the list with the original list and lists with one element removed *)
    let rec aux acc idx lst og =
      match lst with
      | [] -> acc  (* Return the accumulator when the list is exhausted *)
      | _ :: xs -> 
          (* Add the list with the element removed at index idx *)
          aux ((remove_at idx og) :: acc) (idx + 1) xs og
    in
  
    (* Include the original list and process each index *)
    aux [lst] 0 lst lst
    in
    let print_bool_list lst =
      List.iter (fun b -> print_endline (string_of_bool b)) lst in
      let print_string_list lst =
        List.iter print_endline lst in
  let rec aux cnt reports = 
    match reports with
    | [] -> cnt
    | x :: xs -> 
      let parts = String.split_on_char ' ' x in
      let rec aux2 parts = 
        match parts with 
        | [] -> true
        | f :: [] -> true
        | f :: s :: t :: xs -> 
          let ff = int_of_string f in
          let ss = int_of_string s in
          let tt = int_of_string t in
          let dif1 = abs (ff - ss) in
          let dif2 = abs (ss - tt) in
          if not ((ff < ss && ss < tt) || (ff > ss && ss > tt)) then false
          else if dif1 > 3 || dif1 < 1 || dif2 > 3 || dif2 < 1 then false
          else aux2 (s :: t :: xs)
          | f :: s :: [] -> let a = abs ((int_of_string f) - (int_of_string s)) in a <= 3 && a >= 1 
        in 
        let all_lists = remove_one parts in
        let ok = List.fold_left (fun acc x -> acc || x) false (List.map (fun x -> aux2 x) all_lists) in
        (*print_bool_list (List.map (fun x -> aux2 x) all_lists);
        print_string_list (List.nth all_lists 0);
        print_endline "----------------";*)
        if ok then aux (cnt+1) xs
        else aux cnt xs
      in string_of_int (aux 0 reports)

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "day_2.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_2_1.out" odgovor1;
  izpisi_datoteko "day_2_2.out" odgovor2