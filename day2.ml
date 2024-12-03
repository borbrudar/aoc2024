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
  string_of_int (String.length vsebina_datoteke)

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