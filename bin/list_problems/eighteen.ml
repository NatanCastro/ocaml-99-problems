let slice l start stop =
  let rec aux l start stop acc =
    match l with
    | [] -> acc
    | h :: t ->
        if start = 0 && stop > 0 then aux t 0 (stop - 1) (h :: acc)
        else if stop = 0 then h :: acc
        else aux t (start - 1) (stop - 1) acc
  in
  let list_length = List.length l in
  let valid_range =
    start <= stop || start <= list_length || stop <= list_length
  in
  match valid_range with
  | true -> Some (List.rev (aux l start stop []))
  | false -> None

let print_list l f = List.iter (fun x -> f x) l

let run () =
  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let start = 3 in
  let stop = 6 in
  match slice l start stop with
  | Some l ->
      print_list l (fun x -> Printf.printf "%d " x);
      print_string "\n"
  | None -> print_string "None"
