let remove_at pos list =
  let rec aux pos acc = function
    | [] -> List.rev acc
    | h :: t ->
      if pos = 0 then
        aux (pos-1) acc t
      else
        aux (pos-1) (h :: acc) t
  in
  aux pos [] list

let print_list f l = List.iter (fun x -> f x) l

let run () =
  let l = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] in
  remove_at 5 l |> print_list (fun (item) -> Printf.printf "%d " item);
  print_newline ();
