let rotate l n =
  let rec aux l n acc =
    match l with
    | [] -> acc
    | h :: t ->
        if n = 1 then t @ List.rev (h :: acc) else aux t (n - 1) (h :: acc)
  in
  let count = if n > 0 then n else n + List.length l in
  aux l count []

let print_list f l = List.iter (fun x -> f x) l

let run () =
  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let n = 3 in
  rotate l n |> print_list (fun x -> Printf.printf "%d " x);
  print_string "\n";

  let l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let n = -3 in
  rotate l n |> print_list (fun x -> Printf.printf "%d " x);
  print_string "\n"
